"""
COBOL DDE Parser and JSONSchema Builder.

COBOL
=====

A COBOL "Copybook" is a group-level DDE.
See https://www.ibm.com/docs/en/cobol-zos/4.2?topic=division-data-data-description-entry

There are three formats for DDE's. We only really care about one of them.

- Format 1 is the useful DDE level numbers 01 to 49 and 77.

- Format 2 is a RENAMES clause, level 66. We don't support this.

. Format 3 is a CONDITION, level 88. This is a kind of enumeration of values; we tolerate it, but don't do anything with it.

Here's the railroad diagramm for each sentence. Clauses after level-number and data-name-1 can appear in any order.

::

    >>-level-number--+-------------+--+------------------+---------->
                     +-data-name-1-+  '-redefines-clause-'
                     '-FILLER------'

    >--+------------------------+--+-----------------+-------------->
       '-blank-when-zero-clause-'  '-external-clause-'

    >--+---------------+--+--------------------+-------------------->
       '-global-clause-'  '-group-usage-clause-'

    >--+------------------+--+---------------+---------------------->
       '-justified-clause-'  '-occurs-clause-'

    >--+----------------+--+-------------+-------------------------->
       '-picture-clause-'  '-sign-clause-'

    >--+---------------------+--+--------------+-------------------->
       '-synchronized-clause-'  '-usage-clause-'

    >--+--------------+--+--------------------+--------------------><
       '-value-clause-'  '-date-format-clause-'

A separator period occurs at the end of the sentence. (It's described elsewhere in the COBOL language reference.)

For simple examples, see https://github.com/rradclif/mortgagesample/tree/master/MortgageApplication/copybook

For comprehensive, complex examples, see
https://github.com/royopa/cb2xml/tree/ec83af657b781afd0dad9cc263623faa2549f738/source/cb2xml_tests/src/common/cobolCopybook

These cover a large number of COBOL-to-XML cases.

Approach
========

We can use a (long) regular expression to parse the various clauses.
This defines the entire DDE syntax.

Beyond the essential language syntax, there's a "reference format" for source code.
For this format, we need to remove positions 1-6 and 72-80. Position 7 may involve a comment indicator, "*", or a continuation character, "-".
See https://www.ibm.com/docs/en/cobol-zos/4.2?topic=structure-reference-format.

Goals
=====

This module's purpose is to translate COBOL to JSON Schema. This involves the following kinds of transformations:

- Group level items become "type": "object".

- Elementary items become one of the atomic types, "string", "integer", "number". If an extened vocabulary is used, then "decimal" can be used, also.

- Occurs items become ``"type": "array"``. There are additional special cases.

    -  An item with both OCCURS and a PICTURE becomes an anonymous array that contains the elementary item.

    - ``OCCURS DEPENDING ON`` builds a ``"$ref": "#name"`` to a ``"$anchor": "name"`` item in the schema.

- ``REDEFINES`` refers to another item under this parent. While this is similar to a "oneOf" definition, it's a bit more complex because the alternatives each have separate names. The structure is not simply a ``{"name": {"type": {"oneOf": [base-A, redefine-B, redefine-C, etc.]}}``.
The "redefines" property is effectively anonymous and each of the subtypes has a distinct name. It's ``{"redefine-A-B-C": {"type": {"oneOf": [{"type": "object", "properties": {"A": base}}, {"type: "object", "properties": {"B": redefined}}, etc.]}}}``.
This is cumbersome, but is required to capture the COBOL semantics accurately in JSONSchema.

We require some extensions or adaptations to cover two COBOL issues:

- COBOL encoded data (Packed-Decimal, Binary, etc.) JSON Schema presumes delimited files with a parser's conversions of data. For COBOL, the parsing is driven from the JSON Schema, therefore additional details are required. This includes the ``contentEncoding`` and a conversion function.

- Occurs Depending On reference. JSON Schema limits the maxItems to an unsigned integer. We have to provide an alternative keyword for this.

It is also handy to have a "cobol" keyword with the original source text.

Because COBOL flattens the namespace of records, we define an ``$anchor`` for each individual field to make them easier to search for.

"""

import re
from typing import (
    Iterator,
    Iterable,
    TextIO,
    Optional,
    Any,
    Callable,
    Union,
    NamedTuple,
    Sequence,
    cast,
)
import weakref

class DesignError(BaseException):
    pass

JSON = Union[None, bool, int, float, str, list[Any], dict[str, Any]]


DEBUG_REF_FORMAT = False  # TODO: Use a separate logger for this.


def reference_format(source: TextIO, replacing: Optional[list[tuple[str, str]]] = None) -> Iterator[str]:
    """
    Extract source from files that have sequence numbers in 1-6, indicator in 7, and code in 8-72.
    Zero-based, these slices are [0:6], [6], [7:72]
    
    This can be extended to handle COPY statements that include other copybooks into a copybook.

    The ``replacing`` parameter is a list of ("'old'", "new") strings to replace in the source
    The apostrophes on the old are required here.
    """
    non_empty = filter(lambda line: line.rstrip(), source)
    non_directive = filter(
        lambda line: line.strip() not in {"EJECT", "SKIP1", "SKIP2", "SKIP3"},
        non_empty
    )
    indicator_line_iter = (
        (line[6], line[7:72]) for line in non_directive if len(line) >= 7
    )
    non_comment = filter(
        lambda indic_ln: indic_ln[0] not in {"*", "D"}, indicator_line_iter
    )
    if replacing:
        replaced = (
            (indic, line.replace(old, new))
            for indic, line in non_comment
            for old, new in replacing
        )
        non_comment_iter = iter(replaced)
    else:
        non_comment_iter = iter(non_comment)
    indicator, line = next(non_comment_iter)
    for indicator, next_line in non_comment_iter:
        if indicator == "-":
            line += next_line
        else:
            if line.strip().startswith("COPY"):
                raise ValueError(f"directive not supported: {line!r}")
            if DEBUG_REF_FORMAT:
                print(f"{line!r}")  # pragma: no cover
            yield line
            line = next_line
    if DEBUG_REF_FORMAT:
        print(f"{line!r}")  # pragma: no cover
    yield line


# The overall sentence pattern has an optional "data-name-1" as a separate clause.
# It can be the literal "FILLER". Or. It can be omitted.
# The only rule, then, is NOT one of the reserved words.
# Therefore, the last pattern is the data-name-1 -- a non-reserved-word field name.
#
# Unnamed "filler" fields and fields named "FILLER" need to be properly handled to compute offsets of named fields.


def dde_sentences(source: Iterable[str]) -> Iterator[Sequence[str]]:
    """Decompose source into separate sentences by looking for the trailing period-space.
    The pattern will produce a sequence of (level, source text) 2-tuples.
    Since we simply collect all the matching groups, it's technically a Sequence[str].
    """
    dde_sentence_pattern = re.compile(
        "\s*(?P<level>\d\d)\s*(?P<clauses>.*?)\.\s", re.M | re.S
    )

    text = "".join(source)
    for s in dde_sentence_pattern.finditer(text):
        yield s.groups()


# TODO: is COBOL case independent?

SPACE = r"[\s|,|;]+"
NAME = r"[\w-]+"
KEY = fr"((?:ASCENDING|DESCENDING){SPACE}(?:KEY{SPACE})?(?:IS{SPACE})?{NAME})*(?:{SPACE}(?:INDEXED){SPACE}(?:BY{SPACE})?{NAME}(?:{SPACE}{NAME})*)"

CLAUSES = (
    fr"{SPACE}"
    fr"|(?:REDEFINES){SPACE}(?P<redefines>{NAME})"
    fr"|(?:BLANK){SPACE}(WHEN{SPACE})?(?P<blank>ZERO|ZEROES|ZEROS)"
    r"|EXTERNAL"
    r"|GLOBAL"
    fr"|(?:JUSTIFIED|JUST){SPACE}(?P<justified>RIGHT)?"
    fr"|(?:OCCURS){SPACE}(?:(?P<odo_minitems>\d+){SPACE}TO{SPACE})?(?P<odo_maxitems>\d+)(?:{SPACE}TIMES)?{SPACE}DEPENDING{SPACE}(?:ON{SPACE})?(?P<depending_on>{NAME})(?:{SPACE}{KEY})?"
    fr"|(?:OCCURS){SPACE}(?P<occurs_maxitems>\d+)(?:{SPACE}TIMES)?(?:{SPACE}{KEY})?"
    fr"|(?:PIC|PICTURE){SPACE}(?:IS{SPACE})?(?P<picture>\S+)"
    fr"|(?:SIGN{SPACE})?(?:IS{SPACE})?(?P<sign>LEADING|TRAILING)(?P<sign_sep>{SPACE}SEPARATE{SPACE}CHARACTER|{SPACE}SEPARATE)"
    fr"|(?:SYNCHRONIZED|SYNC)(?P<synch>{SPACE}LEFT|{SPACE}RIGHT)?"
    fr"|(?:USAGE{SPACE})?(?:IS{SPACE})?(?P<usage>BINARY|COMPUTATIONAL-1|COMPUTATIONAL-2|COMPUTATIONAL-3|COMPUTATIONAL-4|COMPUTATIONAL|COMP-1|COMP-2|COMP-3|COMP-4|COMP|DISPLAY|PACKED-DECIMAL)(?!-)"
    fr"|(?:VALUE{SPACE})(?:IS{SPACE})?(?P<value>'.*'|\".*\"|\S+)"
    r"|(?P<filler>FILLER)"
    fr"|(?P<name>{NAME})"
)

clause_pattern = re.compile(CLAUSES)


def expand_repeat(group_dict: dict[str, str]) -> dict[str, str]:
    """
    Replace {"repeat": "x(y)"} with {"digit": "xxx...x"}

    >>> expand_repeat({'repeat': '9(5)'})
    {'digit': '99999'}
    >>> expand_repeat({'repeat': '9(0005)'})
    {'digit': '99999'}
    """
    if "repeat" in group_dict:
        # Mypy doesn't like the assignment below;
        # it's not clear the value of pic.groupdict()["repeat"] has 4 or more substrings.
        count: str
        char: str
        char, left, *count, right = group_dict["repeat"]  # type: ignore [misc]
        return {"digit": int("".join(count)) * char}
    return group_dict


def pass_non_empty(group_dict: dict[str, str]) -> dict[str, str]:
    """
    Pass dictionary items with non-empty values; reject items with empty values.

    >>> pass_non_empty({"a": "b", "empty": None})
    {'a': 'b'}
    """
    return {n: v for n, v in group_dict.items() if v}


def normalize_picture(source: str) -> list[dict[str, str]]:
    """
    Parse PICTURE clause into component pieces to make it easier to work with.
    This breaks down a complex mask into individual pieces.

    >>> normalize_picture("9(5)")
    [{'digit': '99999'}]
    >>> normalize_picture("S9(5)V99")
    [{'sign': 'S'}, {'digit': '99999'}, {'decimal': 'V'}, {'digit': '99'}]
    >>> normalize_picture("S9(0005)V9(0002)")
    [{'sign': 'S'}, {'digit': '99999'}, {'decimal': 'V'}, {'digit': '99'}]

    """
    pic_pattern = re.compile(
        r"(?P<sign>\+|-|S|DB|CR)"
        r"|(?P<char>\$|,|/|\*|B)"
        r"|(?P<decimal>V|\.)"
        r"|(?P<repeat>[AX9Z0]\(\d+\))"
        r"|(?P<digit>[AX9Z0]+)"
    )

    matches = list(pic_pattern.finditer(source))
    if matches[-1].end() != len(source):
        raise ValueError(
            f"invalid characters {source[matches[-1].end():]!r} in PIC {source!r}"
        )

    non_empty = map(pass_non_empty, (m.groupdict() for m in matches))
    normalized = map(expand_repeat, non_empty)
    return list(normalized)


# Most clauses are simple strings.
# The parsed picture clause, however, is a list of dicts.

CLAUSE = Union[str, list[dict[str, str]]]


def clause_dict(source: str) -> dict[str, CLAUSE]:
    """
    Expand a COBOL DDE sentence into a dict of clauses and values.
    This tends to preserve much (but not all) of the source syntax.
    
    1. Non-space separators (, or ;) are dropped.
    
    2. Some productions don't have all the values captured.
       The ASCENDING/DESCENDING KEY options in OCCURS, for example, are stripped away.
    """
    clauses = (
        {n: v for n, v in c.groupdict().items() if v}
        for c in clause_pattern.finditer(source)
    )
    non_empty_clauses: dict[str, CLAUSE] = {n: v for c in clauses for n, v in c.items()}
    if "picture" in non_empty_clauses:
        non_empty_clauses["_picture_parsed"] = normalize_picture(
            cast(str, non_empty_clauses["picture"])
        )
    return non_empty_clauses


class DDE:
    """
    An instance of a COBOL DDE.
    
    For name == "FILLER", this assigns a unique internal name.
    A class-level counter is used.

    Note that level of "01" resets the counter.
    """

    filler_count = 0

    def __init__(
        self, *sentence: str, clauses: Optional[dict[str, Any]] = None
    ) -> None:
        self.level, self.source = sentence
        self.clauses = clauses or clause_dict(self.source)
        self.name = self.clauses.get("name") or self.clauses.get("filler") or "FILLER"
        if self.level == "01":
            DDE.filler_count = 0
        if self.name == "FILLER":
            DDE.filler_count += 1
            self.unique_name = f"FILLER-{DDE.filler_count}"
        else:
            self.unique_name = str(self.name)
        self.children: list["DDE"] = []
        self.parent: Optional[weakref.ReferenceType["DDE"]] = None
        self.compact_source = " ".join(self.source.split())

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.level!r}, {self.source!r}, clauses={self.clauses!r})"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, DDE):
            return (
                    self.level == other.level
                    and self.source == other.source
                    and self.clauses == other.clauses
            )
        return NotImplemented  # pragma: no cover

    def append(self, child: "DDE") -> None:
        child.parent = weakref.ref(self)
        self.children.append(child)

    @staticmethod
    def display(node: "DDE", indent: int = 0) -> None:
        print(
            f"{indent*'    '}{node}: "
            f"USAGE {node.clauses.get('usage', 'DISPLAY')} "
            f"PIC {node.clauses.get('_picture_parsed', '(Group)')} "
            f"{node.clauses.get('redefines')}"
        )
        for c in node.children:
            DDE.display(c, indent + 1)


def structure(sentences: Iterable[Sequence[str]]) -> list[DDE]:
    """
    Create a list of DDE trees from a sequence of lines.
    Each DDE contains zero or more children.
    
    We update the start of a REDEFINES union.
    We don't know X will be redefined until we encounter "Y REDEFINES X".

    The sentence regular expression produces two-tuples. Since we use
    the simple groups() function, however, it's technically a Sequence[str].
    """
    node_iter = iter(DDE(*s) for s in sentences)
    bottom: Optional[DDE] = next(node_iter)
    trees = [cast(DDE, bottom)]
    for node in node_iter:
        if node.level in {"66", "77", "88"}:
            continue
        while bottom and node.level <= bottom.level:
            bottom = bottom.parent() if bottom.parent else None  # Deference parent
        if bottom is None:
            # Multiple 01-level definitions in Copybook.
            trees.append(node)
            bottom = node
        else:
            if "redefines" in node.clauses:
                # Find first instance of the referenced name. It should be in bottom.children.
                (matches,) = [
                    c for c in bottom.children if c.name == node.clauses["redefines"]
                ]
                matches.clauses["redefines"] = node.clauses["redefines"]
            bottom.append(node)
            bottom = node
    return trees


class JSONSchemaMaker:
    """
    Translate COBOL DDE to JSONSchema.
    
    This handles REDEFINES and OCCURS DEPENDING ON.
    
    - REDEFINES becomes a ``oneOf`` with the alternatives.
    
    - OCCURS DEPENDING On uses a ``maxItemsDependsOn`` vocabulary extension.

    COBOL "flattens" the namespace so an elementary name implies the path 
    to that name. This is done with the "$anchor" keyword to mark the visible names.
    """

    def __init__(self, source: DDE) -> None:
        self.source = source
        self.names: dict[str, JSON] = {}

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.source}): {self.names=!r}"

    def jsonschema(self) -> JSON:
        return self.build_json_schema(self.source)

    def json_type(self, node: DDE) -> JSON:
        """
        The JSON Schema type for an elementary (or atomic) field.
        
        If we're using a standad JSON Schema validator (without ``decimal`` as part of the vocabulary),
        the following mappings are used:

        COBOL encoded numeric can be ``"type": "string"`` with additional ``"contentEncoding"`` details. 

        The ``"contentEncoding"`` describes COBOL Packed Decimal and Binary as strings of bytes. 

        A ``"cobol"`` keyword gets Usage and Picture values required to decode EBCDIC. 

        A ``"conversion"`` keyword converts to a more useful Python type from raw file strings.

        Here's an example::

            {"title": "SOME-FIELD",
             "$anchor": "SOME-FIELD",
             "cobol": "05 SOME-FIELD USAGE COMP-3 PIC S999V99",

             "type": "string", 
             "contentEncoding": "packed-decimal",
             "conversion": "decimal"
            }
            
        The title, anchor, and cobol are defined separately. This function provides the 
        type, encoding, and conversion.

        Other ``"contentEncoding"`` values include "bigendian-int".  Also, "bigendian-float" and "bigendian-double". And, of course, "CP037" or "EBCDIC" to decode ordinary strings from EBCDIC to native text.

        If USAGE DISPLAY and PIC has only SVP9: zoned decimal: "type": "string", "contentEncoding": "cp037", "conversion": "decimal",
        If USAGE DISPLAY: "string", "contentEncoding": "cp037"
        If USAGE COMP-3, COMPUTATIONAL-3, PACKED-DECIMAL: "type": "string", "contentEncoding": "packed-decimal", "conversion": "decimal"
        If USAGE COMP-4, COMPUTATIONAL-4, COMP, COMPUTATIONAL, BINARY: "integer", "contentEncoding": "bigendian-int"
        If USAGE COMP-1, COMPUTATIONAL-1, COMP-2, COMPUTATIONAL-2: "number", "contentEncoding": "bigendian-float" or "bigendian-double"

        TODO: Compute maxLength and minLength from the ``"contentEncoding"`` and ``"cobol"`` fields.
            Use :py:mod:`estruct` or :py:mod:`struct` modules' :py:mod:`calcsize`.
        """
        usage = node.clauses.get("usage", "DISPLAY")
        if usage == "DISPLAY":
            picture = node.clauses.get("picture")
            if picture and all(c in {"S", "V", "P", "9"} for c in picture):
                return {
                    "type": "string",
                    "contentEncoding": "cp037",
                    "conversion": "decimal",
                }
            return {"type": "string", "contentEncoding": "cp037"}
        elif usage in {
            "COMP-3",
            "COMPUTATIONAL-3",
            "PACKED-DECIMAL",
        }:
            return {
                "type": "string",
                "contentEncoding": "packed-decimal",
                "conversion": "decimal",
            }
        elif usage in {
            "COMP-4",
            "COMPUTATIONAL-4",
            "COMP",
            "COMPUTATIONAL",
            "BINARY",
        }:
            return {"type": "integer", "contentEncoding": "bigendian-int"}
        elif usage in {
            "COMP-1",
            "COMPUTATIONAL-1",
        }:
            return {"type": "number", "contentEncoding": "bigendian-float"}
        elif usage in {"COMP-2", "COMPUTATIONAL-2"}:
            return {"type": "number", "contentEncoding": "bigendian-double"}
        else:
            # The regular expression pattern is not reflected in this if-statement.
            raise DesignError(f"usage clause {usage!r} unknown")  # pragma: no cover

    def build_json_schema(
        self, node: DDE, path: tuple[str, ...] = (), ignore_redefines: bool = False
    ) -> JSON:
        """
        Emit a JSON schema that reflects a COBOL DDE and all nested DDE's within it.
        """
        json_schema: JSON
        cobol = f"{node.level} {node.compact_source}"

        # Refactoring: the following condition may be a wrapper around another function.
        # Because it can be bypassed, the ignore_redefines condition suggests a secondary function
        # without the redefines condition in it.

        if not ignore_redefines and "redefines" in node.clauses and node.parent:
            # REDEFINES alternatives must be accumulated into a single OneOf for all of them.
            # The OneOf is part of the *parent* of this DDE Node, which we must update.
            # (A REDEFINES on an 01-level is syntactic noise, and we ignore this.)
            parent_schema = cast(
                dict[str, Any], self.names[cast(DDE, node.parent()).unique_name]
            )
            base_def_name = f"REDEFINES-{node.clauses['redefines']}"

            # Side-Effect: Add an empty oneOf property to the parent.
            if base_def_name not in parent_schema["properties"]:
                redef_schema = {"oneOf": [], "$anchor": base_def_name}
                parent_schema["properties"][base_def_name] = redef_schema

            # Create this particular alternative redefinition.
            redef = self.build_json_schema(
                node, path[:-1] + (base_def_name,), ignore_redefines=True
            )

            # Append to the OneOf list.
            parent_schema["properties"][base_def_name]["oneOf"].append(redef)

            # A placeholder to provide the original COBOL structure's property name.
            return {"title": node.name, "cobol": cobol, "$ref": f"#{node.unique_name}"}

        elif "occurs_maxitems" in node.clauses or "odo_maxitems" in node.clauses:
            json_schema = {
                "title": node.name,
                "$anchor": node.unique_name,
                "cobol": cobol,
                "type": "array",
                "items": {},
            }

            # Two choices for maxItems.
            # - simple occurs_maxitems is an array size.
            # - occurs depending on is a reference to another field somewhere.
            if node.clauses.get("depending_on"):
                # Creates an absolute path to the named field.
                dep_name = node.clauses.get("depending_on")
                json_schema["maxItemsDependsOn"] = {"$ref": f"#{dep_name}"}
            else:
                json_schema["maxItems"] = int(
                    cast(str, node.clauses.get("occurs_maxitems"))
                )

            self.names[node.unique_name] = json_schema

            # Two choices for subsidiary ``items`` type.
            # - Elementary Level. PIC Clause is here. The child schema is an object with a single property.
            # - Group Level. No PIC Clause. The child schema is built through ordinary recursion.

            if "picture" in node.clauses:
                # In principle, PIC and USAGE are stripped from the parent and pushed into the child.
                base_schema = {"cobol": cobol}
                child_schema = {
                    "type": "object",
                    "properties": {
                        node.unique_name: base_schema
                        | cast(dict[str, str], self.json_type(node)),
                    },
                }
            else:
                child_schema = {
                    "type": "object",
                    "properties": {
                        c.unique_name: self.build_json_schema(
                            c, path + (c.unique_name,)
                        )
                        for c in node.children
                    },
                }
            json_schema["items"] = child_schema

        elif node.children:
            # Group. Children may have REDEFINES, so ``OneOf`` instances may be added.
            json_schema = {
                "title": node.name,
                "$anchor": node.unique_name,
                "cobol": cobol,
                "type": "object",
                "properties": {},
            }
            self.names[node.unique_name] = json_schema

            properties_schema = {
                c.unique_name: self.build_json_schema(c, path + (node.name,))
                for c in node.children
            }
            # NOTE! Side effect of ``self.build_json_schema()`` is to add properties.
            # Our new properties must preserve those via an update.
            json_schema["properties"].update(properties_schema)

        else:
            # Elementary
            json_schema = {
                "title": node.name,
                "$anchor": node.unique_name,
                "cobol": cobol,
            }
            json_schema |= cast(dict[str, Any], self.json_type(node))

        self.names[node.unique_name] = json_schema
        return json_schema


class JSONSchemaMakerExtendedVocabulary(JSONSchemaMaker):
    VOCABULARY: JSON = {
        # TODO: Provide the JSONSchema Vocabulary definition to add 'decimal' as a type.
    }
    def json_type(self, node: DDE) -> JSON:
        """
        If we're using an extended vocabulary including  ``decimal``, the following mappings can be used.

        If USAGE DISPLAY and PIC has only SVP9: zoned "decimal".
        If USAGE DISPLAY: "string".
        If USAGE COMP-3, COMPUTATIONAL-3, PACKED-DECIMAL: "decimal".
        If USAGE COMP-4, COMPUTATIONAL-4, COMP, COMPUTATIONAL, BINARY: "integer".
        If USAGE COMP-1, COMPUTATIONAL-1, COMP-2, COMPUTATIONAL-2: "number".

        Example::

            {
                "type": json_type(node),
                "cobol": f"{node.level} {node.name} {node.source}",
            }
        """
        usage = node.clauses.get("usage", "DISPLAY")
        if usage == "DISPLAY":
            picture = node.clauses.get("picture")
            if picture and all(c in {"S", "V", "P", "9"} for c in picture):
                return {"type": "decimal"}
            return {
                "type": "string",
            }
        elif usage in {
            "COMP-3",
            "COMPUTATIONAL-3",
            "PACKED-DECIMAL",
        }:
            return {"type": "decimal"}
        elif usage in {
            "COMP-4",
            "COMPUTATIONAL-4",
            "COMP",
            "COMPUTATIONAL",
            "BINARY",
        }:
            return {"type": "integer"}
        elif usage in {
            "COMP-1",
            "COMPUTATIONAL-1",
            "COMP-2", "COMPUTATIONAL-2",
        }:
            return {"type": "number"}
        else:
            # The regular expression pattern is not reflected in this if-statement.
            raise DesignError(f"usage clause {usage!r} unknown")  # pragma: no cover

REPLACING = Optional[list[tuple[str, str]]]
REFERENCE_FORMAT = Callable[[TextIO, REPLACING], Iterator[str]]

def schema_iter(source: TextIO, deformat: REFERENCE_FORMAT = reference_format) -> Iterator[JSON]:
    copy_books = structure(
        dde_sentences(
            reference_format(
                source
            )
        )
    )
    schemas = (JSONSchemaMaker(record).jsonschema() for record in copy_books)
    return schemas

