r"""
#############################
Schema and Navigation models.
#############################

A number of file formats require a schema to unpack (or decode) the data. Even a simple CSV file offers headings in
the first row as a kind of schema. Each row can be interpreted as a dictionary where the headers are keys.

The JSON Schema permits definition of a schema that can be used to navigate spreadsheet files, like CSV. It can be
used to unpack more sophisticated generic structures in JSON, YAML, and TOML format, as well as XML.

It can also be used to unpack COBOL files, in Unicode or ASCII text as well as EBCDIC. Most of the features of a
COBOL DDE definition parallel JSON Schema constructs.

- COBOL has Atomic fields of type text (with vaious format details), and a variety of "Computational" variants. The
  most important is `COMP-3`, which is a decimal representation with digits packed two per byte. THe JSON Schema
  presumes types "null", "boolean", "number", or "string" types have text representations that fit well with COBOL.

- The hierarchy of COBOL DDE's is the JSON Schema "object" type.

- The COBOL `OCCURS` clause is the JSON Schema "array" type. The simple case, with a single, literal `TIMES` option
  is expressed with `maxItems` and `minItems`.

While COBOL is more sophisticated than CSV, it's generally comprarable to JSON/YAML/TOML/XML. There are some unique
specializations related to COBOL processing.

COBOL Processing
================

The parallels between COBOL and JSON Schema suggest that COBOL Data Definition Entries (DDE's) can be translated to
JSON Schema constructs. The JSON Schema (with extensions) can be used to decode bytes from COBOL representation to
create native Python objects. There are three areas: handling `REDEFINES`,  `OCCURS DEPENDING ON` structures,
and EBCDIC unpacking.

Redefines
---------

A COBOL `REDEFINES` clause defines a free union of types for a given sequence of bytes. Within the application code,
there are generally fields that imply a more useful tagged union. The tags used for discrimination is not part of the
COBOL definition.

To make this work, each field within the JSON schema has an implied starting offset and length.
A COBOL `REDEFINES` clause can be described with a JSON Schema extension that includes a JSON Pointer to name a field with which a given field is co-located.

The COBOL language requires a strict backwards reference to a previously-defined field, and the names must have the
name indentation level, making them peers within the same parent, reducing the need for complex pointers.

Occurs Depending On
-------------------

The complexity of `OCCURS DEPENDING ON` constructs arises because the size (`maxItems`) of the array is the value of
another field in the COBOL record definition.

Ideally, a JSON Reference names the referenced field as the `maxItems` attribute for an array. This, however,
is not supported, so an extension vocabulary is required.

EBCDIC Unpacking
----------------

There are a number of distinctive data types used by COBOL and represented in EBCDIC. The most notable of these is
the packed decimal representation of numbers. Each byte has two decimal digits. The final byte as a digit and sign
information in the lower four bits.

Further, COBOL records have a "RECFM" wrapper that may include bytes to describe the record's physical length.

Notes
-----

See https://json-schema.org/draft/2020-12/relative-json-pointer.html#RFC6901 for information on JSON Pointers.

Terminology
-----------

The JSON Schema specification talks about the data described by the schema as a "instance" of the schema. The schema
is essentially a class of object, the data is an instance of that class.

It's awkward to distinguish the more general use of "instance" from the specific use of "Instance of a JSON Schema".
We'll try to use `Instance`, and `NDInstance` to talk about the object described by a JSON Schema.

Goals
=====

Stingray Reader must support Delimited Files (JSON, YAML, TOML, and XML) as well as Non-Delimited Files, typified by COBOL. Other non-delimited formats should be describable. Additionally, workbook formats like XLSX, ODS, Numbers, and CSV should be handled properly.

The JSON Schema can be used to provide validation of an instance. It can also be used to unpack the instance. In the case of COBOL, the unpacking must be done lazily to properly handle `REDEFINES` and `OCCURS DEPENDING ON`.

Delimited Files
----------------

Delimited files have text representations with syntax defined by a module like `json`. Because of the presence of delimiters, individual character and byte counting isn't relevant.

Pythonic navigation through instances of delimited structures leverages the physical format's parser output. Since most formats provide a mixture of dictionaries and lists, `object["field"]` and `object[index]` work nicely.

The JSON Schema structure will parallel the instance structure.

Workbook Files
--------------

Files may also be complex binary objects described by workbook file for XLSX, ODS, Numbers, or CSV files. To an extent, these are somewhat like delimited files, since individual character and byte counting isn't relevant.

Pythonic navigation through instances of workbook row structures leverages the workbook format's parser output. Most workbooks are lists of cells; a schema with a flat list of properties will work nicely.

The `csv` fornmat is built-in. It's more like a workbook than it is like JSON or TPOML. For example, with simple CSV files, the JSON Schema must be a flat list of properties corresponding to the columns.

Non-Delimited Files (COBOL)
---------------------------

It's essential to provide Pythonic navigation through a COBOL structure. Because of `REDEFINES` clauses, the COBOL structure may not map directly to simple Pythonic dict and list types. Instead, the evaluation of each field must be strictly lazy.

This suggests several target constructs.

- `object.name("field").value()` should catapult down through the instance to the named field. Syntactic sugar might include `object["field"]` or `object.field`. Note that COBOL raises a compile-time error for a reference to an amiguous name; names may be duplicated, but the duplicates must be disambiguated with `OF` clauses.

- `object.name("field").index(x).value()` works when the field is a member of an array somewhere above it in the structure. Syntactic sugar might include `object["field"][x]` or `object.field[x]`.

These constructs are abbreviations for explicit field-by-field navigation. The field-by-field navigation involves explicitly naming all parent fields. Here are some constructs.

- `object.name("parent").name("child").name("field").value()` is the full navigation path to a nested field. This can be `object["parent"]["child"]["field"]`. A more sophisticated parser for URL path syntax might also be supported. `object.name["parent/child/field"]`.

- `object.name("parent").name("child").index(x).name("field").value()` is the full navigation path to a nesteed field with a parent occurs-depending-on clause. This can be `object["parent"]["child"][x]["field"]`. A more sophisticated parser for URL path syntax might also be supported. `object.name["parent/child/0/field"]`.

The COBOL `OF` construct provides parentage in reverse order. This means `object.name("field").of("child").of("parent").value()` is requred to parallel COBOL syntax. While unpleasant, it's helpful to support this.

The `value()` method can be helpful to be explicit about locating a value. This avoids eager evaluation of `REDEFINES` alternatives that happen to be invalid.

An alternative to the `value()` method is to use built-in special names `__int__()`, `__float__()`, `__str__()`, `__bool__()` to do conversion to a primitive type; i.e., `int(object.name["parent/child/field"])`. Additional functions like `asdict()`, `aslist()`, and `asdecimal()` can be provided to handle conversion edge cases.

We show these as multi-step operations with a fluent interface. This works out well when a nagivation context object is associated with each sequence of `object.name()...`, `object.index()`, and `object.of()` operations. The first call in the sequence emits a navigation object; all subsequent steps in the fluent interface return navigation objects. The final `value()` or other special method refers back to the original instance container for type conversion of an atomic field.

Each COBOL navigation step involves two parallel operations:

- Finding the definition of the named subschema within a JSON schema.

- Locating the sub-instance for the item. This is a slice of the instance.

The instance is a buffer of bytes (or characters for non-COBOL file processing.) The overall COBOL record has a starting offset of zero. Each DDE has an starting offset and length. For object property navigation this is the offset to the named property that describes the DDE. For array navigation, this is the index to a subinstance within an array of subinstances.

It's common practice in COBOL to use a non-atomic field as if it was atomic. A date field, for example, may have year, month, and day subfields that are rarely used independently. This means that JSON Schema array and object definitions are implicitly `type: "string"` to parallel the way COBOL treats non-atomic fields as `USAGE IS DISPLAY`.

Formal Definitions for Non-Delimited Files
===========================================

One approach to locating field values in a nested COBOL object with `REDEFINES` and `OCCURS DEPENDING ON` is to decorate the schema with instance location details for each item in the schema based on the current value of the instance.

This handles `OCCURS DEPENDING ON` by computing unique decorations for each instance.

We'll describe this as being done eagerly. Pragmatically, it can be done lazily and cached. The `REDEFINES` is handled by application program logic to avoid references to invalid data.

The instance, :math:`I`, is an array of :math:`b` characters or bytes.

..  math::

    I = \{ i_0, i_1, i_2, ..., i_{b-1} \}


We can slice :math:`I` using start, :math:`s`, and end, :math:`e` locations to create sub-instances.

..  math::

    I[s: e] = \{ i_x \mid s \leq x < e \}


It seems like :math:`I_{s:e}` might be slightly more traditional notation than :math:`I[s:e]`. :math:`I_{\{x \mid s \leq x < e\}}` seems fussy.

We can follow the Python convention of omitting :math:`s` when :math:`s = 0` and omitting :math:`e` when :math:`e = b`. This means that :math:`I \equiv I[:]`.

A Schema, :math:`S`, is a static description of a number of instances, :math:`I_0, I_1, I_2, ...`. We say the schema models an instance: :math:`S \tilde \models I`. The modelling is approximate; the model is incomplete. A more complete model requires resolution of ``OCCURS DEPENDING ON`` to compute locations, :math:`L`. This is :math:`S \circ L \models I`.
The schema, :math:`S`, composed with additional location information, :math:`L`, models the instance, :math:`I`.

A Schema has a structure with three kinds of elements:

- Atomic, with no further structure. This has a type conversion (or "unpack") function, :math:`t(I)`, to extract a value from an instance.
  It requires a length, :math:`l`, that defines a slice from the instance.
  We can say :math:`t_{S}(I[:l])` to refer to the type conversion function for a specific Schema, :math:`S`, used to decode an instance, :math:`I`.
  JSON Schema defines types of number, int, string, boolean, and null. Other types may need to be added to handle COBOL data.

- Array, with an Items subschema, :math:`S_i` that describes a repeating sub-instance.
  The number of sub instances, :math:`r` may be provided explicitly.
  In the ``OCCURS DEPENDING ON`` case, the value of :math:`r` is the value of another field.
  This means :math:`r = t_{S_d}(I)` because the ocurrences depend on the value in field :math:`S_d`.

- Object, with Properties structure that describes a number of subschema, :math:`S_{p_x}`.

We can summarize these three definitions as follows:

..  math::

    S = \begin{cases}
        \langle t(I), l \rangle &\text{if atomic} \\
        \langle S_i, r \rangle &\text{if array} \\
        \{S_{p_0}, S_{p_1}, S_{p_2}, ...\} &\text{if object}
    \end{cases}


The value of an instance, :math:`I`, interpreted by given schema, :math:`S`, is the type conversion function, :math:`t_S(I)`, applied to a slice of the instance defined by the length, :math:`l`, which is :math:`I[:l_S]`.


..  math::

    v = t_S(I[:l_S])


As suggested above, the ``OCCURS DEPENDING ON`` means the locations of sub-instances depend on  values elsewhere in the instance.

The starting location of a slice is computed for each schema item as follows:

- For an atomic item, it is the schema, :math:`S`. The starting offset, :math:`s`, is zero, and can be omitted, :math:`I[s=0: l] \equiv I[:l]`.

- For an array, each item within the array has its own start based on the index value, :math:`x`, and the length of the item sub-schema, :math:`S_i`. The start for a given index value, :math:`s(x) = x \times l(S_i)`; :math:`I[s: s+l] = I[x \times l(S_i): x \times l(S_i)+l(S_i)]`. The overall size of the array is :math:`r \times l(S_i)`; note that :math:`r` may be dependent on the value of another field.

- For an object, each property begins after the previous property. :math:`s(S_{p+1}) = s(S_{p}) + l(S_{p})`. This recurrence is a summation. :math:`s(S_{p+1}) = \sum_{0 \leq i < p+1}l(S_i)`.


The length of a schema, :math:`l(S)`, is the sum of the lengths of the items within the schema.

..  math::

    l(S) = \begin{cases} l &\text{if atomic} \\
                      l(S_i) \times r &\text{if array} \\
                      \sum_{S_{p} \in S} l(S_{p}) &\text{if object}
        \end{cases}


This can be unrolled into a top-down, post-order traversal of the schema to create Location objects.

JSON Schema Considerations
==========================

JSON Schema is focused on delimited data where a parser has handled data type conversions.

We require some extensions or adaptations to cover issues that not handled well by JSON Schema.

- COBOL encoded data (Packed-Decimal, Binary, etc.) JSON Schema presumes delimited files with a parser's conversions of data. We are driving the parser from the JSON Schema, and additional details are required.

- Occurs Depending On reference. JSON Schema limits the `maxItems` to an unsigned integer. We need to `$ref`, and an extension vocabulary is required.

- Workbook conversions of numeric data to durations or timestamps. This distinct from the `format` option on a `"type": "string"` where the format provides a pattern for parsing text. We need to specify an additional conversion.

See https://github.com/json-schema-org/json-schema-spec for the meta schema used to define a schema.

We have two overall strategies: Update the meta schema or adapt to the existing meta schema.

Update the meta schema
-----------------------

We could make some changes to the JSON Schema meta schema through a vocabulary addition.

- Add a new `decimal` type to the validation vocabulary.

- Permit a `$ref` validation for `"maxItems"` in the array vocabulary.

- Additional unpacking details for "integer" and "number" are needed. Currently "format" is used for strings. We could expand it's use to include encoding details for numbers. This would support bigendian integers, bigendian floats, packed decimal, and zoned decimal number, integer, and decimal values.

- Additional conversion details for Workbook numbers that are encodings of durations or timestamps.

We can then use a revised meta schema to validate the schema used for Workbooks or COBOL.

Adapt to the existing meta schema
----------------------------------

We could adapt to the existing JSON Schema meta schema.

We *can* consider COBOL encoded numeric values as `"type": "string"` with additional `"contentEncoding"` details. Calling these values strings seems to push the envelope a bit: JSON Schema generally avoies syntax and unpacking issues. The value truly is a number.

It seems improper to require a separate type conversion to create the needed, Python-specific `Decimal`.  A conversion can be *implicit*, using the "cobol" keyword value. Or, it can be *explicit*, using a new "conversion" keyword for extended types like timestamp, duration based on float, and `Decimal` based unpacking bytes.

This is the approach taken for Workbook values like durations and timestamps. A supplemental `"format": "duration"` is used to parse a string that encodes a duration value. Parsers, generally, don't do anything with this. Validation can be handled to conform the string matches the format. Decoding a useful object is left to the application.

The `"contentEncoding"` provides a way to describe COBOL Packed Decimal and Binary as strings of bytes. A `"cobol"` keyword gets Usage and Picture values required to decode EBCDIC.

Here's an example::

    {"type": "string",
     "contentEncoding": "packed-decimal",
     "cobol": "05 SOME-FIELD USAGE COMP-3 PIC S999V99",
     "conversion": "decimal"
    }

Other `"contentEncoding"` values include "bigendian-h",  "bigendian-i", "bigendian-q" (for short, int, and long long values.) Also, "bigendian-float" and "bigendian-double". And, of course, "CP037" or "EBCDIC" to decode ordinary strings from EBCDIC to native text.

This permits a little more flexibility for moving to native (non-EBCDIC) file formats. Instead of the "cobol" keyword to propvide unpackong details, we can use a "struct" keyword to provide Python's "struct" module format codes.

Also, we need to add "maxItemsDependsOn" keyword to include the "$ref" value for arrays. Here's an example::

    {"type": "array",
     "maxItemsDependsOn": {"$ref": "#/path/to/SIZE-FIELD"},
     "cobol": "05 THINGS OCCURS DEPENDING ON SIZE-FIELD",
     "items": {...}
     }

For `REDEFINES`, we use the existing "oneOf"::

    "REDEFINES-A-B-C": {
        "oneOf": [
            {"type": "object", "properties": {"A": {"type": "number"}}},
            {"type": "object", "properties": {"B": {"type": "string"}}},
            {"type": "object", "properties": {"C": {"type": "boolean"}}},
        ]
    }

To provide COBOL-style naming to OneOf/REDEFINES schema, we impose a rule that each alternative must be an object, and each object must have at most one named property. The parent item is effectively anonymous, since it's never used in COBOL.



Schema Design
==============

Here is the extended JSON Schema definition.
This is a translation of the various JSON Schema constructs into Python class definitions.
These objects must be considered immutable. (Pragmatically, RefTo objects can be updated
to resolve forward references.)

A :py:class:`Schema` is used to describe an :py:class:`Instance`.
For non-delimited instances, the schema requires additional :py:class:`Location` information;
this must be computed lazily to permits ``OCCURS DEPENDING ON`` to work.
For delimited instances, no additional data is required, since the parser located all object
boundaries and did conversions to Python types. Similarly, for workbook instances, the
underlying workbook parser can create a row of Python objects.

The `DependsOnArraySchema` is an extension to handle references to another
field's value to provide ``minItems`` and ``maxItems`` for an array.
This handles the COBOL ``OCCURS DEPENDING ON``. This requires a reference to another
field which is a reference to another field instead of a simple value.

The COBOL ``REDEFINES`` clause is handled created a OneOf suite of alternatives
and using some JSON Schema "$ref" references to preserve the original, relatively flat
naming for an elements and the element(s) which redefine it.

..  code-block::

    @startuml
        abstract class Schema {
            type: string
        }

        class AtomicSchema

        class ArraySchema

        class DependsOnArraySchema

        class ObjectSchema

        class OneOfSchema

        class RefToSchema

        Schema <|-- AtomicSchema
        Schema <|-- ArraySchema
        Schema <|-- ObjectSchema
        AtomicSchema <|-- OneOfSchema
        AtomicSchema <|-- RefToSchema
        ArraySchema <|-- DependsOnArraySchema
        RefToSchema --> Schema
        'OneOfSchema --> "*" Schema : Redefines
        'ObjectSchema --> "*" Schema : Properties
        'ArraySchema --> Schema : Items
        'DependsOnArraySchema --> Schema : maxItems
    @enduml

"""

import abc
import csv
from decimal import Decimal
import logging
from pprint import pprint
import sys
from typing import (
    Union, Any, NamedTuple, Protocol, TextIO, Optional, overload, Iterator, cast, Type,
    Callable, TypeVar, Generic, AnyStr, Sequence
)
import weakref

from stingray import estruct

logger = logging.getLogger("stingray.schema_instance")

class DesignError(BaseException):
    pass

JSON = Union[None, bool, int, float, str, list[Any], dict[str, Any]]


class Reference(Protocol):
    """
    # TODO: Formalize a ref_to protocol, used by ``RefToSchema`` and ``DependsOnArraySchema``.
    """
    pass

class Schema:
    """
    Base class for Schema definitions.

    This is Python class that wraps the a JSONSchema definition.
    The class provides slightly simpler navigation via attribute names
    instead of dictionary keys.

    Generally, these should be seen as immutable. To permit forward
    references, the RefTo subclass needs to be mutated.
    """
    def __init__(self, attributes: JSON) -> None:
        self._attributes = cast(dict[str, Any], attributes)
        # keys for _attributes include "type", "items", "properties", "format", etc.
        self.ref: Optional[str] = None
        self.ref_to: Optional[Schema] = None
    @property
    def type(self) -> str:
        try:
            return cast(str, self._attributes["type"])
        except KeyError as ex:
            logger.error(f"{ex} on {self!r}")
            raise
    @property
    def attributes(self) -> JSON:
        return self._attributes
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes})"
    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Schema):
            return type(self) == type(other) and self._attributes == other._attributes
        return NotImplemented  # pragma: no cover
    def print(self, indent: int=0, hide: set[str] = set()) -> None:
        show_attr = {k: self._attributes[k] for k in self._attributes if k not in hide}
        line = f"{indent*'  '}{self.__class__.__name__}({show_attr})"
        print(line)
    def json(self) -> JSON:
        return self._attributes
    def dump_iter(self, nav: Optional["Nav"], indent: int = 0) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """Yield nesting, schema, indices, and value"""
        if nav:
            yield indent, self, (), nav.value()

class AtomicSchema(Schema):
    pass

class ArraySchema(Schema):
    def __init__(self, attributes: JSON, items: "Schema") -> None:
        super().__init__(attributes)
        self._items = items
        self._maxItems: int = (
            cast(dict[str, Any], attributes).get("maxItems")
            or cast(dict[str, Any], attributes).get("minItems")
            or 1
        )
    @property
    def items(self) -> "Schema":
        return self._items
    @property
    def maxItems(self) -> int:
        return self._maxItems
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.items})"
    def print(self, indent: int=0, hide: set[str] = set()) -> None:
        super().print(indent, hide={"items"})
        self._items.print(indent+1)
    def dump_iter(self, nav: Optional["Nav"], indent: int = 0) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """Yield nesting, schema, indices, and object"""
        # TODO: Enumerate index values from minItems or 0 to maxItems
        yield indent, self, (), None
        if nav:
            yield from self._items.dump_iter(nav.index(0), indent+1)

class DependsOnArraySchema(ArraySchema):
    """
    An array with a size that depends on another field.
    The extension "maxItemsDependsOn" attribute has a reference to another
    field in this definition.
    """
    def __init__(self, attributes: JSON, items: "Schema", ref_to: Optional["Schema"]) -> None:
        super().__init__(attributes, items)
        self._items = items
        self.max_ref = self._attributes["maxItemsDependsOn"].get("$ref")
        self.max_ref_to = ref_to
    @property
    def maxItems(self) -> int:
        raise TypeError(f"maxItems depends on {self.max_ref_to} value of an instance")

class ObjectSchema(Schema):
    def __init__(self, attributes: JSON, properties: dict[str, "Schema"]) -> None:
        super().__init__(attributes)
        self.properties = properties
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.properties})"
    def print(self, indent: int=0, hide: set[str] = set()) -> None:
        super().print(indent, hide={"properties"})
        for prop in self.properties.values():
            prop.print(indent+1)
    def dump_iter(self, nav: Optional["Nav"], indent: int = 0) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """Yield nesting, schema, indices, and object"""
        yield indent, self, (), None
        if nav:
            for name, child in self.properties.items():
                yield from child.dump_iter(nav.name(name), indent+1)

class OneOfSchema(Schema):
    def __init__(self, attributes: JSON, alternatives: list["Schema"]) -> None:
        super().__init__(attributes)
        self.alternatives = alternatives
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.alternatives})"
    @property
    def type(self) -> str:
        return "oneOf"
    def print(self, indent: int=0, hide: set[str] = set()) -> None:
        super().print(indent, hide={"oneOf"})
        for alt in self.alternatives:
            alt.print(indent+1)
    def dump_iter(self, nav: Optional["Nav"], indent: int = 0) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """Yield nesting, schema, indices, and object"""
        yield indent, self, (), None
        if nav:
            for alt in self.alternatives:
                # Not all alternatives will be valid.
                try:
                    yield from alt.dump_iter(nav, indent+1)
                except (IndexError, KeyError):  # pragma: no cover
                    pass

class RefToSchema(Schema):
    """Must deference type and attributes properties."""
    def __init__(self, attributes: JSON, ref_to: Optional[Schema]) -> None:
        super().__init__(attributes)
        self.ref = self._attributes.get("$ref")
        self.ref_to = ref_to
    def __repr__(self) -> str:
        # This isn't exactly right.
        # To avoid recursion and provide a display value, we show a name here.
        return f"{self.__class__.__name__}({self._attributes}, {self.ref})"
    @property
    def type(self) -> str:
        if self.ref_to:
            return self.ref_to.type
        raise ValueError(f"Invalid {self.__class__.__name__}")
    @property
    def attributes(self) -> JSON:
        if self.ref_to:
            return self.ref_to.attributes
        raise ValueError(f"Invalid {self.__class__.__name__}")
    @property
    def properties(self) -> dict[str, "Schema"]:
        if self.ref_to:
            return cast(ObjectSchema, self.ref_to).properties
        raise ValueError(f"Invalid {self.__class__.__name__}")
    @property
    def items(self) -> Schema:
        if self.ref_to:
            return cast(ArraySchema, self.ref_to).items
        raise ValueError(f"Invalid {self.__class__.__name__}")
    def dump_iter(self, nav: Optional["Nav"], indent: int = 0) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """Yield nesting, schema, indices, and object"""
        yield indent, self, (), None

class SchemaMaker:
    """
    Build a Schema structure from a JSON Schema document.

    This doesn't do much, but it allows us to use classes to define methods 
    that apply to the JSON Schema constructs instead of referring to them 
    as the source document dictionaries.

    All ``$ref`` names are expected to refer to explicit ``:anchor`` names within this schema.
    """
    ATOMIC = {"null", "boolean", "integer", "number", "string"}  # Requires extended meta-schema to add "decimal"
    
    def __init__(self) -> None:
        self.name_cache: dict[str, Schema] = {}
        self.fixup_list: list[Schema] = []
        
    def walk_schema(self, source: JSON, path: tuple[str, ...] = ()) -> Schema:
        """
        Starting with JSON Schema document, create Schema objects.
        Relies on an ``maxItemsDependsOn`` extension to describe `OCCURS DEPENDING ON`.
        """
        schema: Schema
        source = cast(dict[str, Any], source)

        if source.get("oneOf"):
            alternatives_schema = [self.walk_schema(value, path) for value in source.get("oneOf", [])]
            schema = OneOfSchema(attributes=source, alternatives=alternatives_schema)
        
        elif source.get("$ref"):
            # A reference to a redefined field.
            # A ``name REDEFINES parent`` is a reference to ``REDFINES-parent.name``.
            # The first definition of ``name`` becomes ``REDEFINES-name.name``.
            try:
                ref_uri = source["$ref"]
                assert ref_uri.startswith("#"), f"Invalid {ref_uri}"
                _, _, ref_name = ref_uri.partition("#")
                ref_to = self.name_cache[ref_name]
                schema = RefToSchema(source, ref_to)
            except KeyError:
                schema = RefToSchema(source, None)
                self.fixup_list.append(schema)

        elif source["type"] in self.ATOMIC:
            schema = AtomicSchema(source)
        
        elif source["type"] == "array" or "items" in source:
            items_schema = self.walk_schema(source.get("items", {}), path)
            if "maxItemsDependsOn" in source:
                try:
                    ref_uri = source["maxItemsDependsOn"].get("$ref")
                    assert ref_uri.startswith("#"), f"Invalid {ref_uri}"
                    _, _, ref_name = ref_uri.partition("#")
                    ref_to = self.name_cache[ref_name]
                    schema = DependsOnArraySchema(attributes=source, items=items_schema, ref_to=ref_to)
                except KeyError:
                    # Forward references for OCCURS DEPENDS ON are sketchy at best.
                    # If we wanted to resolve them later...
                    # schema = DependsOnArraySchema(attributes=source, items=items_schema, ref_to=None)
                    # self.fixup_list.append(schema)
                    raise ValueError(f"unknown $ref in {source!r}; forward references for maxItemsDependsOn aren't supported")
            else:
                schema = ArraySchema(attributes=source, items=items_schema)
            
        elif source["type"] == "object" or "properties" in source:
            properties_schema = {
                name: self.walk_schema(value, path+(name,)) 
                for name, value in source.get("properties", {}).items()
            }
            schema = ObjectSchema(attributes=source, properties=properties_schema)
        
        else:
            # Serious Design Error...
            raise ValueError(f"Unknown {source['type']=!r} in {source}")
            
        self.name_cache[source.get("$anchor", source.get("title", "*UNNAMED*"))] = schema
        return schema
            
    def resolve(self, schema: Schema) -> Schema:
        """
        Resolve forward $ref references.
        """
        for fixup in self.fixup_list:
            try:
                ref_uri = fixup._attributes["$ref"]
                assert ref_uri.startswith("#"), f"Invalid {ref_uri}"
                _, _, ref_name = ref_uri.partition("#")
                ref_to = self.name_cache[ref_name]
            except KeyError:
                raise ValueError(f"Cannot resolve {fixup.ref!r} in {list(self.name_cache.keys())}")
            fixup.ref_to = ref_to
        return schema
                            
    @classmethod
    def from_json(cls: Type["SchemaMaker"], source: JSON) -> Schema:
        sm = cls()
        schema = sm.walk_schema(source)
        sm.resolve(schema)
        return schema


"""
Unpackers
=========

An `Unpacker` is a strategy class that handles details of physical unpacking of bytes or text. We call it an `Unpacker`, because it's similar to `struct.unpack`. 

The JSON Schema's intent is to depend on delimited files, using a separate parser. For this application, however, the schema is used to provide information to the parser. 

To work with the variety of instance data, we have several subclasses of `Instance` and related `Unpacker` classes:

- **Non-Delimited**. These cases use `Location` objects. We define an ``NDInstance`` as a common protocol
  wrapped around ``AnyStr`` types. There are three sub-cases depending on the underlying object.

    - **COBOL Bytes**. An `NDInstance` type union includes `bytes`. The `estruct` module is a COBOL replacement for the `struct` module. The JSON Schema requires extensions to handle COBOL complexities.

    - **STRUCT Bytes**. An `NDInstance` type union includes `bytes`. The `struct` module `unpack` and `calcsize` are used directly. This means the field codes must match the `struct` module's definitions. This can leverage some of the same extensions as COBOL requires.

    - **Text**. An `NDInstance` type union includes `str`. This is the case with non-delimited text that has plain text encodings for data. The DISPLAY data will be ASCII or UTF-8, and any COMP/BINARY numbers are represented as text.

- **Delimited**. These cases do not use `Location` objects. There are two sub-cases:

    - **JSON Objects**. This is a Union of ``dict[str, Any] | Any | list[Any]``. 
      The instance is created by some external unpacker, and is already in a Python native structure. 
      Unpackers include ``json``, ``toml``, and ``yaml``. A wrapper around an ``xml`` parser
      can be used, also.
      We'll use a ``JSON`` type hint for objects this unpacker works with.

    - **Workbook Rows**. These include CSV, ODS, XLSX, and Numbers documents. 
      The instance is a structure created by the workbook module as an unpacker. 
      The :py:mod:`csv` unpacker is built-in. 
      These all use ``list[Any]`` for objects this unpacker works with.

Unpacking is a plug-in strategy. 
For non-delimited data, it combines some essential location information with a `value()` method that's unique to the instance source data. 
For delimited data, it provides a uniforma interface for the various kinds of spreadsheets.

The JSON Schema extensions to drive unpacking include the `"cobol"` keyword. The value for this has the original COBOL DDE. This definition can have  USAGE and  PICTURE clauses that define how bytes will encode the value.

..  code-block::
    
    @startuml
        abstract class Unpacker {
            calcsize(schema): int
            value(schema, instance): Any
            nav(schema, instance): Nav
        }
        
        class Location
        
        abstract class Nav
        
        abstract class Schema
        
        Unpacker "n" -- Schema
        Unpacker --> Location : creates
        Unpacker --> Nav : creates
        
        abstract class NonDelimited
        Unpacker <|-- NonDelimited 
    
        class EBCDIC
        NonDelimited <|-- EBCDIC 
    
        class Struct
        NonDelimited <|-- Struct 
    
        class Text
        NonDelimited <|-- Text 
        
        class Delimited
        Unpacker <|-- Delimited
        
        class Sequence
        Delimited <|-- Sequence
        
        class Mapping
        Delimited <|-- Mapping
    
    @enduml
    
Implementation Notes
====================

We need three separate kinds of ``Unpacker`` subclasses to manage the kinds of ``Instance`` subclasses:

- The `NonDelimited` subclass of `Unpacker` handles an `NDInstance` 
  which is either a string or bytes with non-delimited data. 
  The `Location` reflects an offset into the `NDInstance`.

- The `Delimited` subclass of `Unpacker` handles delimited data, generally using ``JSON`` 
  as a type hint. This will have a `dict[str, Any] | list[Any] | Any` structure. 

- A `Workbook` subclass of `Unpacker` wraps a workbook parser creating a ``WBInstance``.
  Generally workbook rows are `list[Any]` structures.

An ``Unpacker`` instance is a factory for ``Nav`` objects. When we need to navigate around
an instance, we'll leverage ``unpacker.nav(schema, instance)``. Since the
schema binding doesn't change very often, ``nav = partial(unpacker.nav, (schema,))`` is
a helpful simplification. With this partial, ``nav(instance).name(n)`` or ``nav(instance).index(n)`` are all that's needed
to locate a named field or apply array indices.


Unpacker Size Computations
--------------------------

The sizes are *highly* dependent on format information that comes from COBOL DDE (or other schema details.) A `cobol` extension to JSON Schema provides the COBOL-syntax `USAGE` and `PICTURE` clauses required to parse bytes. There are four overall cases, only two of which require careful size computations.

**Non-Delimited COBOL**.  See https://www.ibm.com/docs/en/cobol-zos/4.2?topic=clause-computational-items and https://www.ibm.com/docs/en/cobol-zos/4.2?topic=entry-usage-clause and https://www.ibm.com/docs/en/cobol-zos/4.2?topic=entry-picture-clause.

-  `USAGE DISPLAY`. `PIC X...` or `PIC A...`. Data is text. Size given by the picture. Value is `str`.

-  `USAGE DISPLAY`. `PIC 9...`. Data is "Zoned Decimal" text. Size given by the picture. Value is `decimal`.

-  `USAGE COMP` or `USAGE BINARY` or `USAGE COMP-4`. `PIC 9...`. Data is bytes. Size based on the picture: 1-4 digits is two bytes. 5-9 digits is 4 bytes. 10-18 is 8 bytes. Value is a `int`. 


-  `USAGE COMP-1`. `PIC 9...`. Data is 32-bit float. Size is 4. Value is `float`. 

-  `USAGE COMP-2`. `PIC 9...`. Data is 64-bit float. Size is 8. Value is `float`.

-  `USAGE COMP-3` or `USAGE PACKED-DECIMAL`. `PIC 9...`. Data is two-digits-per-byte packed decimal. Value is a `decimal`.


**Non-Delimited Native**. Follows the Python `struct` module definitions. The `struct.calcsize()` function computes the structure's size. `struct.unpack()` unpacks the values using the format specification. Or `maxLength` can be used to define sizes.

**Delimited**. The underlying parser (JSON, YAML, TOML, XML) decomposed the data and performed conversions. The schema conversions *should* match the data that's present

**Workbook**. Generally, an underlying workbook unpacker is required. For CSV, the data is all strings, conversions are defined only in the schema.
"""

# These are values for the "conversion" keyword, part of the extended vocabulary for COBOL
# and Workbooks.
# Date, Time, Timestamp, Duration, and Error may also be part of these conversions.
# Also, a "decimal_2" might be useful for truncating float values to pennies.

CONVERSION: dict[Union[str, None], Callable[[Any], Any]] = {
    "null": lambda x: None,
    "bool": bool,
    "integer": int,
    "number": float,
    "string": str,
    "decimal": Decimal,
    None: lambda x: x,
}

class NDInstance(Protocol):
    """
    The essential features of a non-delimited instance.
    The underlying data is ``AnyStr``, either bytes or text.
    """
    # Not Used
    def __init__(self, source: AnyStr) -> None: ...
    def unpacker(self, unpacker: "NonDelimited") -> "NDInstance": ...
    def schema(self, schema: "Schema") -> "NDInstance": ...

    @overload
    def __getitem__(self, index: int) -> Union[str, int]: ...
    @overload
    def __getitem__(self, index: slice) -> Union[str, bytes]: ...


class DInstance(Protocol):
    """
    JSON/YAML/TOML documents are wild and free.
    Pragmatically, we want o supplement these classes with methods
    that emit ``DNav`` objects to manage navigating an object and a schema in parallel.

    Not sure this is useful...
    """
    def __init__(self, source: JSON) -> None: ...
    def unpacker(self, unpacker: "Delimited") -> "DInstance": ...
    def schema(self, schema: "Schema") -> "DInstance": ...


class WBInstance(Protocol):
    """
    CSV files are ``list[str]``. All other workbooks tend to be  ``list[Any]``
    because their unpacker modules do conversions.
    We'll tolerate any sequence type.
    """
    # Not Used
    def __init__(self, source: Sequence[Any]) -> None: ...
    def unpacker(self, unpacker: "WBUnpacker") -> "WBInstance": ...
    def schema(self, schema: "Schema") -> "WBInstance": ...

    @overload
    def __getitem__(self, index: int) -> Any: ...
    @overload
    def __getitem__(self, index: slice) -> Any: ...

# We'll define an Instance type is the union of the protocols for
# non-delimited instances, workbook instances, and Python native ("Non-delimited" or "JSON")
# objects.
Instance = TypeVar('Instance', NDInstance, DInstance, WBInstance)

class Unpacker(Generic[Instance]):
    """
    An Unpacker helps convert data from an ``Instance``.
    For NDInstances, this involves size calculations and value conversions.
    For WBInstances and JSON, this is a pass-through because the sizes don't matter and
    the values are already Native Python objects.
    """
    def calcsize(self, schema: Schema) -> int: ...
        
    def value(self, schema: Schema, instance: Instance) -> Any: ...
        
    def nav(self, schema: Schema, instance: Instance) -> "Nav": ...


class NonDelimited(Unpacker[NDInstance]):
    """
    NDInstance protocol is a Sequence, either bytes or str, the AnyStr type.
    This abstract Unpacker can perform size calculations or value conversions on the underlying data.
    """
    def nav(self, schema: Schema, instance: NDInstance) -> "Nav":
        location = LocationMaker(self, schema).from_instance(instance)
        return NDNav(self, location, instance)

class EBCDIC(NonDelimited):
    """
    Unpacker for Non-Delimited EBCDIC bytes.

    Uses :py:mod:`estruct` module for calcsize and value of Big-Endian, EBCDIC data.
    This requires the "cobol" and "conversion" keywords, part of the extended vocabulary for COBOL.
    A `"cobol"` keyword gets Usage and Picture values required to decode EBCDIC. 
    A "conversion" keyword converts to a more useful Python type.

    This assumes the 
    COBOL encoded numeric can be ``"type": "string"`` with additional ``"contentEncoding"`` details. 
    
    This class implements a ``"contentEncoding"`` using values of "packed-decimal", and "cp037",
    to unwind COBOL Packed Decimal and Binary as strings of bytes.  
    """
    def calcsize(self, schema: Schema) -> int:
        # assert schema.attributes.get("contentEncoding") in {"packed-decimal", "cp037"}
        format = cast(dict[str, Any], schema.attributes).get("cobol", "USAGE DISPLAY")
        return estruct.calcsize(format)

    def value(self, schema: Schema, instance: NDInstance) -> Any:
        # assert schema.attributes.get("contentEncoding") in {"packed-decimal", "cp037"}
        format = cast(dict[str, Any], schema.attributes).get("cobol", "USAGE DISPLAY")
        conversion_func = CONVERSION[cast(dict[str, Any], schema.attributes).get("conversion")]
        return conversion_func(estruct.unpack(format, cast(bytes, instance)))
    
class Struct(NonDelimited):
    """
    Unpacker for Non-Delimited bytes.

    Uses built-in :py:mod:`struct` module for calcsize and value.
    TODO: Finish this.
    """
    pass

class TextUnpacker(NonDelimited):
    """
    Unpacker for Non-Delimited string values.

    Uses string slicing and built-ins.
    This is for a native Unicode (or ASCII) text-based format.
    If utf-16 is being used, this is effectively a Double-Byte Character Set used by COBOL.
    
    A universal approach is to include ``maxLength`` (optionally ``minLength``) attributes 
    on each field. ``maxLength`` == the length of the field == ``minLength``.
    
    While it's tempting to use "type": "number" on this text data, it can be technically suspicious.
    If file has strings, conversions may part of the application's use 
    of the data, not the data itself. We use a "conversion" keyword to do these conversions
    from external string to internal Python object.
    
    For various native bytes formats, this is a `{"type": "string", "contentEncoding": "struct-xxx"}`
    where the Python :py:mod:`struct` module codes are used to define the number of interpretation of the bytes.
    
    TODO: For COBOL, the "cobol" keyword provides USAGE and PICTURE. This defines size.
    In this case, since it's not in EBCDIC, we can use :py:mod:`struct` to unpack COMP values.
    
    This requires the "cobol" and "conversion" keywords, part of the extended vocabulary for COBOL.
    A `"cobol"` keyword gets Usage and Picture values required to decode EBCDIC. 
    A "conversion" keyword converts to a more useful Python type.
        
    (An alternative approach is to use the ``pattern`` attribute to provide length information.
    This is often {"type": "string", "pattern": "^.{64}$"} or similar. This can provide a length.
    Because patterns can be hard to reverse engineer, we don't use this.)
    """
    def calcsize(self, schema: Schema) -> int:
        if "maxLength" in cast(dict[str, Any], schema.attributes):
            return int(cast(dict[str, Any], schema.attributes)["maxLength"])
        elif "cobol" in cast(dict[str, Any], schema.attributes):
            representation = estruct.Representation.parse(cast(dict[str, Any], schema.attributes)["cobol"])
            return representation.picture_size
        else:
            raise ValueError(f"can't compute size of {schema}; neither maxLength nor cobol provided")
    def value(self, schema: Schema, instance: NDInstance) -> Any:
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(cast(dict[str, Any], schema.attributes).get("conversion", type_value), lambda x: x)
        return conversion(instance)

class Delimited(Unpacker[DInstance]):
    """
    Unpacker for Delimited values, i.e. ``JSON`` documents wrapped as a ``DInstance``.

    An instance will be ``list[Any] | dict[str, Any] | Any``.
    It will is built by a separate parser.

    For JSON/YAML/TOML, the instance *should* have same structure as the schema.
    For XML, this should also be true.

    A subclass handles workbooks which are narrowed to be ``list[Any]``.

    The sizes and formats of delimited data don't matter:
    the :py:func:`calcsize` function returns 1 to act as a position in a sequence of values.
    """
    def calcsize(self, schema: Schema) -> int:
        return 1
    def value(self, schema: Schema, instance: DInstance) -> Any:
        return instance
    def nav(self, schema: Schema, instance: DInstance) -> "Nav":
        return DNav(self, schema, cast(JSON, instance))


class WBUnpacker(Unpacker[WBInstance]):
    """
    Unpacker for Workbook-defined values.

    Most of WBInstances defer to another module for unpacking.
    CSV, however, relies on the :py:mod:`csv` module, where the instance is ``list[str]``.
    
    While it's tempting to use "type": "number" on CSV data, it's technically suspicious.
    The file has strings, and only strings. Conversions are part of the application's use 
    of the data, not the data itself.
    """
    def calcsize(self, schema: Schema) -> int:
        return 1
    def value(self, schema: Schema, instance: WBInstance) -> Any:
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(cast(dict[str, Any], schema.attributes).get("conversion", type_value), lambda x: x)
        return conversion(instance)
    def nav(self, schema: Schema, instance: WBInstance) -> "Nav":
        return WBNav(self, schema, instance)

# A handy alias
CSVUnpacker = WBUnpacker

class JSONUnpacker(Delimited):
    """
    Unpacker for delimited values from JSON documents.

    JSON, however, relies on the :py:mod:`json` module, where the instance can
    be ``list[Any]`` or ``dict[str, Any]``.

    TODO: This should be the same as Delimited.
    """

    def calcsize(self, schema: Schema) -> int:
        return 1

    def value(self, schema: Schema, instance: "DInstance") -> Any:
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(cast(dict[str, Any], schema.attributes).get("conversion", type_value), lambda x: x)
        return conversion(instance)

    def nav(self, schema: Schema, instance: "DInstance") -> "Nav":
        return DNav(self, schema, cast(JSON, instance))


"""
Locations
=========

A `Location` is required to unpack bytes from non-delimited instances. This is a feature of the `NonDelimited` subclass of `Unpacker` and the associated `NDNav` class.

It's common to consider the `Location` details as "decoration" applied to a `Schema`. An implementation that decorates the schema requires a stateful schema and cant process more than one `Instance` at a time.

We prefer to have `Location` objects as "wrappers" on `Schema` objects; the `Schema` remains stateless and we process multiple `NDInstance` objects with distinct `Location` objects.

Each `Location` object contains a `Schema` object and additional start and end offsets. This may be based on the values of dependencies like `OCCURS DEPENDING ON` and `REDEFINES`.

The abstract `Location` class is built by a `LocationMaker` to provide specific offsets and sizes for non-delimited files with `OCCURS DEPENDING ON`. The `LocationMaker` seems to be part of the `Unpacker` class definition.

@startuml
    abstract class Unpacker {
        value(schema, NDInstance): Any
        size(schema): int
        location(schema, NDInstance): Location
    }
    abstract class Schema {
        type: string
    }
    abstract class NDInstance
    
    class Location {
        schema: Schema
        unpacker: Unpacker
        start: int
        end: int
        value(NDInstance): Any
    }
    
    Location ...> NDInstance
    Unpacker -> Location
    Location -> Schema
    Unpacker .> Schema : "Uses"
    
    class AtomicLocation
    Location <|-- AtomicLocation 
    class ArrayLocation
    Location <|-- ArrayLocation 
    class ObjectLocation
    Location <|-- ObjectLocation
    class OneOfLocation
    Location <|-- OneOfLocation
    class RefToLocation
    Location <|-- RefToLocation
@enduml

"""


class Location(abc.ABC):
    """
    A Location is used to navigate within an `NDInstance` objects.

    The ``Unpacker[NDInstnace]`` strategy is a subclass of NonDelimited,
    one of EBCDIC(), Struct(), or TextUnpacker().

    The value() method delegates the work to the ``Unpacker`` strategy.

    TODO: Add unpacker and locationMaker to initialization.
    """
    def __init__(self, schema: Schema, start: int, end: int = 0) -> None:
        self.schema = schema
        self.start = start
        if end:
            self.end = end
            self.size = end-start
        else:
            # References to other fields don't have sizes themselves. 
            self.end = start
            self.size = 0
        # To be set later, by :py:class:`LocationMaker`.
        self.unpacker: Unpacker[NDInstance]
        self.locationMaker: weakref.ReferenceType["LocationMaker"]

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.schema}, {self.start}, {self.end})"

    def __str__(self) -> str:
        return f"{cast(dict[str, Any], self.schema.attributes).get('cobol')} {self.start} {self.end}"

    @abc.abstractmethod
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any: ...

    @abc.abstractmethod
    def value(self, instance: NDInstance, offset: int = 0) -> Any: ...

    @property
    def referent(self) -> "Location":
        """Most things refer to themselves. a RefToLocation, however, overrides this."""
        return self

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, self.__class__):
            return (
                self.schema == other.schema
                and self.start == other.start
                and self.end == other.end
            )
        return NotImplemented  # pragma: no cover

    @abc.abstractmethod
    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, "Location", tuple[int, ...], Optional[bytes], Any]]: ...


class AtomicLocation(Location):
    """
    type(Schema) == AtomicSchema
    The location of a COBOL elementary item.
    """
    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        For an atomic value, locate the underlying value. This may involve unpacking.
        """
        logger.debug(f"{self}, {instance}, {instance[self.start+offset: self.end+offset]!r}")
        return self.unpacker.value(
            self.schema, 
            cast(NDInstance, instance[self.start+offset: self.end+offset])
        )
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any:
        return instance[self.start+offset: self.end+offset]

    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        An Iterator over tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), nav.raw(), nav.value()


class ArrayLocation(Location):
    """
    type(Schema) == ArraySchema
    The location of an array of instances with the same schema. A COBOL ``OCCURS`` item.
    """
    def __init__(self, schema: Schema, item_size: int, item_count: int, items: "Location", start: int, end: int) -> None:
        super().__init__(schema, start, end)
        self.item_size = item_size
        self.item_count = item_count
        self.items = items
    def __repr__(self) -> str:
        return (
            f"{self.__class__.__name__}({self.schema}, {self.item_size}, {self.item_count}, {self.items}, {self.start}, {self.end})"
        )
    def __str__(self) -> str:
        if "cobol" in cast(dict[str, Any], self.schema.attributes):
            return f"{cast(dict[str, Any], self.schema.attributes)['cobol']} {self.start} {self.end}"
        return f"Array [{self.item_count}] {self.start} {self.end}"
    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        array_value = [
            self.items.value(instance, offset=offset+i*self.item_size) 
            for i in range(self.item_count)
        ]
        return array_value
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any:
        return instance[self.start+offset: self.end+offset]
    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        An Iterator over tuples of (indent, Location, array indices, raw bytes, value)
        """
        # TODO: Step through array index combinations.
        # For now, we provide the index=0 value only
        yield indent, self, (), None, None
        yield from self.items.dump_iter(nav.index(0), indent+1)


class ObjectLocation(Location):
    """
    type(Schema) == ObjectSchema
    The location of an object with a dictionary of named properties. A COBOL group-level item.
    """
    def __init__(self, schema: Schema, properties: dict[str, "Location"], start: int, end: int) -> None:
        super().__init__(schema, start, end)
        self.properties = properties
        self.size = sum(p.size for p in self.properties.values())
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.schema}, {self.properties}, {self.start}, {self.end})"
    def __str__(self) -> str:
        if "cobol" in cast(dict[str, Any], self.schema.attributes):
            return f"{cast(dict[str, Any], self.schema.attributes)['cobol']} {self.start} {self.end}"
        return f"Object {self.start} {self.end}"
    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        object_value = {
            name: self.properties[name].value(instance, offset=offset)
            for name in cast(ObjectSchema, self.schema).properties
        }
        return object_value
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any:
        return instance[self.start+offset: self.end+offset]
    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        An Iterator over tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), None, None
        for name, child in self.properties.items():
            yield from child.dump_iter(nav.name(name), indent+1)

    
class OneOfLocation(Location):
    """
    type(Schema) == OneOfSchema
    The location of an object which has a list of REDEFINES alternatives.
    """
    def __init__(self, schema: Schema, alternatives: list[Location], start: int, end: int) -> None:
        super().__init__(schema, start, end)
        self.alternatives = {
            alt.schema._attributes.get("$anchor", alt.schema._attributes.get("title", "UNNAMED")): alt
            for alt in alternatives
        }
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.schema}, {list(self.alternatives.values())!r}, {self.start}, {self.end})"
    def __str__(self) -> str:
        return f"OneOf [{', '.join(list(self.alternatives.keys()))}] {self.start} {self.end}"
    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        first, *others = self.alternatives.values()
        return first.value(instance, offset)
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any:
        return instance[self.start+offset: self.end+offset]
    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        An Iterator over tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), None, None
        for index, alt in enumerate(self.alternatives.values()):
            sub_nav = NDNav(self.unpacker, alt, nav.instance)
            yield from alt.dump_iter(sub_nav, indent+1)

    
class RefToLocation(Location):
    """
    type(Schema) == RefToSchema
    Part of REDEFINES; this is the COBOL-visible name of a path into a ``OneOfLocation`` alternative.

    This could also be part of OCCURS DEPENDING ON.
    If used like this, it would be the COBOL-visible name of an item with an array size.
    The OCCURS DEPENDING ON doesn't formalize this, however.
    """
    def __init__(self, schema: Schema, anchors: dict[str, Location], start: int, end: int) -> None:
        super().__init__(schema, start, end)
        self.anchors = anchors
    def __str__(self) -> str:
        return f"RefTo {self.schema.ref} {self.start} {self.end}"
    @property
    def properties(self) -> dict[str, Location]:
        return cast(ObjectLocation, self.referent).properties
    @property
    def referent(self) -> Location:
        uri = cast(str, self.schema.ref)
        assert uri.startswith("#"), f"Invalid $ref in {self.schema}"
        _, _, ref_to_name = uri.partition("#")
        return self.anchors[ref_to_name]
    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        return self.referent.value(instance, offset)
        
    def raw(self, instance: NDInstance, offset: int = 0 ) -> Any:
        return self.referent.raw(instance, offset)
    def dump_iter(self, nav: "NDNav", indent: int = 0) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        An Iterator over tuples of (indent, Location, array indices, raw bytes, value)
        These are silenced -- they were already displayed in an earlier OneOf.
        """
        return
        # Makes this a generator function even though it yields nothing.
        # Would it be better to raise StopIteration? I doubt it.
        yield  # pragma: no cover

class LocationMaker:
    """
    Creates ``Location`` objects to find sub-instances in a non-delimited ``NDInstance``.

    A ``LocationMaker`` walks through a ``Schema`` structure applied to a ``NDInstance`` to
    emit ``Location`` objects. This is based on the current values in the `NDInstance`,
    to support providing a properly-computed value for ``OCCURS DEPENDING ON`` arrays.

    This is based on an `NDUnpacker` definition of the physical format of the file.
    It's only used for non-delimited files where the underlying `NDInstance` is `Union[bytes, str]`.

    This creates ``NDNav`` isntances for navigation through Non-Delimited instances.

    The algorithm is a post-order traversal of the subschema to build `Location` instances
    that contain references to their children.
    """
    def __init__(self, unpacker: Unpacker[NDInstance], schema: Schema) -> None:
        self.unpacker = unpacker
        self.schema = schema
        self.anchors: dict[str, Location] = {}
        self.instance: NDInstance

    def from_instance(self, instance: NDInstance, start: int = 0) -> Location:
        self.instance = instance
        return self.walk(self.schema, start)
    
    def from_schema(self, start: int = 0) -> Location:
        """May raise an exception if there is an ``OCCURS DEPENDING ON``."""
        return self.walk(self.schema, start)

    def walk(self, schema: Schema, start: int) -> Location:
        """
        ..  note: This can't easily be refactored into the :py:class:`Schema` class hierarchy.

            Doing so has the unfortunate consequence of binding schema with Location.
            We don't really want that.

            The composition choice is to attach a **Strategy** object to each  :py:class:`Schema` object
            that can emit an appropriate :py:class:`Location` object for that  :py:class:`Schema` subclass.
        """
        loc: Location
        if isinstance(schema, AtomicSchema):
            size = self.size(schema)
            loc = AtomicLocation(schema, start, start+size)
        elif isinstance(schema, DependsOnArraySchema):
            if not hasattr(self, "instance"):
                raise ValueError("an Occurs Depending On requires an instance")
            # Find value of maxItems and minItems references in self.anchors.
            max_ref_uri = schema.max_ref
            _, _, max_ref_name = max_ref_uri.partition("#")
            maxItems = int(self.anchors[max_ref_name].value(self.instance))
            # Compute the locations based on the number of items.
            sublocation = self.walk(schema.items, start)
            sublocation.unpacker = self.unpacker
            sublocation.locationMaker = weakref.ref(self)
            item_size = sublocation.size
            total_size = item_size * maxItems
            loc = ArrayLocation(schema, item_size, maxItems, sublocation, start, start+total_size)
        elif isinstance(schema, ArraySchema):    
            assert "maxItems" in cast(dict[str, Any], schema.attributes) or "minItems" in cast(dict[str, Any], schema.attributes)
            maxItems = int(cast(dict[str, Any], schema.attributes).get("maxItems", cast(dict[str, Any], schema.attributes).get("minItems", 0)))
            # Compute the locations based on the number of items.
            sublocation = self.walk(schema.items, start)
            sublocation.unpacker = self.unpacker
            sublocation.locationMaker = weakref.ref(self)
            item_size = sublocation.size
            total_size = item_size * maxItems
            loc = ArrayLocation(schema, item_size, maxItems, sublocation, start, start+total_size)
        elif isinstance(schema, ObjectSchema):
            offset = start
            property_locations: dict[str, Location] = {}
            for name, property_schema in schema.properties.items():
                # Recursive walk to get size of property_schema.
                # Note that any RefTo must NOT be dereferenced. 
                # The attributes property deferences, the private _attributes variable doesn't
                prop_loc = self.walk(property_schema, offset)
                prop_loc.unpacker = self.unpacker
                prop_loc.locationMaker = weakref.ref(self)
                property_locations[name] = prop_loc
                if anchor_name := property_schema._attributes.get("$anchor"):
                    self.anchors[anchor_name] = prop_loc
                size = prop_loc.size
                offset += size
            loc = ObjectLocation(schema, property_locations, start, offset)
        elif isinstance(schema, OneOfSchema):
            # Redefines alternatives defined here.
            # All the alternatives *should* be the same size.
            alt_locs = [self.walk(alternative_schema, start) for alternative_schema in schema.alternatives]
            for a in alt_locs:
                a.unpacker = self.unpacker
                a.locationMaker = weakref.ref(self)
            offset = start + max(loc.size for loc in alt_locs)
            loc = OneOfLocation(schema, alt_locs, start, offset)
        elif isinstance(schema, RefToSchema):
            # Redefines a name as a reference to an alternative in a oneOf.
            # This is a reference to an $anchor within the existing schema.
            # This should *not* be a forward reference, since this is used for Redefines and OccursDependingOn.
            logger.debug(f"$ref to {schema.ref=}")
            # Size is self.anchors[schema.ref].size
            offset = 0  # Only a reference, no actual size allocated here. 
            loc = RefToLocation(schema, self.anchors, start, offset)
        else:
            # The subclasses of Schema aren't reflected in this IF-statement
            raise DesignError(f"Invalid Schema construct: {schema}")  # pragma: no cover
        loc.unpacker = self.unpacker
        loc.locationMaker = weakref.ref(self)
        if anchor_name := loc.schema._attributes.get("$anchor"):
            self.anchors[anchor_name] = loc
        return loc
    
    def size(self, schema: Schema) -> int:
        return self.unpacker.calcsize(schema)
    
    def ndnav(self, instance: "NDInstance") -> "NDNav":
        location = self.from_instance(instance)
        return NDNav(self.unpacker, location, instance)


"""
Structure Navigation
====================

This is the core abstraction for a `Row` of a `Sheet`. Or a document in an JSON-Newline file. Or a row in a CSV file or other workbook. It's one document in an interable YAML file. (While there's no trivial mapping to TOML files, a subclass can locate sections or objects within a section that are treated as rows.)

A `Row` is a collection of named values. A `Schema` provides name and type information for unpacking the values. In the case of non-delimited file formats, navigation becomes a complex problem and `Location` objects are created. With COBOL `REDEFINES` and `OCCURS DEPENDING ON` clauses, fields are found in positions unique to each `NDInstance`.

The names for attributes should be provided as `"$anchor"` values to make them visible. In the case of simple workbook files, the rows are flat and property names are a useful surrogate for anchors.

A `Row` has a plug-in strategy for navigation among cells in the workbook or fields in a JSON object, or the more complex structures present in a Non-Delimited File.

The abstract `Nav` class provides unifieid navigation for delimited as well as non-delimited rows. The `NDNav` subclass handles non-delimited files where `Location` objects are required. The `DNav` handles JSON and other delimited structures. The `WBNav` subclass wraps workbook modules.

An `NDNav` instance provides a context that can help to move through an `NDInstance` of non-delimited data using a `Schema`.  These are created by a `LocationMaker` instance because this navigation so intimately involved in creating `Location` objects to find structures.

A separate `DNav` subclass is a context that navigates through delimited objects where the object structure matches the schema structure. In the case of JSON/YAML/TOML, the operations are trivially delegated to the underlying native Python object; it's already been unpacked.

A third `WBNav` subclass handles CSV, XML and Workbook files. These rely on an underlying unpacker to handle the details of navigation, which are specific to a parser. The `WBNav` is a **Facade** over these various kinds of parsers.

All of these are plug-in strategies used by a `Row` that provides a uniform wrapper.


..  code-block::
    
    @startuml
        abstract class Unpacker
        
        abstract class Row {
            name(string): Nav
            index(int): Nav
        }
        
        Row ..> Unpacker
        abstract class NDInstance {
            name(string): NDNav
            index(int): NDNav
        }
        
        Unpacker --> Nav : creates
        
        abstract class Nav
            
        class NDNav {
            name(string): NDNav
            index(int): NDNav
            value(schema): Any
        }
        
        Nav <|-- NDNav
        Row --> NDNav
        NDNav --> NDInstance
        
        class Location
        
        NDNav --> Location
        class DNav {
            name(string): DNav
            value(schema): Any
        }
        
        Nav <|-- DNav
        Row --> DNav
        DNav --> JSON
        
        class WBNav {
            name(string): DNav
            value(schema): Any        
        }
        
        Row --> WBNav
        Nav <|-- WBNav
    @enduml

We could try to create a subclass of ``dict`` that added methods to support ``Nav`` and ``DNav`` behaviors. 
This seems a bit complicated, since we're generally dealing with a :py:class:`Row`.
This class creates an appropriate :py:class:`NDInstance` or :py:class:`WBInstance` based in the
Workbook's :py:class:`Unpacker` subclass.
 
A separate plug-in **Strategy** acts as an **Adapter** over the distinct implementation details.
"""


class Nav(Protocol):
    """
    Navigation into items by field name or index into an array.

    - For Non-Delimited, names as well as indices are required for object and array navigation.

    - For Delimited, a name or an index can be used, depending on what the underlying Python
      Instance object is. Dictionaries use names, lists use indices.

    - For Workbook, an index is used because we only know the cells of a row by position.

    A ``Nav`` is built by an ``Unpacker``.
    ``unpacker.nav(schema, instance)``.
    """
    def name(self, name: str) -> "Nav": ...

    def index(self, index: int) -> "Nav": ...

    def value(self) -> Any: ...

    def dump(self) -> None: ...


class NDNav(Nav):
    """
    Navigate through an ``NDInstance`` using ``Location`` as a helper.
    """
    def __init__(self, unpacker: Unpacker[NDInstance], location: Location, instance: NDInstance) -> None:
        self.unpacker = weakref.ref(unpacker)
        self.location = location
        self.instance = instance

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.location!r}, {self.instance!r})"

    @property
    def schema(self) -> Schema:
        return self.location.schema

    def name(self, name: str) -> "NDNav":
        """
        Locate the "$anchor" in the ``Schema`` and the related ``Location``.
        Return a new ``NDNav`` for the requested anchor or property name.
        """
        if self.schema.type != "object":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type 'object'")

        # TODO: Find $anchors first, if no matching anchor, then try a property name lookup.

        # Two kinds of Locations -- Locations vs. RefToLocation.
        # For ObjectLocation (and all others) referent == self
        sublocation = cast(ObjectLocation, self.location).properties[name].referent

        logger.debug(f"{self.__class__.__name__}.name({name})")
        logger.debug(locals())
        
        return NDNav(cast(Unpacker[NDInstance], self.unpacker()), sublocation, self.instance)
    
    def index(self, index: int) -> "NDNav":
        """
        Compute an offset into the array of items.
        Create a special ``Location`` for the requested index value.
        The location, attribute, assigned to ``base_location``, is for index == 0.
        """
        if self.schema.type != "array":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type array")

        subschema = cast(ArraySchema, self.schema).items

        base_location = cast(ArrayLocation, self.location)
        if index >= base_location.item_count:
            raise IndexError
        item_offset = base_location.item_size * index
        item_location = (
            LocationMaker(
                cast(Unpacker[NDInstance], self.unpacker()), subschema)
            .from_instance(self.instance, start=base_location.start+item_offset)
        )

        logger.debug(f"{self.__class__.__name__}.index({index})")
        logger.debug(locals())
        
        return NDNav(cast(Unpacker[NDInstance], self.unpacker()), item_location, self.instance)
    
    def value(self) -> Any:
        """The final Python value from the current schema and location."""
        return self.location.value(self.instance)
    
    def raw(self) -> Any:
        """Raw bytes (or text) from the current schema and location."""
        return self.instance[self.location.start: self.location.end]
    
    def raw_instance(self) -> "NDInstance":
        """
        Clone a piece of this instance as a new :py:class:`NDInstance` object.
        Since NDInstance is Union[BytesInstance, TextInstance], there are two paths:
        a new bytes or a new str.
        """
        unpacker: NonDelimited = cast(NonDelimited, self.unpacker())
        cls: Type[NDInstance] = self.instance.__class__
        return cls(self.raw())

    def dump(self) -> None:
        """
        Navigates a non-delimited Schema using :py:class:`Location` (based on the Schema)
        to expose values in the instance.
        This is similar to the way DNav and WBNav objects work.
        """
        layout = "{:45s} {:3d} {:3d} {!r} {!r}"
        print("{:45s} {:3s} {:3s} {!s} {!s}".format("Field", "Off", "Sz", "Raw", "Value"))
        for indent, loc, indices, raw, value in self.location.dump_iter(self):
            print(
                layout.format(
                    indent*'  '+cast(dict[str, Any], loc.schema.attributes).get("cobol", ""),
                    loc.start,
                    loc.size,
                    "" if raw is None else raw,
                    "" if value is None else value)
            )

class DNav(Nav):
    """
    Navigate through a ``DInstance`` using a ``Schema``.
    This is a wrapper around a ``JSON`` document.

    Note that these objects have an inherent ambiguity. 
    A JSON document can have the form of a dictionary with names and values.
    The schema *also* names the properties and suggests types.
    If the two don't agree, that's an instance error, spotted by schema validation.

    The JSON/YAML/TOML parsers have types implied by syntax and the schema *also* has types.
    
    We need an option to validate the instance against the schema.
    """
    def __init__(self, unpacker: Unpacker[DInstance], schema: Schema, instance: JSON) -> None:
        self.unpacker = weakref.ref(unpacker)
        self.schema = schema
        self.instance = instance
        
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.schema!r}, {self.instance!r})"

    def name(self, name: str) -> "DNav":
        # TODO Find $anchors, fall back to property names
        if self.schema.type != "object":
            raise TypeError(f"{self.schema!r} is not of type 'object'")
        subschema = cast(ObjectSchema, self.schema).properties[name]
        value = cast(dict[str, JSON], self.instance)[name]
        return DNav(
            cast(Unpacker[DInstance], self.unpacker()), subschema, value
        )

    def index(self, index: int) -> "DNav":
        if self.schema.type != "array":
            raise TypeError(f"{self.schema!r} is not of type 'array'")
        subschema = cast(ArraySchema, self.schema).items
        value = cast(list[JSON], self.instance)[index]
        return DNav(
            cast(Unpacker[DInstance], self.unpacker()), subschema, value
        )

    def value(self) -> Any:
        """
        The final Python value from the current schema.
        Consider refactoring to use Unpacker explicitly
        """
        return self.instance

    def dump(self) -> None:
        """Navigates a Delimited instance schema and values."""
        layout = "{:53s} {!r}"
        print("{:53s} {!s}".format("Field", "Value"))
        for indent, schema, indices, value in self.schema.dump_iter(self):
            print(
                layout.format(
                    indent*'  '+self.schema.type,
                    "" if value is None else value)
            )

class WBNav(Nav):
    """
    Navigate through a workbook ``WBInstance`` using a ``Schema``.
    
    A Workbook Row is a ``Sequence[Any]`` of cell values. Therefore, navigation
    by name translates to a position within the ``WBInstance`` row.
    """
    def __init__(self, unpacker: Unpacker[WBInstance], schema: Schema, instance: "WBInstance") -> None:
        self.unpacker = weakref.ref(unpacker)
        self.schema = schema
        self.instance = instance

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.schema!r}, {self.instance!r})"

    def name(self, name: str) -> "WBNav":
        if self.schema.type != "object":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type 'object'")
        # TODO Find $anchors, fall back to property names
        subschema = cast(ObjectSchema, self.schema).properties[name]
        if "position" in cast(dict[str, Any], subschema.attributes):
            position = cast(dict[str, Any], subschema.attributes)["position"]
        else:
            position = list(cast(ObjectSchema, self.schema).properties.keys()).index(name)
        return WBNav(cast(Unpacker[WBInstance], self.unpacker()), subschema, self.instance[position])

    def index(self, index: int) -> "WBNav":
        if self.schema.type != "array":
            raise TypeError(f"{self.schema!r} is not of type 'array'")
        subschema = cast(ArraySchema, self.schema).items
        return WBNav(cast(Unpacker[WBInstance], self.unpacker()), subschema, self.instance[index])

    def value(self) -> Any:
        """The final Python value from the current schema -- relies on the Unpacker."""
        return self.instance

    def dump(self) -> None:
        """Navigates a Workbook instance schema and values."""
        layout = "{:53s} {}"
        print("{:53s} {!s}".format("Field", "Value"))
        for indent, schema, indices, value in self.schema.dump_iter(self):
            attrs = cast(dict[str, Any], schema.attributes)
            name = attrs.get("$anchor", attrs.get("title", schema.type))
            print(
                layout.format(
                    indent*'  '+name,
                    "" if value is None else repr(value)
                ).rstrip()
            )

    
class CSVNav(WBNav):
    pass

### Instance Implementations

"""
Instances
=========

For bytes and strings, we provide wrapper :py:class:`Instance` definitions.
These :py:class:`BytesInstance` and :py:class:`TextInstance`
are used by :py:class:`NDNav` and :py:class:`Location` objects.

For :py:class:`DInstance` and :py:class:`WBInstance`, however, we don't 
really need any additional features. We can use native ``JSON`` or ``list[Any]``
objects.
"""

class BytesInstance(bytes):
    """
    Fulfills the protocol for an ``NDInstance``, useful for ``EBCDIC`` and ``StructUnpacker``
    Unpackers.

    To create an :py:class:`NDNav`, this object requires two things:
    - A `Schema` used to create `Location` objects.
    - An `NonDelimited` subclass of `Unpacker` to provide physical format details like size and unpacking.
    
    >>> schema = SchemaMaker.from_json({"type": "object", "properties": {"field-1": {"type": "string", "cobol": "PIC X(12)"}}})
    >>> unpacker = EBCDIC()
    >>> data = BytesInstance('blahblahblah'.encode("CP037"))
    >>> unpacker.nav(schema, data).name("field-1").value()
    'blahblahblah'
    
    The ``Sheet.row_iter()`` build ``Row`` objects that wrap an unpacker, schema, and instance.
    """
    pass

class TextInstance(str):
    """
    Fulfills the protocol for an ``NDInstance``. Useful for ``TextUnpacker``.

    To create an :py:class:`NDNav`, this object requires two things:
    - A `Schema` which populates the `Location` objects.
    - An `NonDelimited` subclass of `Unpacker` to provide physical format details like size and unpacking.

    >>> schema = SchemaMaker.from_json({"type": "object", "properties": {"field-1": {"type": "string", "cobol": "PIC X(12)"}}})
    >>> unpacker = TextUnpacker()
    >>> data = TextInstance('blahblahblah')
    >>> unpacker.nav(schema, data).name("field-1").value()
    'blahblahblah'

    The ``Sheet.row_iter()`` build ``Row`` objects that wrap an unpacker, schema, and instance.
    """
    pass
