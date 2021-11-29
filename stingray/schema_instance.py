"""
schema_instance --  Schema and Navigation models

This module defines a number of foundational
class hierarchies:

-   The :py:class:`Schema` structure.
    The concept is to represent any schema
    as JSON Schema. From there, **Stingray Reader**
    can work with it.
    The JSON Schema can be used to provide validation of an instance.

-   The :py:class:`Instance` hierarchy to support
    individual "rows" or "records" of a physical
    format.
    For delimited files (JSON, YAML, TOML, and XML) this is a native object.
    For non-delimited files, typified by COBOL, this is a ``bytes`` or ``str``.
    For workbook files, this is a ``list[Any]``.

-   The :py:class:`Unpacker` hierarchy to support
    Unpacking values from bytes, EBCDIC bytes,
    strings, navtive Python objects, and workbooks.
    In the case of COBOL, the unpacking must be done lazily to properly handle ``REDEFINES`` and ``OCCURS DEPENDING ON`` features.

-   A :py:class:`Nav` hierarchy to handle navigation
    through a schema and an instance. This class
    allows the :py:class:`Schema` objects to
    be essentially immutable and relatively abstract.
    All additional details are handled here.

-   A :py:class:`Location` hierarchy specifically
    to work with Non-Delimited objects represented
    as ``bytes`` or ``str`` instances.

"""

import abc
import csv
from decimal import Decimal
from functools import partial
import logging
from pathlib import Path
from pprint import pprint
import sys
from typing import (
    Union,
    Any,
    NamedTuple,
    Protocol,
    TextIO,
    BinaryIO,
    Optional,
    overload,
    Iterator,
    cast,
    Type,
    Callable,
    TypeVar,
    Generic,
    AnyStr,
    Sequence,
    SupportsInt,
    IO,
)
import weakref

from stingray import estruct

logger = logging.getLogger("stingray.schema_instance")


class DesignError(BaseException):
    """
    This is a catastrophic design problem.
    A common root cause is a named REGEX capture clause that's not properly
    handled by a class, method, or function.
    """
    pass


JSON = Union[None, bool, int, float, str, list[Any], dict[str, Any]]


class Reference(Protocol):
    """
    # TODO: Formalize a ref_to protocol, used by ``RefToSchema`` and ``DependsOnArraySchema``.
    """

    pass


# TODO: Refactor schema_instance module into two pieces:
#  - Schema, Unpacker, and Nav superclasses -> stingray.schema
#  - Location and Nav subclasses -> stingray.navigation


class Schema:
    """
    Base class for Schema definitions.

    This wraps a JSONSchema definition,
    providing slightly simpler navigation via attribute names
    instead of dictionary keys.
    ``s.type`` instead of ``s['type']``.

    It only works for a few attribute values
    in use here. It's not a general ``__getattribute__`` wrapper.

    Generally, these should be seen as immutable. To permit forward
    references, the RefTo subclass needs to be mutated.
    """

    def __init__(self, attributes: JSON) -> None:
        """
        Builds a schema object.

        :param attributes: All of the JSONSchema attributes for this schema.
        """
        self._attributes = cast(dict[str, Any], attributes)
        # keys for _attributes include "type", "items", "properties", "format", etc.
        self.ref: Optional[str] = None
        self.ref_to: Optional[Schema] = None

    @property
    def type(self) -> str:
        """
        Extract the type attribute value.

        :returns: One of the JSON Schema 'type' values.
        """
        try:
            return cast(str, self._attributes["type"])
        except KeyError as ex:
            logger.error(f"{ex} on {self!r}")
            raise

    @property
    def attributes(self) -> JSON:
        """
        Extract the dictionary of attribute values.

        :returns: dict of keywords from this schema.
        """
        return self._attributes

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes})"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Schema):
            return type(self) == type(other) and self._attributes == other._attributes
        return NotImplemented  # pragma: no cover

    def print(self, indent: int = 0, hide: set[str] = set()) -> None:
        """
        A formatted display of the nested schema.

        :param indent: Indentation level
        :param hide: Attributes to hide because they're contained within this as children
        """
        show_attr = {k: self._attributes[k] for k in self._attributes if k not in hide}
        line = f"{indent*'  '}{self.__class__.__name__}({show_attr})"
        print(line)

    def json(self) -> JSON:
        """Return the attributes as a JSON structure."""
        return self._attributes

    def dump_iter(
        self, nav: Optional["Nav"], indent: int = 0
    ) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """
        Navigate into a schema using a ``Nav`` object to provide unpacker, location and instance context.

        :param nav: The :py:class:`Nav` helper with unpacker, location, and instance details.
        :param indent: Identation for nested display
        :yields: tuple with nesting, schema, indices, and value
        """
        if nav:
            yield indent, self, (), nav.value()


class AtomicSchema(Schema):
    """Schema for an atomic element."""
    pass


class ArraySchema(Schema):
    """Schema for an array definition."""
    def __init__(self, attributes: JSON, items: "Schema") -> None:
        """
        Builds the schema for an array.

        :param attributes: Attributes of this schema definition
        :param items: The value of the 'items' keyword; the subschema.
        """
        super().__init__(attributes)
        self._items = items
        self._maxItems: int = (
            cast(dict[str, Any], attributes).get("maxItems")
            or cast(dict[str, Any], attributes).get("minItems")
            or 1
        )

    @property
    def items(self) -> "Schema":
        """
        Returns the items sub-schema.

        :return: The sub-schema for the items in this array.
        """
        return self._items

    @property
    def maxItems(self) -> int:
        """
        Returns a value for maxItems.
        For simple arrays, this is the maxItems
        value.
        The :py:class:`DependsOnArraySchema`
        subclass will override this.
        """
        return self._maxItems

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.items})"

    def print(self, indent: int = 0, hide: set[str] = set()) -> None:
        """
        A formatted display of the nested schema.

        :param indent: Indentation level
        :param hide: Attributes to hide because they're contained within this as children
        """
        super().print(indent, hide={"items"})
        self._items.print(indent + 1)

    def dump_iter(
        self, nav: Optional["Nav"], indent: int = 0
    ) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """
        Navigate into a schema using a ``Nav`` object to provide unpacker, location and instance context.

        :param nav: The :py:class:`Nav` helper with unpacker, location, and instance details.
        :param indent: Identation for nested display
        :yields: tuple with nesting, schema, indices, and value

        ..  todo:: Enumerate index values from minItems or 0 to maxItems
        """
        yield indent, self, (), None
        if nav:
            yield from self._items.dump_iter(nav.index(0), indent + 1)


class DependsOnArraySchema(ArraySchema):
    """
    Schema for an array with a size that depends on another field.
    An extension vocabulary includes a "maxItemsDependsOn" attribute has a reference to another
    field in this definition.
    """

    def __init__(
        self, attributes: JSON, items: "Schema", ref_to: Optional["Schema"]
    ) -> None:
        """
        Builds an Array where the maxItems depends on another attribute.

        :param attributes: Attributes of this schema definition
        :param items: Subschema for the array items.
        :param ref_to: The Schema with the referred to by the ``maxItemsDependsOn`` keyword.
            Ideally this is a reference to a schema item
            already seen. To permit a forward reference into
            a later "#defs" section, this can be omitted and
            filled in later.
        """
        super().__init__(attributes, items)
        self._items = items
        self.max_ref = self._attributes["maxItemsDependsOn"].get("$ref")
        self.max_ref_to = ref_to

    @property
    def maxItems(self) -> int:
        """
        Returns a value for ``maxItems``.
        For this class, it's a type error -- there is no ``maxItems``
        in the schema.
        A :py:class:`Location` object will have size information,
        """
        raise TypeError(f"maxItems depends on {self.max_ref_to} value of an instance")


class ObjectSchema(Schema):
    """Schema for an object with properties."""
    def __init__(self, attributes: JSON, properties: dict[str, "Schema"]) -> None:
        """
        Build a schema for an object.

        :param attributes: The attributes of this object
        :param properties: The value of the ``properties`` keyword.
        """
        super().__init__(attributes)
        self.properties = properties

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.properties})"

    def print(self, indent: int = 0, hide: set[str] = set()) -> None:
        """
        A formatted display of the nested schema.

        :param indent: Indentation level
        :param hide: Attributes to hide because they're contained within this as children
        """
        super().print(indent, hide={"properties"})
        for prop in self.properties.values():
            prop.print(indent + 1)

    def dump_iter(
        self, nav: Optional["Nav"], indent: int = 0
    ) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """
        Navigate into a schema using a ``Nav`` object to provide unpacker, location and instance context.

        :param nav: The :py:class:`Nav` helper with unpacker, location, and instance details.
        :param indent: Identation for nested display
        :yields: tuple with nesting, schema, indices, and value
        """
        yield indent, self, (), None
        if nav:
            for name, child in self.properties.items():
                yield from child.dump_iter(nav.name(name), indent + 1)


class OneOfSchema(Schema):
    """
    Schema for a "oneOf" definition.
    This is the basis for COBOL ``REDEFINES``.
    """
    def __init__(self, attributes: JSON, alternatives: list["Schema"]) -> None:
        super().__init__(attributes)
        self.alternatives = alternatives

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self._attributes}, {self.alternatives})"

    @property
    def type(self) -> str:
        """
        Returns an imputed type of "oneOf".
        The actual JSON Schema doesn't use
        the "type" keyword for these.

        :return: Literal["oneOf"]
        """
        return "oneOf"

    def print(self, indent: int = 0, hide: set[str] = set()) -> None:
        """
        A formatted display of the nested schema.

        :param indent: Indentation level
        :param hide: Attributes to hide because they're contained within this as children
        """
        super().print(indent, hide={"oneOf"})
        for alt in self.alternatives:
            alt.print(indent + 1)

    def dump_iter(
        self, nav: Optional["Nav"], indent: int = 0
    ) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """
        Navigate into a schema using a ``Nav`` object to provide unpacker, location and instance context.

        :param nav: The :py:class:`Nav` helper with unpacker, location, and instance details.
        :param indent: Identation for nested display
        :yields: tuple with nesting, schema, indices, and value
        """
        yield indent, self, (), None
        if nav:
            for alt in self.alternatives:
                # Not all alternatives will be valid.
                try:
                    yield from alt.dump_iter(nav, indent + 1)
                except (IndexError, KeyError):  # pragma: no cover
                    pass


class RefToSchema(Schema):
    """Must deference type and attributes properties."""

    def __init__(self, attributes: JSON, ref_to: Optional[Schema]) -> None:
        """
        Builds a schema that's a reference to another schema.

        :param attributes: Attributes of this reference. Usually, there are none.
        :param ref_to: The schema to which this refers. This is optional
            and may be filled in later.
        """
        super().__init__(attributes)
        self.ref = self._attributes.get("$ref")
        self.ref_to = ref_to

    def __repr__(self) -> str:
        # This isn't exactly right.
        # To avoid recursion and provide a display value, we show a name here.
        return f"{self.__class__.__name__}({self._attributes}, {self.ref})"

    @property
    def type(self) -> str:
        """Deference the anchor name and return the type."""
        if self.ref_to:
            return self.ref_to.type
        raise ValueError(f"Invalid {self.__class__.__name__}")

    @property
    def attributes(self) -> JSON:
        """Deference the anchor name and return the attributes."""
        if self.ref_to:
            return self.ref_to.attributes
        raise ValueError(f"Invalid {self.__class__.__name__}")

    @property
    def properties(self) -> dict[str, "Schema"]:
        """Deference the anchor name and return properties."""
        if self.ref_to:
            return cast(ObjectSchema, self.ref_to).properties
        raise ValueError(f"Invalid {self.__class__.__name__}")

    @property
    def items(self) -> Schema:
        """Deference the anchor name and return items."""
        if self.ref_to:
            return cast(ArraySchema, self.ref_to).items
        raise ValueError(f"Invalid {self.__class__.__name__}")

    def dump_iter(
        self, nav: Optional["Nav"], indent: int = 0
    ) -> Iterator[tuple[int, "Schema", tuple[int, ...], Optional[Any]]]:
        """
        Navigate into a schema using a ``Nav`` object to provide unpacker, location and instance context.

        :param nav: The :py:class:`Nav` helper with unpacker, location, and instance details.
        :param indent: Identation for nested display
        :yields: tuple with nesting, schema, indices, and value
        """
        yield indent, self, (), None


class SchemaMaker:
    """
    Build a :py:class:`Schema` structure from a JSON Schema document.

    This doesn't do much, but it allows us to use classes to define methods 
    that apply to the JSON Schema constructs instead of referring to them 
    as the source document dictionaries.

    This relies on an ``maxItemsDependsOn`` extension vocabulary to describe ``OCCURS DEPENDING ON``.

    All ``$ref`` names are expected to refer to explicit ``$anchor`` names within this schema.
    Since anchor names may occur at the end, in a ``#def`` section,
    we defer the forward references and tweak the schema objects.
    """

    ATOMIC = {
        "null",
        "boolean",
        "integer",
        "number",
        "string",
    }  # Requires extended meta-schema to add "decimal"

    def __init__(self) -> None:
        """
        Initialise a SchemaMaker instance.
        """
        self.name_cache: dict[str, Schema] = {}
        self.fixup_list: list[Schema] = []

    def walk_schema(self, source: JSON, path: tuple[str, ...] = ()) -> Schema:
        """
        Recursive walk of a JSON Schema document, create :py:class:`Schema` objects
        for each schema and all of the children sub-schema.

        This is not invoked directly. It's used by the :py:meth:`from_json` method.

        Relies on an ``maxItemsDependsOn`` extension to describe ``OCCURS DEPENDING ON``.

        Builds an anchor name cache to resolve "$ref" after an
        initial construction pass.

        :param source: A valid JSONSchema document.
        :param path: The Path to a given property. This starts as an empty tuple. Names are added as properties are processed.
        :return: A :py:class:`Schema` object.
        """
        schema: Schema
        source = cast(dict[str, Any], source)

        if source.get("oneOf"):
            alternatives_schema = [
                self.walk_schema(value, path) for value in source.get("oneOf", [])
            ]
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
                    schema = DependsOnArraySchema(
                        attributes=source, items=items_schema, ref_to=ref_to
                    )
                except KeyError:
                    # Forward references for OCCURS DEPENDS ON are sketchy at best.
                    # If we wanted to resolve them later...
                    # schema = DependsOnArraySchema(attributes=source, items=items_schema, ref_to=None)
                    # self.fixup_list.append(schema)
                    raise ValueError(
                        f"unknown $ref in {source!r}; forward references for maxItemsDependsOn aren't supported"
                    )
            else:
                schema = ArraySchema(attributes=source, items=items_schema)

        elif source["type"] == "object" or "properties" in source:
            properties_schema = {
                name: self.walk_schema(value, path + (name,))
                for name, value in source.get("properties", {}).items()
            }
            schema = ObjectSchema(attributes=source, properties=properties_schema)

        else:
            # Serious Design Error...
            raise ValueError(f"Unknown {source['type']=!r} in {source}")

        self.name_cache[
            source.get("$anchor", source.get("title", "*UNNAMED*"))
        ] = schema
        return schema

    def resolve(self, schema: Schema) -> Schema:
        """
        Resolve forward ``$ref`` references.

        This is not invoked directly, it's used by the :py:meth:`from_json` method.

        :param schema: A ``Schema`` document that requires fixup
            Generally, this **must** be the schema created by :py:meth:`walk`.
            This :py:class:`SchemaMaker` instance has a cache of ``$anchor`` names
            used for resolution.
        :return: A ``Schema`` document after fixing references.
        """
        for fixup in self.fixup_list:
            try:
                ref_uri = fixup._attributes["$ref"]
                assert ref_uri.startswith("#"), f"Invalid {ref_uri}"
                _, _, ref_name = ref_uri.partition("#")
                ref_to = self.name_cache[ref_name]
            except KeyError:
                raise ValueError(
                    f"Cannot resolve {fixup.ref!r} in {list(self.name_cache.keys())}"
                )
            fixup.ref_to = ref_to
        return schema

    @classmethod
    def from_json(cls: Type["SchemaMaker"], source: JSON) -> Schema:
        """
        Build a :py:class:`Schema` from a JSONSchema document.
        This walks the hierarchy and resolves the ``$ref`` references.

        :param source: A JSONSchema document.
        :return: A :py:class:`Schema`.
        """
        sm = cls()
        schema = sm.walk_schema(source)
        sm.resolve(schema)
        return schema

# Unpackers

def digit_string(size: int, value: SupportsInt) -> str:
    """
    Transforms a numeric value from a spreadsheet into a string with leading zeroes.

    This undoes mischief to ZIP codes an SSN's with leading zeroes in a workbook.

    >>> digit_string(5, 1020)
    '01020'

    :param size: target size of the string
    :param value: numeric value
    :return: string with the requested size.
    """
    return (size * "0" + str(value))[-size:]


digits_5 = partial(digit_string, 5)


def decimal_places(digits: int, value: Any) -> Decimal:
    """
    Quantizes a :py:class:`Decimal` value to the requested precision.

    This undoes mischief to currency values in a workbook.

    >>> decimal_places(2, 3.99)
    Decimal('3.99')

    :param digits: number of digits of precision.
    :param value: a numeric value.
    :return: a :py:class:`Decimal` value, quantized to the requested number of decimal places.
    """
    digits_right = Decimal(f"0.{(digits-1)*'0'}1")
    return Decimal(value).quantize(digits_right)


decimal_2 = partial(decimal_places, 2)

#: Conversion functions that can be provided
#: in a schema using the extension ``conversion``
#: keyword.
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

    def __init__(self, source: AnyStr) -> None: ...

    @overload
    def __getitem__(self, index: int) -> Union[str, int]: ...

    @overload
    def __getitem__(self, index: slice) -> Union[str, bytes]: ...


class DInstance(Protocol):
    """
    JSON/YAML/TOML documents are wild and free.
    Pragmatically, we want o supplement these classes with methods
    that emit ``DNav`` objects to manage navigating an object and a schema in parallel.

    ..  todo:: Not sure this is useful, it's a kind of alias for Any/object
    """

    def __init__(self, source: JSON) -> None:  ...


class WBInstance(Protocol):
    """
    CSV files are ``list[str]``. All other workbooks tend to be  ``list[Any]``
    because their unpacker modules do conversions.

    We'll tolerate any sequence type.

    ..  todo:: Not sure this is useful, it's a kind of alias for ``list[Any]``
    """

    @overload
    def __getitem__(self, index: int) -> Any: ...

    @overload
    def __getitem__(self, index: slice) -> Any: ...


# We'll define an Instance type is the union of the protocols for
# non-delimited instances, workbook instances, and Python native ("Non-delimited" or "JSON")
# objects.
Instance = TypeVar("Instance", NDInstance, DInstance, WBInstance)


class Mode:
    """Two handy constants used to by Unpackers to open files."""
    #: Text mode file open
    TEXT = "r"
    #: Binary mode file open
    BINARY = "rb"


class Unpacker(Generic[Instance]):
    """
    An Unpacker helps convert data from an ``Instance``.
    For NDInstances, this involves size calculations and value conversions.
    For WBInstances and JSON, this is a pass-through because the sizes don't matter and
    the values are already Native Python objects.

    An Unpacker is a generic procotol. A class that implements the protocol **should**
    provide all of the methods.

    It might make sense to define one more method

    ..  method:: instance_iter(self, sheet: str, **kwargs: Any) -> Iterator[Instance]

        Iterates through all the records of a given sheet.

    There doesn't seem to be a way to sensibly defined here.
    There are too many variations on the instance types.
    """

    def calcsize(self, schema: Schema) -> int:  # pragma: no cover
        """
        Compute the size of this schema item.

        :param schema: A schema item to unpack
        :return: The size
        """
        ...

    def value(self, schema: Schema, instance: Instance) -> Any:  # pragma: no cover
        """
        Unpack the value for this schema item.

        :param schema: A schema item to unpack
        :param instance: An instance with a value to unpack
        :return: The Python object
        """
        ...

    def nav(self, schema: Schema, instance: Instance) -> "Nav":  # pragma: no cover
        """
        Creates a :py:class:`Nav` helper to locate items
        within this instance.

        :param schema: Schema to apply.
        :param instance: Instance to navigate into
        :return: A subclass of :py:class:`Nav` appropriate to this unpacker.
        """
        ...

    def open(
        self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None
    ) -> None:  # pragma: no cover
        """
        File open. This is generally delegated to
        a workbook module.

        :param name: :py:class:`Path` to the file.
        :param file_object: Optional IO object in case the file is part of a ZIP archive.
        """
        ...

    def close(self) -> None:  # pragma: no cover
        """
        File close. This is generally delegated to
        a workbook module.
        """
        ...

    def sheet_iter(self) -> Iterator[str]:  # pragma: no cover
        """
        Yields all the names of sheets of a workbook.
        In the case of CSV or NDJSON files
        or COBOL files, there's only one sheet.

        :yields: string sheet names.
        """
        ...

    def used(self, count: int) -> None:  # pragma: no cover
        """
        Provide feedback to the unpacker on how many bytes
        an instance actually uses.

        This is for ``RECFM=N`` kinds of COBOL
        files where there are no RDW headers on
        the records, and the size must be deduced
        from the number of bytes actually used.

        :param count: bytes used.
        """
        ...


class EBCDIC(Unpacker[NDInstance]):
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
        """
        Computes the size of a field.

        :param schema: The field definition.
        :return: The size.
        """
        # assert schema.attributes.get("contentEncoding") in {"packed-decimal", "cp037"}
        format = cast(dict[str, Any], schema.attributes).get("cobol", "USAGE DISPLAY")
        return estruct.calcsize(format)

    def value(self, schema: Schema, instance: NDInstance) -> Any:
        """
        Computes the value of a field in a given :py:class:`NDInstance`.

        :param schema: The field definition.
        :param instance: The instance to unpack.
        :return: The value.
        """
        # assert schema.attributes.get("contentEncoding") in {"packed-decimal", "cp037"}
        format = cast(dict[str, Any], schema.attributes).get("cobol", "USAGE DISPLAY")
        conversion_func = CONVERSION[
            cast(dict[str, Any], schema.attributes).get("conversion")
        ]
        return conversion_func(estruct.unpack(format, cast(bytes, instance)))

    def nav(self, schema: Schema, instance: NDInstance) -> "NDNav":
        """
        Create a :py:class:`NDNav` helper to navigate through an :py:class:`NDInstance`.

        :param schema: The schema for this instance
        :param instance: The instance
        :return: an :py:class:`NDNav` helper.
        """
        location = LocationMaker(
            cast(Unpacker[NDInstance], self), schema
        ).from_instance(instance)
        return NDNav(self, location, instance)

    def open(
        self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None
    ) -> None:
        """
        A file open suitable for unpacking an EBCDiC COBOL file.

        :param name: The :py:class:`Path`
        :param file_object: An open
        """
        if file_object:
            self.the_file = file_object
        else:
            self.the_file = name.open(mode=Mode.BINARY)

    def close(self) -> None:
        """A file close suitable for most COBOL files."""
        if hasattr(self, "the_file") and self.the_file:
            self.the_file.close()
            del self.the_file

    def sheet_iter(self) -> Iterator[str]:
        """
        Yields one name for the 'sheet' in this file.

        :yields: Literal[""]
        """
        yield ""

    def instance_iter(
        self,
        sheet: str,
        recfm_class: Type["estruct.RECFM_Reader"],
        lrecl: int,
        **kwargs: Any,
    ) -> Iterator[NDInstance]:
        """
        Yields all of the record instances in this file.

        Delegates the details of instance iteration to a :py:class:`estruct.RECFM_Reader` instance.

        :param sheet: The name of the sheet to process; for COBOL files, this is ignored.
        :param recfm_class: a subclass of :py:class:`estruct.RECFM_Reader`
        :param lrecl: The expected logical record length of this file. This is used for RECFM without RDW's.
        :param kwargs: Additional args provided to the :py:class:`estruct.RECFM_Reader` instance that's created.
        :yields: :py:class:`NDInstance` for each record in the file.
        """
        self.recfm_parser = recfm_class(cast(BinaryIO, self.the_file), lrecl)
        return cast(Iterator[NDInstance], self.recfm_parser.record_iter())

    def used(self, length: int) -> None:
        """
        This is used by a client application to
        provide the number of bytes actually used.

        This is delegated to the recfm_parser.

        :param length: number of bytes used.
        """
        self.recfm_parser.used(length)


class Struct(Unpacker[NDInstance]):
    """
    Unpacker for Non-Delimited native (i.e., not EBCDIC-encoding) bytes.

    Uses built-in :py:mod:`struct` module for calcsize and value.
    TODO: Finish this.
    """

    pass


class TextUnpacker(Unpacker[NDInstance]):
    """
    Unpacker for Non-Delimited text values.

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
    
    For COBOL, the "cobol" keyword provides USAGE and PICTURE. This defines size.
    In this case, since it's not in EBCDIC, we can use :py:mod:`struct` to unpack COMP values.
    
    This requires the "cobol" and "conversion" keywords, part of the extended vocabulary for COBOL.
    A `"cobol"` keyword gets Usage and Picture values required to decode EBCDIC. 
    A "conversion" keyword converts to a more useful Python type.
        
    (An alternative approach is to use the ``pattern`` attribute to provide length information.
    This is often {"type": "string", "pattern": "^.{64}$"} or similar. This can provide a length.
    Because patterns can be hard to reverse engineer, we don't use this.)
    """

    def calcsize(self, schema: Schema) -> int:
        """
        Computes the size of a field.

        :param schema: The field definition.
        :return: The size.
        """
        if "maxLength" in cast(dict[str, Any], schema.attributes):
            return int(cast(dict[str, Any], schema.attributes)["maxLength"])
        elif "cobol" in cast(dict[str, Any], schema.attributes):
            representation = estruct.Representation.parse(
                cast(dict[str, Any], schema.attributes)["cobol"]
            )
            return representation.picture_size
        else:
            raise ValueError(
                f"can't compute size of {schema}; neither maxLength nor cobol provided"
            )

    def value(self, schema: Schema, instance: NDInstance) -> Any:
        """
        Computes the value of a field in a given :py:class:`NDInstance`.

        :param schema: The field definition.
        :param instance: The instance to unpack.
        :return: The value.
        """
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(
            cast(dict[str, Any], schema.attributes).get("conversion", type_value),
            lambda x: x,
        )
        return conversion(instance)

    def nav(self, schema: Schema, instance: NDInstance) -> "NDNav":
        """
        Create a :py:class:`NDNav` helper to navigate through an :py:class:`NDInstance`.

        :param schema: The schema for this instance
        :param instance: The instance
        :return: an :py:class:`NDNav` helper.
        """
        location = LocationMaker(
            cast(Unpacker[NDInstance], self), schema
        ).from_instance(instance)
        return NDNav(self, location, instance)

    def open(
        self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None
    ) -> None:
        """
        A file open suitable for unpacking a Text COBOL file.

        :param name: The :py:class:`Path`
        :param file_object: An open
        """
        if file_object:
            self.the_file = cast(IO[str], file_object)
        else:
            self.the_file = name.open(mode=Mode.TEXT)

    def close(self) -> None:
        """A file close suitable for most COBOL files."""
        if hasattr(self, "the_file") and self.the_file:
            self.the_file.close()
            del self.the_file

    def sheet_iter(self) -> Iterator[str]:
        """
        Yields one name for the 'sheet' in this file.

        :yields: Literal[""]
        """
        yield ""

    def instance_iter(self, sheet: str, **kwargs: Any) -> Iterator[NDInstance]:
        """
        Yields all of the record instances in this file.

        :param sheet: The name of the sheet to process; for COBOL files, this is ignored.
        :param kwargs: Not used.
        :yields: Instances of rows. Text files are newline delimited.
        """
        self.row_source = cast(Iterator[NDInstance], iter(self.the_file))
        return self.row_source


class Delimited(Unpacker[DInstance]):
    """
    An Abstract Unpacker for delimited instances, i.e. ``JSON`` documents.

    An instance will be ``list[Any] | dict[str, Any] | Any``.
    It will is built by a separate parser, often :py:mod:`json`, YAML, or TOML.

    For JSON/YAML/TOML, the instance *should* have the same structure as the schema.
    JSONSchema validation can be applied to confirm this.

    For XML, the source instance should be transformed into native Python objects, following
    a schema definition. A schema structure may ignore XML tags or extract text from a tag
    with a mixed content model.

    The sizes and formats of delimited data don't matter:
    the :py:func:`calcsize` function returns 1 to act as a position in a sequence of values.

    Concrete subclasses include open, close, and instance_iter.
    """

    def calcsize(self, schema: Schema) -> int:
        """
        Computes the size of a field.
        For delimited files, this isn't relevant.

        :param schema: The field definition.
        :return: Literal[1].
        """
        return 1

    def value(self, schema: Schema, instance: "DInstance") -> Any:
        """
        Computes the value of a field in a given :py:class:`DInstance`.
        The underlying parser for delimited data has already
        created Python objects.

        If the ``conversion`` keyword was used in
        the schema, this conversion function is applied.

        :param schema: The schema
        :param instance: The instance
        :return: The instance
        """
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(
            cast(dict[str, Any], schema.attributes).get("conversion", type_value),
            lambda x: x,
        )
        return conversion(instance)

    def nav(self, schema: Schema, instance: DInstance) -> "Nav":
        """
        Create a :py:class:`DNav` helper to navigate through an :py:class:`DInstance`.

        :param schema: The schema for this instance
        :param instance: The instance
        :return: an :py:class:`DNav` helper.
        """
        return DNav(self, schema, cast(JSON, instance))


class WBUnpacker(Unpacker[WBInstance]):
    """
    Unpacker for Workbook-defined values.

    Most of WBInstances defer to another module for unpacking.
    CSV, however, relies on the :py:mod:`csv` module, where the instance is ``list[str]``.
    
    While it's tempting to use "type": "number" on CSV data, it's technically suspicious.
    The file has strings, and only strings. Conversions are part of the application's use 
    of the data, not the data itself. The schema can use the ``"conversion"`` keyword
    to specify one of the conversion functions.
    """

    def calcsize(self, schema: Schema) -> int:
        """
        Computes the size of a field.
        For delimited files, this isn't relevant.

        :param schema: The field definition.
        :return: Literal[1].
        """
        return 1

    def value(self, schema: Schema, instance: WBInstance) -> Any:
        """
        Computes the value of a field in a given :py:class:`DInstance`.

        The underlying parser for the workbook has already
        created Python objects. We apply a final conversion
        to get from a workbook object to a more useful
        Python object.

        The schema voculary extension "conversion"
        is used to locate a suitable conversion function.

        :param schema: The schema
        :param instance: The instance
        :return: An instance with the conversion applied.
        """
        type_value = cast(dict[str, Any], schema.attributes).get("type", "object")
        conversion = CONVERSION.get(
            cast(dict[str, Any], schema.attributes).get("conversion", type_value),
            lambda x: x,
        )
        return conversion(instance)

    def nav(self, schema: Schema, instance: WBInstance) -> "Nav":
        """
        Create a :py:class:`WBNav` helper to navigate through an :py:class:`WBInstance`.

        :param schema: The schema for this instance
        :param instance: The instance
        :return: an :py:class:`WBNav` helper.
        """
        return WBNav(self, schema, instance)

# Locations


class Location(abc.ABC):
    """
    A Location is used to navigate within an :py:class:`NDInstance` objects.

    These are created by a :py:class:`NDNav` instance.

    The ``Unpacker[NDInstance]`` strategy is a subclass of NonDelimited,
    one of EBCDIC(), Struct(), or TextUnpacker().

    The value() method delegates the work to the :py:class:`Unpacker` strategy.

    TODO: Add unpacker and locationMaker to __init__() method.
    """

    def __init__(self, schema: Schema, start: int, end: int = 0) -> None:
        """
        Create a new Location within an Insance.

        :param schema: The schema for this location.
        :param start: The offset into the underlying sequence.
        :param end: The end of the underlying sequence.
        """
        self.schema = schema
        self.start = start
        if end:
            self.end = end
            self.size = end - start
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
    def raw(self, instance: NDInstance, offset: int = 0) -> Any:  # pragma: no cover
        """The raw bytes of this location."""
        ...

    @abc.abstractmethod
    def value(self, instance: NDInstance, offset: int = 0) -> Any:  # pragma: no cover
        """The value of this location."""
        ...

    @property
    def referent(self) -> "Location":
        """Most things refer to themselves. A RefToLocation, however, overrides this."""
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
    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, "Location", tuple[int, ...], Optional[bytes], Any]]:  # pragma: no cover
        """
        Dump this location and all children in the schema.

        :yields: tuples of (indent, Location, array indices, raw bytes, value)
        """
        ...


class AtomicLocation(Location):
    """
    The location of a COBOL elementary item.

    type(Schema) == AtomicSchema.
    """

    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        For an atomic value, locate the underlying value. This may involve unpacking.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The Python object unpacked from this location
        """
        logger.debug(
            f"{self}, {instance}, {instance[self.start+offset: self.end+offset]!r}"
        )
        return self.unpacker.value(
            self.schema,
            cast(NDInstance, instance[self.start + offset : self.end + offset]),
        )

    def raw(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the bytes of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The raw bytes from this location
        """
        return instance[self.start + offset : self.end + offset]

    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        Dump this atomic location.

        :param nav: The parent :py:class:`NDNav` instance with schema details.
        :param indent: The indentation level
        :yields: tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), nav.raw(), nav.value()


class ArrayLocation(Location):
    """
    The location of an array of instances with the same schema. A COBOL ``OCCURS`` item.

    type(Schema) == ArraySchema.
    """

    def __init__(
        self,
        schema: Schema,
        item_size: int,
        item_count: int,
        items: "Location",
        start: int,
        end: int,
    ) -> None:
        """
        Creates an Array Location within an Instance

        :param schema: The schema for this location.
        :param item_size: The calculated size of each item in the arrary
        :param item_count: The number of items in the array
        :param items: The :py:class:`Location` of the first item.
        :param start: The offset into the underlying sequence.
        :param end: The end of the underlying sequence.
        """
        super().__init__(schema, start, end)
        self.item_size = item_size
        self.item_count = item_count
        self.items = items

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.schema}, {self.item_size}, {self.item_count}, {self.items}, {self.start}, {self.end})"

    def __str__(self) -> str:
        if "cobol" in cast(dict[str, Any], self.schema.attributes):
            return f"{cast(dict[str, Any], self.schema.attributes)['cobol']} {self.start} {self.end}"
        return f"Array [{self.item_count}] {self.start} {self.end}"

    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the value of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The Python object unpacked from this location
        """
        array_value = [
            self.items.value(instance, offset=offset + i * self.item_size)
            for i in range(self.item_count)
        ]
        return array_value

    def raw(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the bytes of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: instance bytes (or characters if it's a text instance.)
        """
        return instance[self.start + offset : self.end + offset]

    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        Dump the first item of this array location.

        :param nav: The parent :py:class:`NDNav` instance with schema details.
        :param indent: The indentation level
        :yields: tuples of (indent, Location, array indices, raw bytes, value)

        ..  todo: Step through array index combinations.

            For now, provide the index=0 value only.
        """
        yield indent, self, (), None, None
        yield from self.items.dump_iter(nav.index(0), indent + 1)


class ObjectLocation(Location):
    """
    The location of an object with a dictionary of named properties. A COBOL group-level item.

    type(Schema) == ObjectSchema.
    """

    def __init__(
        self, schema: Schema, properties: dict[str, "Location"], start: int, end: int
    ) -> None:
        """
        Creates an Object Location within an Instance.

        :param schema: The schema for this location.
        :param properties: The dictionary of subsidary :py:class:`Location` for the children of this location.
        :param start: The offset into the underlying sequence.
        :param end: The end of the underlying sequence.
        """
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
        """
        Return the value of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The Python object unpacked from this location
        """
        object_value = {
            name: self.properties[name].value(instance, offset=offset)
            for name in cast(ObjectSchema, self.schema).properties
        }
        return object_value

    def raw(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the bytes of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: instance bytes (or characters if it's a text instance.)
        """
        return instance[self.start + offset : self.end + offset]

    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        Dump this object location and all of the properties within it.

        :param nav: The parent :py:class:`NDNav` instance with schema details.
        :param indent: The indentation level
        :yields: tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), None, None
        for name, child in self.properties.items():
            yield from child.dump_iter(nav.name(name), indent + 1)


class OneOfLocation(Location):
    """
    The location of an object which has a list of REDEFINES alternatives.

    type(Schema) == OneOfSchema.
    """

    def __init__(
        self, schema: Schema, alternatives: list[Location], start: int, end: int
    ) -> None:
        """
        Creates an Redefined Location within an Instance.

        :param schema: The schema for this location.
        :param alternatives: List of alternative :py:class:`Location` definitions.
        :param start: The offset into the underlying sequence.
        :param end: The end of the underlying sequence.
        """
        super().__init__(schema, start, end)
        self.alternatives = {
            alt.schema._attributes.get(
                "$anchor", alt.schema._attributes.get("title", "UNNAMED")
            ): alt
            for alt in alternatives
        }

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.schema}, {list(self.alternatives.values())!r}, {self.start}, {self.end})"

    def __str__(self) -> str:
        return f"OneOf [{', '.join(list(self.alternatives.keys()))}] {self.start} {self.end}"

    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the value of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The Python object unpacked from this location
        """
        first, *others = self.alternatives.values()
        return first.value(instance, offset)

    def raw(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Return the bytes of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: instance bytes (or characters if it's a text instance.)
        """
        return instance[self.start + offset : self.end + offset]

    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        Dump this object location and all of the alternative definitions.
        Since some of these may raise exceptions, displays may be incomplete.

        :param nav: The parent :py:class:`NDNav` instance with schema details.
        :param indent: The indentation level
        :yields: tuples of (indent, Location, array indices, raw bytes, value)
        """
        yield indent, self, (), None, None
        for index, alt in enumerate(self.alternatives.values()):
            sub_nav = NDNav(self.unpacker, alt, nav.instance)
            yield from alt.dump_iter(sub_nav, indent + 1)


class RefToLocation(Location):
    """
    Part of REDEFINES; this is the COBOL-visible name of a path into a ``OneOfLocation`` alternative.

    type(Schema) == RefToSchema.

    This could also be part of ``OCCURS DEPENDING ON``.
    If used like this, it would refer to the COBOL-visible name of an item with an array size.
    The ``OCCURS DEPENDING ON`` doesn't formalize this, however.
    """

    def __init__(
        self, schema: Schema, anchors: dict[str, Location], start: int, end: int
    ) -> None:
        """
        Creates an Refer-To Location within an Instance.

        The mapping used to locate an anchor name
        is expected to be updated as Locations are
        built for a given record. This allows lazy
        dereferencing, permitting
        forward references. The anchors mapping
        will never be copied.

        :param schema: The schema for this location.
        :param anchors: A mapping from name to :py:class:`Location` definitions.
        :param start: The offset into the underlying sequence.
        :param end: The end of the underlying sequence.
        """
        super().__init__(schema, start, end)
        self.anchors = anchors

    def __str__(self) -> str:
        return f"RefTo {self.schema.ref} {self.start} {self.end}"

    @property
    def properties(self) -> dict[str, Location]:
        """
        Deference the anchor name and get the properties.

        :return:  properties of the referred-to name.
        """
        return cast(ObjectLocation, self.referent).properties

    @property
    def referent(self) -> Location:
        """
        Deference the anchor name and get the properties.

        :return: The Location referred to.
        """
        uri = cast(str, self.schema.ref)
        assert uri.startswith("#"), f"Invalid $ref in {self.schema}"
        _, _, ref_to_name = uri.partition("#")
        return self.anchors[ref_to_name]

    def value(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Dereference the anchor name and return the value of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: The Python object unpacked from this location
        """
        return self.referent.value(instance, offset)

    def raw(self, instance: NDInstance, offset: int = 0) -> Any:
        """
        Dereference the anchor name and return the bytes of this location.

        :param instance: The Non-Delimited Instance
        :param offset: The offset into the sequence
        :return: instance bytes (or characters if it's a text instance.)
        """
        return self.referent.raw(instance, offset)

    def dump_iter(
        self, nav: "NDNav", indent: int = 0
    ) -> Iterator[tuple[int, Location, tuple[int, ...], Optional[bytes], Any]]:
        """
        These items are silenced -- they were already displayed in an earlier OneOf.
        """
        return
        # Makes this a generator function even though it yields nothing.
        # Would it be better to raise StopIteration? I doubt it.
        yield  # pragma: no cover


class LocationMaker:
    """
    Creates :py:class:`Location` objects to find sub-instances in a non-delimited ``NDInstance``.

    A :py:class:`LocationMaker` walks through a ``Schema`` structure applied to a ``NDInstance`` to
    emit ``Location`` objects. This is based on the current values in the `NDInstance`,
    to support providing a properly-computed value for ``OCCURS DEPENDING ON`` arrays.

    This is based on an :py:class:`NDUnpacker` definition of the physical format of the file.
    It's only used for non-delimited files where the underlying `NDInstance` is `Union[bytes, str]`.

    This creates ``NDNav`` isntances for navigation through Non-Delimited instances.

    The algorithm is a post-order traversal of the subschema to build `Location` instances
    that contain references to their children.
    """

    def __init__(self, unpacker: Unpacker[NDInstance], schema: Schema) -> None:
        """
        Builds a LocationMaker. The LocationMaker
        can then build :py:class:`Location` helpers
        for an Instance, using the given Unpacker and Schema.

        :param unpacker: The Unpacker used to unpock instance data
        :param schema: The schema to describe the instance data
        """
        self.unpacker = unpacker
        self.schema = schema
        self.anchors: dict[str, Location] = {}
        self.instance: NDInstance

    def from_instance(self, instance: NDInstance, start: int = 0) -> Location:
        """
        Builds a :py:class:`Location` from an non-delimited py:class:`NDInstance`.

        This will handle ``OCCURS DEPENDING ON``
        references and dynamically-sized arrays.

        :param instance: The record instance.
        :param start: The initial offset, usually zero.
        :return: a :py:class:`Location` describing this instance.
        """
        self.instance = instance
        return self.walk(self.schema, start)

    def from_schema(self, start: int = 0) -> Location:
        """
        Attempt to build a :py:class:`Location` from a schema.

        This will raise an exception if there is an ``OCCURS DEPENDING ON``.
        For these kinds of DDE's, an instance must be used.

        :param start: The initial offset, usually zero.
        :return: a :py:class:`Location` describing any instance of this schema.
        """
        return self.walk(self.schema, start)

    def walk(self, schema: Schema, start: int) -> Location:
        """
        Recursive descent into a Schema, creating a :py:class:`Location`.
        This is generally used via the :py:meth:`from_instance` method.
        It is not invoked directly.

        :param schema: A schema describing a non-delimited :py:class:`NDInstance``.
        :param start: A starting offset into the :py:class:`NDInstance`
        :return: a :py:class:`Location` with this item's location and the location
            of all children or array items.

        ..  note: This can't easily be refactored into the :py:class:`Schema` class hierarchy.

            Factoring this into the schema would have the unfortunate consequence
            of binding schema with Location. We don't really want that.

            The composition choice is to attach a **Strategy** object to each  :py:class:`Schema` object
            that can emit an appropriate :py:class:`Location` object for that  :py:class:`Schema` subclass.
        """
        loc: Location
        if isinstance(schema, AtomicSchema):
            size = self.size(schema)
            loc = AtomicLocation(schema, start, start + size)
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
            loc = ArrayLocation(
                schema, item_size, maxItems, sublocation, start, start + total_size
            )
        elif isinstance(schema, ArraySchema):
            assert "maxItems" in cast(
                dict[str, Any], schema.attributes
            ) or "minItems" in cast(dict[str, Any], schema.attributes)
            maxItems = int(
                cast(dict[str, Any], schema.attributes).get(
                    "maxItems",
                    cast(dict[str, Any], schema.attributes).get("minItems", 0),
                )
            )
            # Compute the locations based on the number of items.
            sublocation = self.walk(schema.items, start)
            sublocation.unpacker = self.unpacker
            sublocation.locationMaker = weakref.ref(self)
            item_size = sublocation.size
            total_size = item_size * maxItems
            loc = ArrayLocation(
                schema, item_size, maxItems, sublocation, start, start + total_size
            )
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
            alt_locs = [
                self.walk(alternative_schema, start)
                for alternative_schema in schema.alternatives
            ]
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
        else:  # pragma: no cover
            # The subclasses of Schema aren't reflected in this IF-statement
            # OR... This isn't a Schema object in the first place.
            if isinstance(schema, dict):
                raise RuntimeError(f"raw dict {schema}; SchemaMaker().from_json() required")
            raise DesignError(f"Invalid Schema construct: {schema}")
        loc.unpacker = self.unpacker
        loc.locationMaker = weakref.ref(self)
        if anchor_name := loc.schema._attributes.get("$anchor"):
            self.anchors[anchor_name] = loc
        return loc

    def size(self, schema: Schema) -> int:
        """
        Returns the overall size of a given schema.

        The work is delegated to the :py:class:`Unpacker`.

        :param schema: The schema to size.
        :return: The size
        """
        return self.unpacker.calcsize(schema)

    def ndnav(self, instance: "NDInstance") -> "NDNav":
        """
        Return a :py:class:`NDNav` navigation helper
        for an Instance using an Unpacker and Schema.

        :param instance: The non-delimited instance to navigate into.
        :return: an :py:class:`NDNav` primed with location information unique to this instance.
        """
        location = self.from_instance(instance)
        return NDNav(self.unpacker, location, instance)


# Schema and Instance Navigation


class Nav(Protocol):
    """
    Helper to navigate into items by field name or index into an array.

    - For Non-Delimited instances, names as well as indices are required for object and array navigation.
      Further, a :py:class:`Location` is also required.

    - For Delimited instances, a name or an index can be used, depending on what the underlying Python
      Instance object is. Dictionaries use names, lists use indices.

    - For Workbook instances, we only know the cells of a row by name in the schema and convert to a position.

    A :py:class`Nav` is built by an :py:class:`Unpacker`::

        unpacker.nav(schema, instance)

    This provides a common protocol for building navigation helpers.
    """

    def name(self, name: str) -> "Nav":  # pragma: no cover
        """
        Navigate into an object by name.

        :param name: name
        :return: new :py:class:`Nav` for the named subschema.
        """
        ...

    def index(self, index: int) -> "Nav":  # pragma: no cover
        """
        Navigate into an array by index.

        :param index: index
        :return: new :py:class:`Nav` for the indexed instance within items
        """
        ...

    def value(self) -> Any:  # pragma: no cover
        """
        Returns the value of this instance.

        :return: Instance value.
        """
        ...

    def dump(self) -> None:  # pragma: no cover
        """
        A helpful dump of this schema and all subschema.
        """
        ...


class NDNav(Nav):
    """
    Navigate through an ``NDInstance`` using ``Location`` as a helper.
    """

    def __init__(
        self, unpacker: Unpacker[NDInstance], location: Location, instance: NDInstance
    ) -> None:
        """
        Create a Non-Delimited Instance Navigation Helper.s

        :param unpacker: The Unpacker to use.
        :param location: The Location object for this instance.
        :param instance: The underlying NDInstance.
        """
        self.unpacker = weakref.ref(unpacker)
        self.location = location
        self.instance = instance

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.location!r}, {self.instance!r})"

    @property
    def schema(self) -> Schema:
        """
        Provide the schema.

        :return: Schema for the Location.
        """
        return self.location.schema

    def name(self, name: str) -> "NDNav":
        """
        Locate the "$anchor" in this Object's properties and the related ``Location``.
        Return a new ``NDNav`` for the requested anchor or property name.

        :param name: name of anchor
        :return: NDNav for the subschema for the given property
        """
        if self.schema.type != "object":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type 'object'")

        # TODO: Find $anchors first, if no matching anchor, then try a property name lookup.

        # Two kinds of Locations -- Locations vs. RefToLocation.
        # For ObjectLocation (and all others) referent == self
        sublocation = cast(ObjectLocation, self.location).properties[name].referent

        logger.debug(f"{self.__class__.__name__}.name({name})")
        logger.debug(locals())

        return NDNav(
            cast(Unpacker[NDInstance], self.unpacker()), sublocation, self.instance
        )

    def index(self, index: int) -> "NDNav":
        """
        Locate the given index in an array.

        Compute an offset into the array of items.
        Create a special ``Location`` for the requested index value.
        The location, attribute, assigned to ``base_location``, is for index == 0.

        :param index: the array index value
        :return: An NDNav for the subschema of an item at the given index.
        """
        if self.schema.type != "array":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type array")

        subschema = cast(ArraySchema, self.schema).items

        base_location = cast(ArrayLocation, self.location)
        if index >= base_location.item_count:
            raise IndexError
        item_offset = base_location.item_size * index
        item_location = LocationMaker(
            cast(Unpacker[NDInstance], self.unpacker()), subschema
        ).from_instance(self.instance, start=base_location.start + item_offset)

        logger.debug(f"{self.__class__.__name__}.index({index})")
        logger.debug(locals())

        return NDNav(
            cast(Unpacker[NDInstance], self.unpacker()), item_location, self.instance
        )

    def __getitem__(self, selector: Union[int, str]) -> "NDNav":
        """
        Wrapper for :py:meth:`name` and :py:meth:`index` methods.

        If the selector is an integer, delegate to :py:meth:`index`.
        Otherwise, assume the selector is a string, delegate to :py:meth:`name`.

        :param selector: An integer or string selector.
        :return: new NDNav for the name or index.
        """
        if isinstance(selector, int):
            return self.index(selector)
        else:
            return self.name(selector)

    def value(self) -> Any:
        """
        The final Python value from the current schema and location.

        :returns: unpacked value from the current location.
        """
        return self.location.value(self.instance)

    def raw(self) -> Any:
        """
        Raw bytes (or text) from the current schema and location.

        :returns: raw value from the current location.
        """
        return self.instance[self.location.start : self.location.end]

    def raw_instance(self) -> "NDInstance":
        """
        Clone a piece of this instance as a new :py:class:`NDInstance` object.
        Since NDInstance is Union[BytesInstance, TextInstance], there are two paths:
        a new bytes or a new str.

        :returns: New :py:class:`NDInstance` for this loocation.
        """
        unpacker: Unpacker[NDInstance] = cast(Unpacker[NDInstance], self.unpacker())
        cls: Type[NDInstance] = self.instance.__class__
        return cls(self.raw())

    def dump(self) -> None:
        """
        Prints this Location and all children.

        Navigates a non-delimited Schema using :py:class:`Location` (based on the Schema)
        to expose values in the instance.
        """
        layout = "{:45s} {:3d} {:3d} {!r} {!r}"
        print(
            "{:45s} {:3s} {:3s} {!s} {!s}".format("Field", "Off", "Sz", "Raw", "Value")
        )
        for indent, loc, indices, raw, value in self.location.dump_iter(self):
            print(
                layout.format(
                    indent * "  "
                    + cast(dict[str, Any], loc.schema.attributes).get("cobol", ""),
                    loc.start,
                    loc.size,
                    "" if raw is None else raw,
                    "" if value is None else value,
                )
            )


class DNav(Nav):
    """
    Navigate through a :py:class:`DInstance` using a ``Schema``.
    This is a wrapper around a ``JSON`` document.

    Note that these objects have an inherent ambiguity. 
    A JSON document can have the form of a dictionary with names and values.
    The schema *also* names the properties and suggests types.
    If the two don't agree, that's an instance error, spotted by schema validation.

    The JSON/YAML/TOML parsers have types implied by syntax and the schema *also* has types.
    
    We need an option to validate the instance against the schema.
    """

    def __init__(
        self, unpacker: Unpacker[DInstance], schema: Schema, instance: JSON
    ) -> None:
        """
        Create a Delimited Instance Navigation helper.

        :param unpacker: The delimited unpacker to use
        :param schema: The schema to use
        :param instance: The delimited instance.
        """
        self.unpacker = weakref.ref(unpacker)
        self.schema = schema
        self.instance = instance

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.schema!r}, {self.instance!r})"

    def name(self, name: str) -> "DNav":
        """
        Locate the "$anchor" in this Object's properties.
        Return a new ``DNav`` for the requested anchor or property name.

        :param name: name of anchor
        :return: DNav for the subschema for the given property
        """
        # TODO Find $anchors, fall back to property names
        if self.schema.type != "object":
            raise TypeError(f"{self.schema!r} is not of type 'object'")
        subschema = cast(ObjectSchema, self.schema).properties[name]
        value = cast(dict[str, JSON], self.instance)[name]
        return DNav(cast(Unpacker[DInstance], self.unpacker()), subschema, value)

    def index(self, index: int) -> "DNav":
        """
        Locate the given index in an array.

        Compute an offset into the array of items.

        :param index: the array index value
        :return: An DNav for the subschema of an item at the given index.
        """
        if self.schema.type != "array":
            raise TypeError(f"{self.schema!r} is not of type 'array'")
        subschema = cast(ArraySchema, self.schema).items
        value = cast(list[JSON], self.instance)[index]
        return DNav(cast(Unpacker[DInstance], self.unpacker()), subschema, value)

    def value(self) -> Any:
        """
        The final Python value from the current schema.
        Consider refactoring to use Unpacker explicitly

        :returns: Python object for the current instance.
        """
        return self.instance

    def dump(self) -> None:
        """
        Prints this instance and all of its children.
        """
        layout = "{:53s} {!r}"
        print("{:53s} {!s}".format("Field", "Value"))
        for indent, schema, indices, value in self.schema.dump_iter(self):
            print(
                layout.format(
                    indent * "  " + self.schema.type, "" if value is None else value
                )
            )


class WBNav(Nav):
    """
    Navigate through a workbook ``WBInstance`` using a ``Schema``.
    
    A Workbook Row is a ``Sequence[Any]`` of cell values. Therefore, navigation
    by name translates to a position within the ``WBInstance`` row.
    """

    def __init__(
        self, unpacker: Unpacker[WBInstance], schema: Schema, instance: "WBInstance"
    ) -> None:
        """
        Creates a Workbook Instance Navigation helper

        :param unpacker: The workbook unpacker
        :param schema: The workbook sheet's schema
        :param instance: The workbook row instance
        """
        self.unpacker = weakref.ref(unpacker)
        self.schema = schema
        self.instance = instance

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.unpacker()}, {self.schema!r}, {self.instance!r})"

    def name(self, name: str) -> "WBNav":
        """
        Locate the "$anchor" in this Object's properties.
        Return a new ``DNav`` for the requested anchor or property name.

        :param name: name of anchor
        :return: WBNav for the subschema for the given property
        """
        if self.schema.type != "object":  # pragma: no cover
            raise TypeError(f"{self.schema!r} is not of type 'object'")
        # TODO Find $anchors, fall back to property names
        subschema = cast(ObjectSchema, self.schema).properties[name]
        if "position" in cast(dict[str, Any], subschema.attributes):
            position = cast(dict[str, Any], subschema.attributes)["position"]
        else:
            position = list(cast(ObjectSchema, self.schema).properties.keys()).index(
                name
            )
        try:
            return WBNav(
                cast(Unpacker[WBInstance], self.unpacker()),
                subschema,
                self.instance[position],
            )
        except IndexError as ex:
            logger.error(f"Column {name!r} not found in {self.instance}")
            return WBNav(
                cast(Unpacker[WBInstance], self.unpacker()),
                subschema,
                cast(WBInstance, [None]),
            )

    def index(self, index: int) -> "WBNav":
        """
        Locate the given index in an array.

        Compute an offset into the array of items.

        :param index: the array index value
        :return: An WBNav for the subschema of an item at the given index.
        """
        if self.schema.type != "array":
            raise TypeError(f"{self.schema!r} is not of type 'array'")
        subschema = cast(ArraySchema, self.schema).items
        return WBNav(
            cast(Unpacker[WBInstance], self.unpacker()), subschema, self.instance[index]
        )

    def value(self) -> Any:
        """
        The final Python value from the current schema.

        :returns: value created by the Workbook unpacker
        """
        return self.instance

    def dump(self) -> None:
        """
        Prints this instance and all of its children.
        """
        layout = "{:53s} {}"
        print("{:53s} {!s}".format("Field", "Value"))
        for indent, schema, indices, value in self.schema.dump_iter(self):
            attrs = cast(dict[str, Any], schema.attributes)
            name = attrs.get("$anchor", attrs.get("title", schema.type))
            print(
                layout.format(
                    indent * "  " + name, "" if value is None else repr(value)
                ).rstrip()
            )


class CSVNav(WBNav):
    pass


### Instance Implementations


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
