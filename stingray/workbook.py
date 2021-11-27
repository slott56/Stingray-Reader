"""
workbook -- Row, Sheet, and Workbook Definitions.

The Stingray Reader treats all workbooks alike.
This means wrapping CSV files, Numbers_13, ODS, and XLSX files
with a **Facade** of :py:class:`Workbook`, :py:class:`Sheet`, and :py:class:`Row`
These classes describe the relevant features of these file formats.

This includes the following built-in workbook formats:

-   CSV via built-in :py:mod:`csv` module.

-   NDJSON a/k/a JSON Newline via the built-in :py:mod:`json` module.

-   COBOL files in EBCDIC via the :py:mod:`stingray.estruct` module.

-   COBOL files in native Text

-   TAB via CSV reader with dialetc options

-   Pure Text via External Schema.
    See ``workbook.simple`` and the metadata in ``simple.csv``.

Other formats are in the :py:mod:`implementations` module.

COBOL File Implementation -- EBCDIC and Text
--------------------------------------------

The EBCDIC files require specific physical "Record Format" (RECFM) assistance.
These classes define a number of Z/OS RECFM conversion.
The :py:mod:`estruct` module has classes for four actual RECFM's plus an additional special case.

-   :py:class:`estruct.RECFM_F` - Fixed.

-   :py:class:`estruct.RECFM_FB` - Fixed Blocked.

-   :py:class:`estruct.RECFM_V` - Variable, data has RDW preceeding each record.

-   :py:class:`estruct.RECFM_VB` - Variable Blocked, data must have BDW and RDW words.

-   :py:class:`estruct.RECFM_N` - Variable, but no BDW or RDW words. This involves some buffer management
    magic to recover the records properly. This is required to handle Occurs Depending On cases
    where there's no V or VB header. This requires the consumer of bytes to announce how many bytes
    where needed so the reader can advance an appropriate amount.

"""

import abc
import csv
from decimal import Decimal
import json
import logging
from pathlib import Path
import re
from typing import (
    Iterator,
    Union,
    Optional,
    IO,
    Type,
    TextIO,
    BinaryIO,
    Any,
    Iterable,
    Callable,
    cast,
    AnyStr,
    TypeVar,
    Generic,
    Match,
)
import warnings
import weakref

from stingray import estruct
from stingray.schema_instance import (
    Instance,
    DInstance,
    WBInstance,
    NDInstance,
    Nav,
    WBNav,
    NDNav,
    DNav,
    JSON,
    Schema,
    ObjectSchema,
    SchemaMaker,
    Unpacker,
    WBUnpacker,
    Delimited,
    TextUnpacker,
    Struct,
    EBCDIC,
    Mode,
    Location,
    LocationMaker,
    CONVERSION,
)
from stingray.cobol_parser import schema_iter
from types import TracebackType

try:  # pragma: no cover
    from jsonschema import Draft202012Validator as SchemaValidator
except ImportError:  # pragma: no cover
    from jsonschema import Draft7Validator as SchemaValidator  # type: ignore[import]

logger = logging.getLogger("stingray.workbook")


class Workbook(Generic[Instance]):
    """
    The **Facade** over all workbooks and COBOL Files.

    This provides uniform access to a variety of physical
    formats.

    Each subclass binds in an Unpacker and the
    unique features of that Unpacker.

    Generally, a Workbook can be opened one of
    three ways

    -   Specifically. ``wb = CSV_Workbook(path)``.
        This expects an explicit ``wb.close()`` to release
        resources.

    -   As a context manager ``with CSV_Workbook(path) as wb:``.
        The context manager release resources.

    -   Via the :py:func:`open_workbook` function.
    """
    def __init__(self, name: Union[Path, str], **kwargs: Any) -> None:
        """
        Creates a Workbook instance.

        :param name: :py:class:`Path` for the workbook's file.
        :param kwargs: KW Arguments to provide to the Unpacker.
        """
        if isinstance(name, str):
            self.name = Path(name)
        else:
            self.name = name
        self.kwargs = kwargs
        self.unpacker: Unpacker[Instance]

    def close(self) -> None:
        """Closes the workbook instance."""
        self.unpacker.close()

    def __enter__(self) -> "Workbook[Instance]":
        return self

    def __exit__(
        self,
        exc_type: Optional[Type[BaseException]],
        exc_val: Optional[BaseException],
        exc_tb: TracebackType,
    ) -> None:
        self.close()

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Workbook):
            return all([self.name == other.name, self.unpacker is other.unpacker,])
        return NotImplemented  # pragma: no cover

    def sheet(self, name: str) -> "Sheet[Instance]":
        """
        Builds a :py:class:`Sheet` instance for the given sheet name.

        :param name: Name of the sheet
        :return: :py:class:`Sheet` instance
        """
        return Sheet(self, name)

    def sheet_iter(self) -> Iterator["Sheet[Instance]"]:
        """
        Yields all :py:class:`Sheet` instances in this Workbook.

        :yields: :py:class:`Sheet` instance
        """
        yield from (Sheet(self, name) for name in self.unpacker.sheet_iter())


WB = Union[Workbook[NDInstance], Workbook[WBInstance], Workbook[DInstance]]


class Sheet(Generic[Instance]):
    """
    A Sheet of a Workbook. This can also be a table on a page of a Numbers workbook.

    It needs a schema binding to describe the content of each row.

    Either a Schema is loaded from an external source.
    OR an internal Schema loader is used to extract a schema from the first few rows of the sheet.

    These can be built manually, but the expectation
    is that they're created by the containing :py:class:`Workbook` object.
    """

    def __init__(self, workbook: Workbook[Instance], sheet_name: str) -> None:
        """
        Create a Sheet instance.

        :param workbook: The containing workbook.
        :param sheet_name: The name of the sheet.
        """
        self.workbook: weakref.ReferenceType[Workbook[Instance]] = weakref.ref(workbook)
        self.name = sheet_name
        # Plug in a do-nothing schema loader in case the schema is supplied externally.
        self.loader: SchemaLoader[Instance] = SchemaLoader()
        # Either schema supplied via :py:meth:`set_schema` or by the loader during row processing.
        self.schema: Schema
        self.raw_instance_iter: Iterator[Instance]
        self.row: "Row[Instance]"  # Cache of last yielded row

    def __repr__(self) -> str:
        schema = self.schema if hasattr(self, "schema") else None
        return f"{self.__class__.__name__}({self.workbook()}, {self.name!r}, schema={schema}, loader={self.loader})"

    def set_schema(self, schema: Schema) -> "Sheet[Instance]":
        """
        Provides an externally-supplied schema.

        This also sets a "do nothing" schema loader
        to assure that all rows are processed.

        :param schema: The :py:class:`Schema` to use.
        """
        self.schema = schema
        self.loader = SchemaLoader()
        return self

    def set_schema_loader(self, loader: "SchemaLoader[Instance]") -> "Sheet[Instance]":
        """
        Provides a :py:class:`SchemaLoader` that builds a schema by extracting some of the Instances.

        :param loader: A subclass of :py:class:`HeadingRowSchemaLoader` that will load schema
            and filter rows.
        """
        self.loader = loader
        return self

    def row_iter(self) -> Iterator["Row[Instance]"]:
        """
        Extracts rows from the unpacker's :py:meth:`instance_iter` method.
        Applies the schema to create :py:class:`Row` instances.

        This leverages the two-phase processing of
        :py:class:`HeadingRowSchemaLoader` class.
        The initial rows pass through the loader's
        :py:meth:`header` method.
        After the header has built a schema,
        the rows are consumed via the
        :py:meth:`header` of the loader.

        :yields: :py:class:`Row` instances.
        """
        wb = cast("Workbook[Instance]", self.workbook())
        self.raw_instance_iter = wb.unpacker.instance_iter(self.name, **wb.kwargs)  # type: ignore [attr-defined]
        json_schema = self.loader.header(self.raw_instance_iter)
        if json_schema:
            # Not all loaders build a schema. If no schema was created, do nothing.
            # TODO: Optionally, validate the JSONSchema before proceeding.
            self.schema = SchemaMaker().from_json(json_schema)
        # It's not clear if this is helpful...
        # if self.schema is None:
        #     warnings.warn(f"Reading sheet {self} without a schema")
        # Cache the most recent row in `sheet.row`.
        for instance in self.loader.body(self.raw_instance_iter):
            self.row = Row(self, instance)
            yield self.row

    rows = row_iter

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Sheet):
            details_match = [
                self.workbook is other.workbook,
                self.name == other.name,
                (
                    self.schema is None
                    and other.schema is None
                    or self.schema == other.schema
                )
                if hasattr(self, "schema") and hasattr(other, "schema")
                else True,
            ]
            return all(details_match)
        return NotImplemented  # pragma: no cover


class Row(Generic[Instance]):
    """Wrapper around a :py:class:`Nav` object bound to an :py:class:`Instance`.

    Note that both Instance and Sheet have the schema. While can be excessive,
    it's also possible the sheet suffers from many schema and the assignment
    is on an instance-by-instance basis.
    """

    def __init__(self, sheet: Sheet[Instance], instance: Instance) -> None:
        """
        Creates a :py:class:`Row` for a row from a :py:class:`Sheet`.

        :param sheet: The Sheet that has the Schema for this row.
        :param instance: The raw instance.
        """
        self.sheet: weakref.ReferenceType[Sheet[Instance]] = weakref.ref(sheet)
        self.instance: Instance = instance
        # Compute these eargerly. They *could* be deferred and cached.
        self.unpacker: Unpacker[Instance] = cast(
            Workbook[Instance], sheet.workbook()
        ).unpacker
        self.nav = self.unpacker.nav(sheet.schema, self.instance)

    @property
    def schema(self) -> Schema:
        """
        Extracts the schema from the associated sheet.

        :return: :py:class:`Schema` instance.
        """
        return cast(Sheet[Instance], self.sheet()).schema

    def name(self, name: str) -> Nav:
        """
        Creates a helper :py:class:`Nav` and navigates
        to the given name.

        :param name: An ``$anchor`` name.
        :return: A :py:class:`Nav` subclass appropriate to the Workbook's Unpacker.
        """
        return self.nav.name(name)

    get = name
    __getitem__ = name

    def values(self) -> list[Any]:
        """
        Creates a helper :py:class:`Nav` and returns the instances values
        for each name in the schema.

        :return: A list of cell values.
        """
        return [
            self.nav.name(name).value()
            for name in cast(
                ObjectSchema, cast(Sheet[Instance], self.sheet()).schema
            ).properties
        ]

    def dump(self) -> None:
        """
        Print a formatted dump of this row.
        """
        self.unpacker.nav(
            cast(Sheet[Instance], self.sheet()).schema, self.instance
        ).dump()

    def __repr__(self) -> str:
        return f"Row({self.sheet()}, {self.values()!r})"

    def __str__(self) -> str:
        return f"{self.values()}"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Row):
            details_match = [self.sheet == other.sheet, self.instance == other.instance]
            return all(details_match)
        return NotImplemented  # pragma: no cover


def name_cleaner(name: str) -> str:
    """
    JSON Schema names for anchors and properties are validated
    against patterns defined in places like core/$defs/anchorString.

    Anchor names must match the following pattern.

    ::

        r"^[A-Za-z_][-A-Za-z0-9._]*$"

    This function replaces illegal characters with "_"

    >>> name_cleaner("TYPICAL-COBOL")
    'TYPICAL-COBOL'
    >>> name_cleaner("common_python")
    'common_python'
    >>> name_cleaner("Not a 'good' name")
    'Not_a_good_name'
    >>> name_cleaner("!@#$%^")
    '_'
    >>> name_cleaner('')
    ''
    """
    while (
        groups := cast(
            Match[str], re.match(r"(^[A-Za-z_][-A-Za-z0-9._]*)?(.*)$", name)
        ).groups()
    )[1]:
        bad_char = groups[1][0]
        name = name.replace(bad_char, "_").replace("__", "_")
    return name


class SchemaLoader(Generic[Instance]):
    """
    Loads a schema.

    There are two general cases:

    -   Load an external schema from a Workbook.

    -   Extract the schema from within a sheet
        of a workbook. Often this is a heading row,
        but it can be more complex.

    The first case is simpler.
    The :py:class:`ExternalSchemaLoader` subclass
    implements a with a :py:meth:`ExternalSchemaLoader.load` method.
    This builds a JSONSchema from the rows it finds.
    All other external loaders should subclass this.

    The :py:class:`HeadingRowSchemaLoader` subclass
    extracts the top row of a sheet and creates a JSONSchema.
    The rows are presented as an iterable that's comsumed
    in two phases:

    1.  The :py:meth:`header` method consumes rows
        from an iterator
        until it has the required header.

    2.  The :py:meth:`body` method can consume
        rows from an iterator to filter
        empty rows, footers, or other irrelevancies
        from a workbook sheet. An alternative
        is to simply return the iterator if no
        filtering will be done.

    This superclass specifically does nothing. This is used when an external
    schema has been bound to a :py:class:`Sheet` and no futher processing is
    required. For example, when processing COBOL files where the schema is separate.

    A schema is returned as a JSONSchema document.
    This is almost always converted to a :py:class:`Schema` instance.
    If, however, changes need to be made, or the schema is used outside this module,
    it's already in a standard format.
    """

    def header(self, source: Iterator[Instance]) -> JSON:
        """
        Consume the first row or rows to build a schema.

        :param source: An iterator over instances.
        :return: A Schema or None if no schema can be parsed.
        """
        return None

    def body(self, source: Iterator[Instance]) -> Iterator[Instance]:
        """
        Consume the non-header rows, applying an additional filter criteria.

        :param source: The iterator over instances. This is the same iterator
            provided to :py:meth:`header`; it will be in the state
            left at the end of :py:meth:`header` processing.
        :return: :py:class:`Instance` instances for each row after the header.
        """
        return source


class HeadingRowSchemaLoader(SchemaLoader[Instance]):
    """
    Create a schema from the first row of a workbook sheet.
    The "Instance" is expected to be a WBInstance, which is a Sequence of column values,
    parsed by a WBUnpacker.
    """

    def header(self, source: Iterator[Instance]) -> JSON:
        """
        Creates a JSONSchema from the first row of a sheet.

        :param source: An iterator over instances.
        :return: A Schema or None if no schema can be parsed.
        """
        first = cast(list[Any], next(source))
        json_schema = {
            "type": "object",
            "properties": {
                str(name): {
                    "title": name,
                    "$anchor": name_cleaner(str(name)),
                    "type": "string",
                    "position": n,
                }
                for n, name in enumerate(first)
            },
        }
        return json_schema


class ExternalSchemaLoader(SchemaLoader[Instance]):
    """
    Read an external source of data to prepare a schema.

    A default schema for extrnal metadata is ``ExternalSchemaLoader.META_SCHEMA``.
    The odds of this being correct are low. The metaschema has three attributes:

    -   name. This becomes a property name.

    -   description. This becomes a property description.

    -   dataType. This becomes a property type.

    A subclass can do more sophisticated parsing of complex metadata.
    For example, COBOL DDE parsing, is an example of complex schema loading.

    A subclass could also read JSONSchema directly, if that's available.
    """

    META_SCHEMA = {
        "title": "generic meta schema for external schema documents",
        "type": "object",
        "properties": {
            "name": {"type": "string", "description": "field name", "position": 0},
            "description": {
                "type": "string",
                "description": "field description",
                "position": 1,
            },
            "dataType": {
                "type": "string",
                "description": "field data type",
                "position": 2,
            },
        },
    }

    def __init__(self, sheet: Sheet[Instance]) -> None:
        """
        Creates an external schema loader for a :py:class:`Sheet`
        :param sheet: The sheet to examine.
        """
        self.sheet: Sheet[Instance] = sheet

    def load(self) -> JSON:
        """
        Loads the sheet's rows and builds a JSONSchema.

        :return: JSONSchema instance built from the sbeet.
        """
        json_schema = {
            "type": "object",
            "properties": {
                row.name("name").value(): {
                    "title": row.name("name").value(),
                    "$anchor": name_cleaner(row.name("name").value()),
                    "type": "string",
                    "position": n,
                    "description": row.name("description").value(),
                    "conversion": row.name("dataType").value(),
                }
                for n, row in enumerate(self.sheet.row_iter())
            },
        }
        return json_schema


class COBOLSchemaLoader:
    """
    The most common case is a single COBOL Schema.
    For other, more complex situations, the single schema assumption may not be appropriate.
    """

    def __init__(self, source: Path) -> None:
        self.source = source

    def load(self) -> JSON:
        """
        Loads the given COBOL schema from a
        copybook file.

        If more than one are present, they're
        saved as the ``schemas`` attribute.

        The first ``01`` level record is returned
        as a JSONSchema.

        :return: JSONSchema from a COBOL DDE.
        """
        with self.source.open() as source:
            self.schemas = list(schema_iter(source))
        return self.schemas[0]


### Registry to map file suffix to implementation

WB_Type = Type[WB]


class WBFileRegistry:
    """
    A global registry for Workbook classes and their associated file suffixes.
    It makes an open_workbook() function that can open a wide variety of file types.

    >>> file_registry = WBFileRegistry()
    >>> @file_registry.file_suffix(".xyz")
    ... class XYZ_Workbook(Workbook):
    ...     pass
    >>> file_registry.suffix_map[".xyz"] == XYZ_Workbook
    True
    """

    def __init__(self) -> None:
        """Initialize the file registry."""
        self.suffix_map: dict[str, WB_Type] = {}

    def file_suffix(self, *name_list: str) -> Callable[[WB_Type], WB_Type]:
        """
        Decorator to adds a file suffix and associated
        class to the registry.

        :param name_list: String suffix or sequence of suffices.
        :return: Decorator that's applied to a class definition.
        """
        def concrete_decorator(cls: WB_Type) -> WB_Type:
            for name in name_list:
                self.suffix_map[name] = cls
            return cls

        return concrete_decorator

    def open_workbook(self, source: Path) -> WB:
        """
        Opens an appropriate subclass of :py:class:`Workbook` for the given file.

        :param source: Path to the Workbook
        :return: A :py:class:`Workbook` subclass.
        """
        try:
            cls = self.suffix_map[source.suffix]
        except KeyError:
            raise NotImplementedError(
                f"unsupported {source.suffix!r} suffix; not one of {self.suffix_map.keys()}"
            )
        return cls(source)


#: The global file registry.
file_registry = WBFileRegistry()

open_workbook = file_registry.open_workbook

"""
CSV Implementation
"""


class CSVUnpacker(WBUnpacker):
    """Upacker that wraps the :py:mod:`csv` module."""

    def open(
        self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None
    ) -> None:
        """
        Opens a CSV file for unpacking.

        :param name: Path to the CSV file.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        """
        if file_object:
            self.the_file = cast(IO[str], file_object)
        else:
            self.the_file = name.open(mode=Mode.TEXT)

    def close(self) -> None:
        """Close the CSV file."""
        if hasattr(self, "the_file") and self.the_file:
            self.the_file.close()
            del self.the_file

    def sheet_iter(self) -> Iterator[str]:
        """Yields all sheet names.
        For CSV files there's only one sheet,
        and the name is "".

        :yields: Literal[""]
        """
        yield ""

    def instance_iter(self, sheet: str, **kwargs: Any) -> Iterator[WBInstance]:
        """
        Yields rows from the CSV File.

        :param sheet: name of the sheet (not used)
        :param kwargs: Additional arguments provided to :py:func:`csv.reader`.
        :yields: rows from the CSV file.
        """
        self.rdr = csv.reader(self.the_file, **kwargs)
        for instance in self.rdr:
            yield cast(WBInstance, instance)


@file_registry.file_suffix(".csv")
class CSV_Workbook(Workbook[WBInstance]):
    """Facade for CSV and TAB Workbooks."""
    def __init__(
        self,
        name: Union[str, Path],
        file_object: Optional[IO[AnyStr]] = None,
        **kwargs: Any,
    ) -> None:
        """
        Open the Workbook for access.

        :param name: Path to the workbook.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        :param kwargs: KW Args to provide to the :py:mod:`csv` module.
        """
        super().__init__(name, **kwargs)
        self.unpacker = CSVUnpacker()
        self.unpacker.open(self.name, file_object)

    def close(self) -> None:
        """Closes the workbook."""
        self.unpacker.close()


"""
NDJSON Implementation
"""


class JSONUnpacker(Delimited):
    """
    Unpacker that wraps the :py:mod:`json` module..
    """

    def open(
        self,
        name: Path,
        file_object: Optional[Union[IO[str], IO[bytes]]] = None,
        **kwargs: Any,
    ) -> None:
        """
        Opens the NDJSON file.

        :param name: Path to the file.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        :param kwargs: KW arguments (not used)
        """
        if file_object:
            self.the_file = cast(IO[str], file_object)
        else:
            self.the_file = name.open(mode=Mode.TEXT)

    def close(self) -> None:
        """Closes the NDJSON file."""
        if hasattr(self, "the_file") and self.the_file:
            self.the_file.close()
            del self.the_file

    def sheet_iter(self) -> Iterator[str]:
        """Yields all sheet names.
        For NDJSON files there's only one sheet,
        and the name is "".

        :yields: Literal[""]
        """
        yield ""

    def instance_iter(self, name: str, **kwargs: Any) -> Iterator[DInstance]:
        """
        Yields items from the NDJSON File.
        Documents are delimited with newlines.

        :param sheet: name of the sheet (not used)
        :param kwargs: Additional arguments provided to :py:func:`json.loads`.
        :yields: rows from the NDJSON file.
        """
        for line in self.the_file:
            instance = json.loads(line, **kwargs)
            yield instance


@file_registry.file_suffix(".json", ".ndjson", ".jsonnl")
class JSON_Workbook(Workbook[DInstance]):
    """Facade for NDJSON Workbooks. Also called JSON Newline."""
    def __init__(
        self,
        name: Union[str, Path],
        file_object: Optional[IO[AnyStr]] = None,
        **kwargs: Any,
    ) -> None:
        """
        Opens the workbook file for access.

        :param name: Path to the NDJSON file.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        :param kwargs: Not Used
        """
        super().__init__(name)
        self.kwargs = kwargs
        self.unpacker = JSONUnpacker()
        self.unpacker.open(self.name, file_object)

    def close(self) -> None:
        """Closees the Workbook file."""
        self.unpacker.close()


### COBOL Files


class COBOL_Text_File(Workbook[NDInstance]):
    """
    Facade for COBOL data in native text
    with in a fixed format, newline delimited.

    This uses the :py:class:`TextUnpacker` unpacker.
    """

    def __init__(
        self,
        name: Union[str, Path],
        file_object: Optional[IO[AnyStr]] = None,
        **kwargs: str,
    ) -> None:
        """
        Opens the workbook for access.

        :param name: Path to the file.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        :param kwargs: Additional KWArgs. Not used.
        """
        super().__init__(name)
        self.unpacker = TextUnpacker()
        self.unpacker.open(self.name, file_object)

    def close(self) -> None:
        """Closes the Workbook."""
        self.unpacker.close()


class COBOL_EBCDIC_File(Workbook[NDInstance]):
    """
    EBCDIC-encoded files. No newline delimiters are used.

    The NDInstance is AnyStr, which is a union of str | bytes.
    This is narrower than that, and could be bytes only, not NDInstance.
    """

    def __init__(
        self,
        name: Union[str, Path],
        file_object: Optional[IO[AnyStr]] = None,
        recfm_class: Optional[Type["estruct.RECFM_Reader"]] = None,
        lrecl: Optional[int] = None,
        **kwargs: str,
    ) -> None:
        """
        Opens the workbook for access.

        :param name: Path to the file.
        :param file_object: Optional Open IO object. If omitted the name is opened.
        :param recfm_class: A :py:class:`estruct.RECFM_Reader` that helps read the records.
        :param lrecl: The LRECL to apply. This may be computable from the schema.
            In the case of ``OCCURS DEPENDING ON`` it cannot be computed,
            and ``None`` should be provided.
        :param kwargs: Additional KW args (not used.)
        """
        super().__init__(name)
        self.recfm_class = recfm_class or estruct.RECFM_N
        self.lrecl: Optional[int] = lrecl
        self.unpacker = EBCDIC()
        self.unpacker.open(self.name, file_object)

    def close(self) -> None:
        """Closes the workbook"""
        self.unpacker.close()

    def sheet(self, name: str) -> Sheet[NDInstance]:
        """
        Builds a :py:class:`Sheet` instance for the given sheet name.

        :param name: Name of the sheet (Not used)
        :return: :py:class:`COBOL_EBCDIC_Sheet` instance
        """
        return COBOL_EBCDIC_Sheet(self, "")

    def sheet_iter(self) -> Iterator[Sheet[NDInstance]]:
        """
        Yields all :py:class:`Sheet` instances in this Workbook.

        :yields: :py:class:`COBOL_EBCDIC_Sheet` instance
        """
        yield from (
            COBOL_EBCDIC_Sheet(self, name) for name in self.unpacker.sheet_iter()
        )


class COBOL_EBCDIC_Sheet(Sheet[NDInstance]):
    """
    The single "Sheet" in a file. This is a container for the rows.
    It handles the RECFM and LRECL complexities of variable-length non-delimited records.
    """

    def set_schema(self, schema: Schema) -> Sheet[NDInstance]:
        """
        Sets the schema for this COBOL file.

        If the workbook has an LRECL alread set,
        this is used to process records.

        If the workbook did not have an LRECL set,
        the Schema is examined to see if an LRECL
        can be computed.

        This extracts the LRECL eagerly to make the it visible
        as soon as possible.

        :param schema: :py:class:`Schema` instance built from the COBOL copybook.
        """
        result = super().set_schema(schema)
        wb = cast(COBOL_EBCDIC_File, self.workbook())
        if wb.lrecl:
            self.lrecl = wb.lrecl
        else:
            # Compute lrecl from layout DDE's, assuming no Occurs Depending On clauses.
            loc = LocationMaker(wb.unpacker, self.schema).from_schema()
            self.lrecl = loc.end
        return result

    def row_iter(self) -> Iterator["Row[NDInstance]"]:
        """
        Extracts rows from the unpacker.
        Applies the schema to create a :py:class:`Row` instances.

        :yields: :py:class:`Row` instances.
        """
        wb = cast(COBOL_EBCDIC_File, self.workbook())
        self.raw_instance_iter = wb.unpacker.instance_iter(self.name, recfm_class=wb.recfm_class, lrecl=self.lrecl)  # type: ignore [attr-defined]
        for instance in self.raw_instance_iter:
            self.row = Row(self, instance)
            yield self.row
            # Ideally, all bytes are used.
            wb.unpacker.used(cast(NDNav, self.row.nav).location.end)

    rows = row_iter
