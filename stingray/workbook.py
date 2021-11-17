"""
Row, Sheet, and Workbook Definitions.

This includes the built-in workbook formats:

-   CSV
-   NDJSON a/k/a JSON Newline
-   COBOL

The Stingray Reader treats all workbooks alike. This means wrapping CSV files, Numbers_13, ODS, and XLSX files so they have a uniform "workbook" interface.

    Note: This implementation drops Numbers_09 and XLS files.

The idea is to have a **Facade** of ``Workbook```, ``Sheet``, and ``Row`` that describe all of these related file formats: non-delimited, delimited, and workbooks.

This depends on the lower-level `Location`, `Nav`, and `Schema` constructs.

Here are some additional side-bar considerations for other formats that depend on external modules.

- CSV is built-in. We permit kwargs to provide additional dialect details.

- JSON is built-in.  We'll treate newline delimited JSON like CSV. An `Unpacker` subclass can decompose a "one big list" JSON document into individual rows.

- XLS is a weird proprietary thing. The xlrd project (https://pypi.org/project/xlrd/) supports this.

- ODS and XLSX are XML files. Incremental parsing is helpful here because they can be large. See https://openpyxl.readthedocs.io/en/stable/ and http://docs.pyexcel.org/en/v0.0.6-rc2/.

- Numbers_13 is protobuf. The legacy version had  protobuf definitions which appear to work and a snappy decoder. See https://pypi.org/project/numbers-parser/ for a better solution.

- TOML requires an external library.  An `Unpacker` subclass can decompose a "one big list" TOML document into individual rows.

- YAML requires an external library. We'll use the iterative parser as a default. An `Unpacker` subclass can decompose a "one big list" YAML document into individual rows.

- XML is built-in. The schema drives navgiation through the XML document, locating the various tags which he schema requires. Other tags which may be present are ignored.

A given workbook has two possible sources for a schema: internal and external. An internal schema might be the first row or it might require more sophisticated parsing. An external schema might be hard-coded in the application, or might be a separate document with its own meta-schema.

Generally, the schema applies to a sheet (or a Table in a Workspace for Numbers.)


@startuml
    abstract class Schema
    abstract class Unpacker
    class Location
    Location --> Schema
    'abstract class NDInstance
    'Location -> NDInstance
    class LocationMaker {
        from_instance(instance): Location
    }
    Unpacker --> LocationMaker
    LocationMaker --> "n" Location : creates
    /'Details...
    abstract class Nav
    class NDNav
    class DNav
    class WBNav
    Nav <|-- NDNav
    Nav <|-- DNav
    Nav <|-- WBNav

    Unpacker --> NDNav : creates
    Unpacker --> DNav : creates

    NDNav --> Location

    class JSON
    DNav --> JSON
    '/

    class File
    abstract class Workbook ##[bold]blue

    Workbook --> Unpacker
    Workbook --> File : opens
    Unpacker --> File : reads

    class Sheet ##[bold]blue {
        row_iter(): Row
    }
    Workbook *-- "1:n" Sheet
    Sheet --> Schema
    class Row  ##[bold]blue {
        name(): Any
    }
    Sheet *-- "n" Row
    Row --> NDNav : "[non-delimited]"
    Row --> DNav : "[delimited]"
    Row --> WBNav : "[workbook]"
    class EmbeddedSchemaSheet
    class ExternalSchemaSheet
    Sheet <|-- EmbeddedSchemaSheet
    Sheet <|-- ExternalSchemaSheet
    class SchemaLoader
    EmbeddedSchemaSheet --> SchemaLoader
    ExternalSchemaSheet --> SchemaLoader
    SchemaLoader --> Schema : creates
@enduml

Legacy API Concept
==================

The original concept for the API looked like this:

**Internal, Embedded Schema**:

::

    from stingray.workbook import open_workbook, EmbeddedSchemaSheet, HeadingRowSchemaLoader

    with open_workbook(path) as workbook:
        sheet = EmbeddedSchemaSheet(workbook, 'Sheet1', HeadingRowSchemaLoader)
        process_sheet(sheet)

**External Schema**:

::

    from stingray.workbook import open_workbook, ExternalSchemaSheet, ExternalSchemaLoader

    with open_workbook(path) as schema_wb:
        esl = ExternalSchemaLoader(schema_wb, sheet_name='Schema')
        schema = esl.load()
    with open_workbook(path) as workbook:
        sheet = ExternalSchemaSheet(workbook, 'Sheet1', schema)
        process_sheet(sheet)

If necessary, the external schema can have a meta-schema. It may be necessary to define a conversion function to create a useful JSON Schema from a schema workbook.

COBOL, External Schema

For COBOL, there were `TextFile` and a `EBCDICFile` subclasses of `Workbook`.

Sheet and Row
-------------

The `Sheet` class contained metadata about the sheet, and a row iterator.

::

    def process_sheet(sheet: Sheet) -> None:
        for row in sheet.rows():
            process_row(row)

A `Row` contained an `Instance` of the sheet's `Schema`. This was decorated to describe the structure of non-delimited text or bytes. A number of subclasses of `Row` handled eager vs. lazy value computation, and occurs-depending-on processing.

The extraction of values was part of workbook parsing, leading to a number of `Cell` subclass definitions.

The location information was buried in a COBOL DDE dependency that was not properly foundational to the schema processing. Generally, the APi seems to be useful, however, as a model.

API Concept
===========

A "fluent" interface is used to open a sheet, extract a header, and process rows. The alternative is to open a sheet, apply the externally loaded schema, and use this to process the sheet's rows.
Once a sheet has been bound to a schema, the rows can be processed.

**Internal, Embedded Schema**:

::

    from stingray.workbook import open_workbook, HeadingRowSchemaLoader

    with open_workbook(path) as workbook:
        sheet = workbook.sheet('Sheet1')
        sheet.set_schema_loader(HeadingRowSchemaLoader(sheet))
        process_sheet(sheet.rows())

In this case, the ``rows()`` method of the ``Sheet`` will exclude the header rows consumed
by the ``HeadingRowSchemaLoader``.

**External Schema**:

::

    from stingray.workbook import open_workbook, ExternalSchemaLoader

    with open_workbook(schema_path) as metaschema_workbook:
        schema_sheet = metaschema_workbook.sheet('Sheet1')
        schema_sheet.set_schema(SchemaMaker().from_json(ExternalSchemaLoader.META_SCHEMA))
        json_schema = ExternalSchemaLoader(schema_sheet).parse()

    schema = SchemaMaker().from_json(json_schema)

    with open_workbook(path) as workbook:
        sheet = workbook.sheet('Sheet1').set_schema(schema)
        process_sheet(sheet.rows())

In this case, the ``rows()`` method of the ``Sheet`` will include all rows.

The **process_sheet()** method:

::

    def process_sheet(rows: Iterator[Row]) -> None:
        for row in rows:
            print(f'{row.name("field").value()=}')

The ``name()`` method returns a ``Nav`` object. This has a ``value()`` method that will extract the value
for a given attribute.

"""

import abc
import csv
from decimal import Decimal
import json
from pathlib import Path
from typing import (
    Iterator, Union, Optional, IO, Type, TextIO, BinaryIO, Any, Iterable, Callable, cast,
    AnyStr, TypeVar, Generic
)
import weakref

from stingray import estruct
from stingray.schema_instance import (
    Instance,
    DInstance,
    WBInstance,
    NDInstance,
    ListInstance,
    Nav,
    WBNav,
    NDNav,
    JSON,
    Schema,
    ObjectSchema,
    SchemaMaker,
    Unpacker,
    WBUnpacker,
    CSVUnpacker,
    JSONUnpacker,
    TextUnpacker,
    EBCDIC,
    Location,
    LocationMaker,
)
from types import TracebackType

try:  # pragma: no cover
    from jsonschema import Draft202012Validator as SchemaValidator
except ImportError:  # pragma: no cover
    from jsonschema import Draft7Validator as SchemaValidator  # type: ignore[import]


class Mode:
    TEXT = "r"
    BYTES = "rb"

class Workbook(Generic[Instance]):
    mode = Mode.TEXT
    def __init__(
        self, name: Union[Path, str], file_object: Optional[IO[AnyStr]] = None
    ) -> None:
        self.the_file: Union[IO[str], IO[bytes]]
        if isinstance(name, str):
            self.name = Path(name)
        else:
            self.name = name
        if file_object:
            self.the_file = file_object
        else:
            self.the_file = self.name.open(self.mode)
        self.unpacker: Unpacker[Instance]

    def __enter__(self) -> "Workbook[Instance]":
        return self

    def __exit__(self, exc_type: Optional[Type[BaseException]], exc_val: Optional[BaseException], exc_tb: TracebackType) -> None:
        if self.the_file:
            self.the_file.close()

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Workbook):
            return all(
                [
                    self.name == other.name,
                    self.unpacker is other.unpacker,
                ]
            )
        return NotImplemented  # pragma: no cover

    @abc.abstractmethod
    def sheet(self, name: str) -> "Sheet[Instance]":
        ...  # pragma: no cover

    @abc.abstractmethod
    def sheet_iter(self) -> Iterator["Sheet[Instance]"]:
        ...  # pragma: no cover

    @abc.abstractmethod
    def instance_iter(self, sheet: "Sheet[Instance]") -> Iterator[Instance]:
        ...  # pragma: no cover


WB = Union[Workbook[NDInstance], Workbook[WBInstance], Workbook[DInstance]]
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
        self.suffix_map: dict[str, WB_Type] = {}

    def file_suffix(self, *name_list: str) -> Callable[[WB_Type], WB_Type]:
        def concrete_decorator(cls: WB_Type) -> WB_Type:
            for name in name_list:
                self.suffix_map[name] = cls
            return cls
        return concrete_decorator

    def open_workbook(self, source: Path) -> WB:
        """
        Opens an appropriate subclass of Workbook for the given file.
        """
        try:
            cls = self.suffix_map[source.suffix]
        except KeyError:
            raise NotImplementedError(f"unsupported {source.suffix!r} suffix")
        return cls(source)


file_registry = WBFileRegistry()
open_workbook = file_registry.open_workbook

class Sheet(Generic[Instance]):
    """
    A Sheet of a Workbook. This can also be a table on a page of a Numbers workbook.

    It needs a schema binding to describe the content of each row.

    Either a Schema is loaded from an external source.
    OR an internal Schema loader is used to extract a schema from the first few rows of the sheet.
    """

    def __init__(self, workbook: WB, sheet_name: str) -> None:
        self.workbook = weakref.ref(workbook)
        self.name = sheet_name
        self.schema: Schema
        self.loader: SchemaLoader[Instance]
        self.raw_instance_iter: Iterator[Instance]
        self.row: "Row[Instance]"  # Cache of last yielded row

    def set_schema(self, schema: Schema) -> "Sheet[Instance]":
        """An externally-supplied schema."""
        self.schema = schema
        # Plug in a do-nothing schema loader when the schema is supplied externally.
        self.loader = SchemaLoader()
        return self

    def set_schema_loader(self, loader: "SchemaLoader[Instance]") -> "Sheet[Instance]":
        """A SchemaLoader that builds a schema by extracting some of the Instances."""
        self.loader = loader
        return self

    def row_iter(self) -> Iterator["Row[Instance]"]:
        """
        Passes instances through the schema loader.  (An external schema will have a dummy loader.)
        Extracts a schema from the header.
        Applies the schema to the body to create a :py:class:`Row` instances.
        """
        self.raw_instance_iter = cast("Workbook[Instance]", self.workbook()).instance_iter(self)
        json_schema = self.loader.header(self.raw_instance_iter)
        if json_schema:
            # Not all loaders build a schema. Some only pass instances through.
            # Optionally, validate the JSONSchema
            self.schema = SchemaMaker().from_json(json_schema)
        rows = (Row(self, x) for x in self.loader.body(self.raw_instance_iter))
        # Cache the most recent row in the sheet.
        for self.row in rows:
            yield self.row

    rows = row_iter

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Sheet):
            details_match = [
                self.workbook is other.workbook,
                self.name == other.name,
                self.schema == other.schema,
            ]
            # print(f"{self} == {other}: {details_match}")
            return all(details_match)
        return NotImplemented  # pragma: no cover

class Row(Generic[Instance]):
    """Wrapper around a :py:class:`Nav` object bound to an :py:class:`Instance`."""

    def __init__(self, sheet: Sheet[Instance], instance: Instance) -> None:
        self.sheet: weakref.ReferenceType[Sheet[Instance]] = weakref.ref(sheet)
        self.instance: Instance = instance
        unpacker: Unpacker[Instance] = cast(Workbook[Instance], sheet.workbook()).unpacker
        self.nav = unpacker.nav(sheet.schema, self.instance)

    @property
    def schema(self) -> Schema:
        return cast(Sheet[Instance], self.sheet()).schema

    def name(self, name: str) -> Nav:
        return self.nav.name(name)

    get = name
    __getitem__ = name

    def values(self) -> list[Any]:
        return [
            self.nav.name(name).value()
            for name in cast(ObjectSchema, cast(Sheet[Instance], self.sheet()).schema).properties
        ]

    def __repr__(self) -> str:
        return f"Row({self.sheet()}, {self.values()!r})"

    def __str__(self) -> str:
        return f"{self.values()}"

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Row):
            details_match = [
                self.sheet == other.sheet,
                self.instance == other.instance
            ]
            # print(f"{self} == {other}: {details_match}")
            return all(details_match)
        return NotImplemented  # pragma: no cover


class SchemaLoader(Generic[Instance]):
    """
    Loads a schema.

    There are two cases:

    - External schema from other files.

    - Internal schema. This is often the top row of a sheet. However, some workbooks will
      have elaborate headers. Some have footers that must also be excluded.

    This superclass is a "dummy" that does nothing. This is used when an external
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
        """
        return None

    def body(self, source: Iterator[Instance]) -> Iterator[Instance]:
        """
        Consume the non-header rows, applying an additional filter criteria.
        """
        return source


class HeadingRowSchemaLoader(SchemaLoader[Instance]):
    """
    Create a schema from the first row of a workbook sheet.
    The "Instance" is expected to be a WBInstance, which is a Sequence of column values,
    parsed by a WBUnpacker.
    """

    def __init__(self, sheet: Sheet[Instance]) -> None:
        self.sheet: Sheet[Instance] = sheet

    def header(self, source: Iterator[Instance]) -> JSON:
        first = cast(list[Any], next(source))
        json_schema = {
            "title": repr(self.sheet),
            "type": "object",
            "properties": {
                name: {"title": name, "$anchor": str(name), "type": "string", "position": n}
                for n, name in enumerate(first)
            },
        }
        return json_schema


class ExternalSchemaLoader(SchemaLoader[Instance]):
    """
    Read an external source of data to prepare a schema.

    A default schema for extrnal metadata is ExternalSchemaLoader.META_SCHEMA.
    The odds of this being correct are low.

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
        self.sheet: Sheet[Instance] = sheet

    def load(self) -> JSON:
        json_schema = {
            "title": repr(self.sheet),
            "type": "object",
            "properties": {
                row.name("name").value(): {
                    "title": row.name("name").value(),
                    "$anchor": row.name("name").value(),
                    "type": "string",
                    "position": n,
                    "description": row.name("description").value(),
                    "dataType": row.name("dataType").value(),
                }
                for n, row in enumerate(self.sheet.row_iter())
            },
        }
        return json_schema


"""
CSV Implementation
"""

@file_registry.file_suffix(".csv")
class CSV_Workbook(Workbook[WBInstance]):
    def __init__(
        self, name: Union[str, Path], file_object: Optional[IO[AnyStr]] = None, **kwargs: str
    ) -> None:
        super().__init__(name, file_object)
        self.csv_args = kwargs
        self.unpacker = CSVUnpacker()

    def sheet(self, name: str) -> Sheet[WBInstance]:
        return CSV_Sheet(self, "")

    def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
        yield CSV_Sheet(self, "")

    def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
        self.rdr = csv.reader(cast(TextIO, self.the_file), **self.csv_args)
        return (cast(WBInstance, row) for row in self.rdr)


class CSV_Sheet(Sheet[WBInstance]):
    pass

"""
NDJSON Implementation
"""

@file_registry.file_suffix(".json", ".ndjson", ".jsonnl")
class JSON_Workbook(Workbook[DInstance]):
    def __init__(
        self, name: Union[str, Path], file_object: Optional[IO[AnyStr]] = None, **kwargs: Any
    ) -> None:
        super().__init__(name, file_object)
        self.json_args = kwargs
        self.unpacker = JSONUnpacker()

    def sheet(self, name: str) -> Sheet[DInstance]:
        return JSON_Sheet(self, "")

    def sheet_iter(self) -> Iterator[Sheet[DInstance]]:
        yield JSON_Sheet(self, "")

    def instance_iter(self, sheet: Sheet[DInstance]) -> Iterator[DInstance]:
        for line in self.the_file:
            document = json.loads(line, **self.json_args)
            yield document


class JSON_Sheet(Sheet[DInstance]):
    pass

### xlrd for .xls files



"""
COBOL File Implementation -- EBCDIC and Text

The EBCDIC files require specific physical "Record Format" (RECFM) assistance.
These classes define a number of Z/OS RECFM conversion. We recognize four
actual RECFM's plus an additional special case.

-   F - Fixed.

-   FB - Fixed Blocked.

-   V - Variable, data has RDW preceeding each record.

-   VB - Variable Blocked, data must have BDW and RDW words.

-   N - Variable, but no BDW or RDW words. This involves some buffer management
    magic to recover the records properly. This is required to handle Occurs Depending On cases
    where there's no V or VB header. This requires the consumer of bytes to announce how many bytes
    where needed so the reader can advance an appropriate amount.
"""


class COBOL_Text_File(Workbook[NDInstance]):
    """Text Files in a fixed format. Newline delimited."""

    def __init__(
        self, name: Union[str, Path], file_object: Optional[IO[AnyStr]] = None, **kwargs: str
    ) -> None:
        super().__init__(name, file_object)
        self.unpacker = TextUnpacker()

    def sheet(self, name: str) -> Sheet[NDInstance]:
        return COBOL_Text_Sheet(self, "")

    def sheet_iter(self) -> Iterator[Sheet[NDInstance]]:
        yield COBOL_Text_Sheet(self, "")

    def instance_iter(self, sheet: Sheet[NDInstance]) -> Iterator[NDInstance]:
        self.row_source = cast(Iterator[NDInstance], iter(self.the_file))
        return self.row_source

class COBOL_Text_Sheet(Sheet[NDInstance]):
    """The single "Sheet" in a file. This is a container for the rows."""
    pass


class COBOL_EBCDIC_File(Workbook[NDInstance]):
    """
    EBCDIC-encoded files. No newline delimiters are used.

    The NDInstance is AnyStr, which is a union of str | bytes.
    This is narrower than that, and could be bytes only, not NDInstance.
    """

    mode = Mode.BYTES
    def __init__(
        self,
        name: Union[str, Path],
        file_object: Optional[IO[AnyStr]] = None,
        recfm_class: Optional[Type["estruct.RECFM_Reader"]] = None,
        lrecl: Optional[int] = None,
        **kwargs: str,
    ) -> None:
        super().__init__(name, file_object)
        self.unpacker = EBCDIC()
        self.recfm_class = recfm_class or estruct.RECFM_N
        self.lrecl: Optional[int] = None

    def sheet(self, name: str) -> Sheet[NDInstance]:
        return COBOL_EBCDIC_Sheet(self, "")

    def sheet_iter(self) -> Iterator[Sheet[NDInstance]]:
        yield COBOL_EBCDIC_Sheet(self, "")

    def instance_iter(self, sheet: Sheet[NDInstance]) -> Iterator[NDInstance]:
        recfm_parser = self.recfm_class(cast(BinaryIO, self.the_file), self.lrecl)
        for r in recfm_parser.record_iter():
            yield cast(NDInstance, r)
            # Ideally, all bytes are used.
            recfm_parser.used(cast(NDNav, sheet.row.nav).location.end)

class COBOL_EBCDIC_Sheet(Sheet[NDInstance]):
    """
    The single "Sheet" in a file. This is a container for the rows.
    It also retains the most recent row so we can determine the actual size
    in the presence of complex OCCURS DEPENDING ON clauses.
    """
    def set_schema(self, schema: Schema) -> Sheet[NDInstance]:
        super().set_schema(schema)
        wb = cast(COBOL_EBCDIC_File, self.workbook())
        if not wb.lrecl:
            # Compute lrecl from simple layout DDE's.
            loc = LocationMaker(wb.unpacker, self.schema).from_schema()
            wb.lrecl = loc.end
        return self

    def row_iter(self) -> Iterator[Row[NDInstance]]:
        for row in super().row_iter():
            self.row = row
            yield self.row

