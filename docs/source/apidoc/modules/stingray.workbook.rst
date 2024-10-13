stingray.workbook
=================

.. automodule:: stingray.workbook

The definitions are mostly protocols to handle non-delimited (i.e., COBOL files), delimited files, and workbooks.
This depends on the lower-level :py:class:`Schema`, :py:class:`Instance`, :py:class:`Nav`, and :py:class:`Location` constructs.

..  uml::

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


Here are some additional side-bar considerations for other formats that depend on external modules.

- CSV is built-in. We permit kwargs to provide additional dialect details.

- JSON is built-in.  We'll treate newline delimited JSON like CSV.

- XLS is a weird proprietary thing. The ``xlrd`` project (https://pypi.org/project/xlrd/) supports it.

- ODS and XLSX are XML files. Incremental parsing is helpful here because they can be large. See https://openpyxl.readthedocs.io/en/stable/ and http://docs.pyexcel.org/en/v0.0.6-rc2/.

- Numbers uses protobuf. The legacy version of Stingray Reader had  protobuf definitions which appear to work and a snappy decoder. See https://pypi.org/project/numbers-parser/ for the better solution currently in use.

- TOML requires an external library.  An :py:class:`Unpacker` subclass can decompose a "one big list" TOML document into individual rows.

- YAML requires an external library. We'll use the iterative parser as a default. An :py:class:`Unpacker` subclass can decompose a "one big list" YAML document into individual rows.

- XML is built-in. A schema can drive navgiation through the XML document, name the various tags of interest. Other tags which may be present would be ignored.


A given workbook has two possible sources for a schema: internal and external. An internal schema might be the first row or it might require more sophisticated parsing. An external schema might be hard-coded in the application, or might be a separate document with its own meta-schema.

Generally, the schema applies to a sheet (or a Table in a Workspace for Numbers.)

API Concept
------------------

A "fluent" interface is used to open a sheet, extract a header, and process rows. The alternative is to open a sheet, apply the externally loaded schema, and use this to process the sheet's rows.
Once a sheet has been bound to a schema, the rows can be processed.

**Internal, Embedded Schema**:

::

    >>> from stingray import open_workbook, HeadingRowSchemaLoader, Row
        >>> from pathlib import Path
        >>> import os
        >>> from typing import Iterable
        >>> source_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_quartet_data.csv"

        >>> def process_sheet(rows: Iterable[Row]) -> None:
        ...     for row in rows:
        ...         row.dump()
        ...         break  # Stop after 1 row.

        >>> with open_workbook(source_path) as workbook:
        ...    sheet = workbook.sheet('Sheet1')
        ...    _ = sheet.set_schema_loader(HeadingRowSchemaLoader())
        ...    process_sheet(sheet.rows())
        Field                                                 Value
        object
          x123                                                '10.0'
          y1                                                  '8.04'
          y2                                                  '9.14'
          y3                                                  '7.46'
          x4                                                  '8.0'
          y4                                                  '6.58'


    In this case, the :py
    >>> from pathlib import Path
    >>> import os
    >>> from typing import Iterable
    >>> source_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_quartet_data.csv"

    >>> def process_sheet(rows: Iterable[Row]) -> None:
    ...     for row in rows:
    ...         row.dump()
    ...         break  # Stop after 1 row.

    >>> with open_workbook(source_path) as workbook:
    ...    sheet = workbook.sheet('Sheet1')
    ...    _ = sheet.set_schema_loader(HeadingRowSchemaLoader())
    ...    process_sheet(sheet.rows())
    Field                                                 Value
    object
      x123                                                '10.0'
      y1                                                  '8.04'
      y2                                                  '9.14'
      y3                                                  '7.46'
      x4                                                  '8.0'
      y4                                                  '6.58'


In this case, the :py:meth:`rows` method of the :py:class:`Sheet` instance will exclude the header rows consumed
by the :py:class:`HeadingRowSchemaLoader`.

**External Schema**:

::

    >>> from stingray import open_workbook, ExternalSchemaLoader, Row, SchemaMaker
    >>> from pathlib import Path
    >>> import os
    >>> from typing import Iterable
    >>> source_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_quartet_data.csv"
    >>> schema_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_schema.csv"

    >>> with open_workbook(schema_path) as metaschema_workbook:
    ...     schema_sheet = metaschema_workbook.sheet('Sheet1')
    ...     _ = schema_sheet.set_schema(SchemaMaker().from_json(ExternalSchemaLoader.META_SCHEMA))
    ...     json_schema = ExternalSchemaLoader(schema_sheet).load()
    ...     schema = SchemaMaker().from_json(json_schema)

    >>> with open_workbook(source_path) as workbook:
    ...     sheet = workbook.sheet('Sheet1').set_schema(schema)
    ...     process_sheet(sheet.rows())
    Field                                                 Value
    object
      x123                                                'x123'
      y1                                                  'y1'
      y2                                                  'y2'
      y3                                                  'y3'
      x4                                                  'x4'
      y4                                                  'y4'

    In this case, the :py
    >>> from pathlib import Path
    >>> import os
    >>> from typing import Iterable
    >>> source_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_quartet_data.csv"
    >>> schema_path = Path(os.environ.get("SAMPLES", "sample")) / "Anscombe_schema.csv"

    >>> with open_workbook(schema_path) as metaschema_workbook:
    ...     schema_sheet = metaschema_workbook.sheet('Sheet1')
    ...     _ = schema_sheet.set_schema(SchemaMaker().from_json(ExternalSchemaLoader.META_SCHEMA))
    ...     json_schema = ExternalSchemaLoader(schema_sheet).load()
    ...     schema = SchemaMaker().from_json(json_schema)

    >>> with open_workbook(source_path) as workbook:
    ...     sheet = workbook.sheet('Sheet1').set_schema(schema)
    ...     process_sheet(sheet.rows())
    Field                                                 Value
    object
      x123                                                'x123'
      y1                                                  'y1'
      y2                                                  'y2'
      y3                                                  'y3'
      x4                                                  'x4'
      y4                                                  'y4'

In this case, the :py:meth:`rows` method of the :py:class:`Sheet` instance will include all rows.
This means the header row is treated like data when an external schema is applied.

The **process_sheet()** method:

::

    def process_sheet(rows: Iterator[Row]) -> None:
        for row in rows:
            print(f'{row.name("field").value()=}')

The :py:meth:`name` method returns a :py:class:`Nav` object. This has a :py:meth:`value` method that will extract the value
for a given attribute.


Sheet and Row
-------------

The :py:class:`Sheet` class contained metadata about the sheet, and a row iterator.
It's generally used like this:

::

    def process_sheet(sheet: Sheet) -> None:
        for row in sheet.rows():
            process_row(row)

A :py:class:`Row` contains an instance that's bound to the :py:class::py:class:`Unpacker` and the :py:class:`Schema`.
This will build :py:class:`Nav` objects for navigation. Where necessary, this may involve creating
:py:class:`Location` objects as part of :py:class:`NDNav` navigation.

   

open_workbook function
----------------------

.. autofunction:: open_workbook
   
   

   
name_cleaner function
---------------------

.. autofunction::      name_cleaner

   
Core Workbook, Sheet, and Row Model
------------------------------------
   
..  autoclass::       Workbook
    :members:
    :undoc-members:

..  autoclass::       Sheet
    :members:
    :undoc-members:

..  autoclass::       Row
    :members:
    :undoc-members:

Schema Loaders
--------------

..  autoclass::      SchemaLoader
    :members:
    :undoc-members:

..  autoclass::      COBOLSchemaLoader
    :members:
    :undoc-members:

..  autoclass::      ExternalSchemaLoader
    :members:
    :undoc-members:

..  autoclass::      HeadingRowSchemaLoader
    :members:
    :undoc-members:

COBOL Files
-----------

..  autoclass::      COBOL_EBCDIC_File
    :members:
    :undoc-members:

..  autoclass::      COBOL_EBCDIC_Sheet
    :members:
    :undoc-members:

..  autoclass::      COBOL_Text_File
    :members:
    :undoc-members:

CSV Workbooks
-------------

..  autoclass::      CSVUnpacker
    :members:
    :undoc-members:

..  autoclass::      CSV_Workbook
    :members:
    :undoc-members:

JSON Workbooks
--------------

..  autoclass::      JSONUnpacker
    :members:
    :undoc-members:

..  autoclass::      JSON_Workbook
    :members:
    :undoc-members:

Workbook File Registry
----------------------

..  autoclass::      WBFileRegistry
    :members:
    :undoc-members:

..  function:: file_registry.file_suffix

    A decorator used to mark a class
    with the file extensions it handles.
   

   
   
   




Legacy API Concept
------------------

The version 4.0 concept for the API looked like this:

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
