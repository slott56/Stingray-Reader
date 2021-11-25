
.. _`developer`:

######################################
Using **Stingray Reader**
######################################

We use **Stingray** to work with data files where the schema is 
either external or complex (or both). We can tackle this question:

    **How do we process a file simply and consistently?**
    
Or, more concretely, 

    **How do we make a program independent of Physical Format and Logical Layout?**
    
We can also use **Stingray** to answer questions about files, the schema those
files purport to use, and the data on those files.
Specifically, we can tackle this question:

    **How do we ensure that a file and an application use the same schema?**

We do need to note the following.

    **If it was simple, we wouldn't need this package, would we?**
    
Concepts
========

As noted in :ref:`intro`, there are three levels of schema that need to be bound to a file.

-   **Physical Format**.  We can make this transparent to our applications.
    When using Stingray Reader, everything is a workbook with a consistent API.
    
-   **Logical Layout**.  This is how an application program will make use
    of the data found in a file. 
    Sometimes the Logical Layout is described by a schema embedded in a file:
    it might be the top row of a sheet in a workbook, for example.
    Sometimes the logical layout can be separate: a COBOL data definition, or perhaps
    a metadata sheet in a workbook.

-   **Conceptual Content**.  
    A single conceptual schema may be implemented by a number of physical
    formats and logical layouts.  
    An application should be able to tolerate variability in the logical
    layout as long is it matches the expected **conceptual content**.

We can't make the logical layout completely transparent.
Our suite of applications all need to agree on a logical layout.
A change to one application's production or consumption must lead to a change to the others.
The column names in the metadata, at a bare minimum, must be agreed to.
The order or position of the columns, however, need not be fixed.

Since we're working in Python, the conceptual schema is often a class
definition. The idea is to provide many ways to build a class instance
based on variations in the logical layout.

There are two sides to schema use:

-   **Within the Application**. To process a data file, we need to bind schema information to the file.
    For a workbook, the schema may be in the column headings of the sheet.
    Or it may be in a separate sheet, or a separate workbook.
    For a COBOL file, the schema is always in a separate COBOL-language
    data definition.

-   **Data Quality**. Given a data file, how do we confirm that some schema matches the file?

We'll tackle the schema binding in several pieces.

-   **File Schema**.  `Binding a Schema to a File`_ describes some preliminary
    operational steps that make Stingray work more simply and reliably.

-   **Processing**. There are two concepts: **Capture** and **Conversion**.
    The `Data Attribute Mapping -- Using a Schema`_ section discusses design patterns for capture.
    The `Data Transformation`_ section focuses on conversion.

-   **Application Design Patterns**. These are more complex issues.
    We can then dig into **Stingray** application programming in `stingray Application Design`_,
    `Variant Records and COBOL REDEFINES`_, and `Big Data Performance`_.

-   **Data Management**. `File Naming and External Schema`_,  
    `Binding a Schema to an Application`_, and `Schema Version Numbering`_.

-   **FAQ**. Some other design questions. `Frequently Asked Questions`_.
        
We'll look at some demonstration software in :ref:`demo`.

Binding a Schema to a File 
=================================

The file suffix, e.g., ``.xlsx``, binds a file's collection of bytes to a physical format.
This pattern is relatively universal. A single :py:func:`stingray.open_workbook` function
looks at the file suffix and locates an appropriate class.

A decorator is available to extend Stingray's file suffix mapping to introduce new
physical format bindings.

We need to bind the logical layout to the file also.
This means relying on a schema, defined by the
:py:class:`stingray.schema_instance.Schema` class to describe
the logical layout of a file.
The :py:class:`stingray.workbook.Sheet` and :py:class:`stingray.workbook.Row` classes
use a :py:class:`stingray.schema_instance.Schema` object to describe each row.

An application must load the schema first. There several sources for schema:

-   **Embedded**.  This is commonly seen as column titles within the sheet.  Or
    any variation on that theme. The common case of column titles is handled
    by a built-in schema loader, :py:class:`stingray.workbook.HeadingRowSchemaLoader`.
    Other variations are managed by building different schema loaders.
    
-   **External**.  This is a separate sheet or separate file. In this case, we
    can start with :py:class:`stingray.workbook.ExternalSchemaLoader` to read
    an external schema. In the case of COBOL files, there's a separate 
    :py:class:`stingray.workbook.COBOLSchemaLoader` that parses COBOL source to create
    a usable schema.

All schemae start as JSONSchema documents. These are Python ``dict[str, Any]`` structures.
A :py:class:`stingray.workbook.SchemaMaker` object is used to transform the JSONSchema
document into a usable :py:class:`stingray.schema_instance.Schema` object. This permits
pre-processing the schema to add features or correct problems.

Data Attribute Mapping -- Using a Schema
==========================================

Data processing starts with **Capture**.
Using a schema is the heart of solving the semantic problem of capturing data in spreadsheet and COBOL files.

We want to have just one application that is adaptable to a number
of variant logical layouts that reflect alternative implementations of a single conceptual content.
Ideally, there's one layout and one schema, but as a practical matter, there are often several similar schemae.

We need to provide three pieces of information.

-   Target attribute name or parameter used by our application.

-   Target data type conversion for our application.

-   Source attribute based on attribute name or position in the source file.

This tripl is essentially a Python assignment statement
with *target*, *to_type* and *source*. A DSL or other encoding is unhelpful.

A simple description is the following:

..  parsed-literal::

    *target* = *target_type*\ (row.['\ *source*\ '].value())

There is a tiny bit of boilerplate in this assignment statement. The overhead of the boilerplate
is offset by the flexibility of using Python directly.

There are some common cases that will extend or modify the boilerplate.
In particular, COBOL structures that are not in first normal form will include
array indexing. COBOL can have ambiguous names, requiring a navigation path to
an atomic value. Finally, because of the COBOL redefines feature, it helps to
do lazy evaluation to compute the value after navgiating to the desired string of bytes.

This is our preferred design pattern: a **Builder Function**:

::

    def build_record_dict(aRow: Row) -> dict[str, Any]:
        return dict(
            name = row['some column'].value(),
            address = row['another column'].value(),
            zip = digits_5(row['zip'].value),
            phone = row['phone'].value(),
        )
        
This function defines the application-specific mapping from a row
in a file. It leverages logical layout information from the schema
definition.

Of course, the schema can lie, and the application can misuse the data.
Those are inevitable (and therefore insoluble) problems.  This is why
we must write customized software to handle these data sources.

In the case of variant schemae, we can use like something like this.

::

    def build_record_dict_1(aRow: Row) -> dict[str, Any]:
        return dict(
            name = row['some column'].value(),
            address = row['another column'].value(),
            zip = digits_5(row['zip'].value()),
            phone = row['phone'].value(),
        )

    def build_record_dict_2(aRow: Row) -> dict[str, Any]:
        return dict(
            name = row['variant column'].value(),
            address = row['something different'].value(),
            zip = digits_5(row['zip'].value()),
            phone = row['phone'].value(),
        )

We can then define a handy factory which picks a builder based on the schema 
version.

..  py:function:: make_builder(args)

    Create a builder object from the args.

    :param args: schema version
    :returns: appropriate builder function for the schema
        
..  parsed-literal::

    def make_builder(args: argparse.namespace) -> Callable[[Row], dict[str, Any]]:
        return eval('build_record_dict_{args.layout}')

Some people worry that an Evil Super-Genius (ESG) might somehow try to exploit the `eval()` function.
The ESG would have to be both clever and utterly unaware that the source
is easily edited Python. People who worry about an ESG that can manipulate the parameters but
while unable to simply edit the Python can use the following:

..  parsed-literal::

        {'1': build_record_dict_1, '2': build_record_dict_2}[args.layout]

The :py:func:`make_builder` function selects one of the available
builders based on a command-line option in the ``args`` structure.

Data Transformation
=================================

There are two parts to data handling: **Capture** and **Conversion**.
Conversion is part of the final application, once the source data has been captured.

A target data conversion can be rather complex.
It can involve involve any combination of filtering, cleansing, conforming to an existing database, or rewriting.

Here's a much more complex **Builder Function** that includes conversion.

::

    def build_record_3(aRow: Row) -> dict[str, Any]:
        if not aRow['flag']:
            return {}
        zip_str = aRow['zip'].value()
        if '-' in zip:
            zip = digits_9(zip_str.replace('-', ''))
        else:
            if len(zip) <= 5:
                zip = digits_5(zip_str)
            else:
                zip = digits_9(zip_str)
        return dict(
            name = aRow['variant column'].value(),
            address = arow['different column'].value(),
            zip = zip,
            phone = aRow['phone'].value(),
        )
        
This shows filtering and cleansing operations.  Yes, it's complex.
Indeed, it's complex enough that attempting to define a domain-specific language will lead to
more problems than simply using Python for this.

**Stingray** Application Design
=================================

We need to consider two tiers of testing.  Conventional unit testing
makes sure our application's processing is valid.
Beyond that, we also need to do data quality testing to
ensure that the data itself is valid.

For application unit testing, our programs should be decomposed into three tiers of
processing.

-   Row-Level.  Inidividual Python objects built from one row of the input.
    This involves our builder functions.

-   Sheet-Level.  Collections of Python objects built from all rows of a sheet.
    This involves sheet processing functions. Mocked row-level functions should be used.

-   Workbook-Level.  In some cases, we may need to work with a collection of sheets.
    If required, these tests will need mocked sheet and row functions.

Each of these tiers should be tested independently.

For data quality testing, we need to validate that the the input files meet the expected schema.
This can use the unit testing framework. However, it's often more helpful to
design application software to work in a "dry-run" or "validation" mode.
This operating mode can check the data without make persistent state changes
to other files or databases.

Row-Level Processing
----------------------

Row-level processing is centered on the builder functions.
These handle the detailed mapping 
from variant logical layouts to a single conceptual schema.

A builder function can create a simple dictionary or :py:class:`types.SimpleNamespace`.

Note that there are two separate steps here.

-   Preparing data for a candidate object. A ``dict[str, Any]`` has data values.
    There may be a number of different builder functions for this.

-   Building an application object from candidate data.
    These objects are often a :py:class:`typing.NamedTuple` or :py:class:`dataclasses.dataclass`.
    These should not vary with the logical layout.

This echoes the design patterns from the Django project where a ``ModelForm``
is used to validate data before attempting to build a ``Model`` instance.

Validation within the class ``__init__()`` method, while possible, is often awkwardly complex.
There are two separate things bound together: validating and initialization. While these
can be separated into methods used by ``__init__()``, each change to a logical layout becomes
yet another subclass. In this case, composition seems more flexible than inheritance.

One additional reason for decomposing the building from the application object
construction is to support multiprocessing pipelines. It's often quicker to serialize
a Python object built as ``dict[str, Any]`` than to serialize an instance of a new class.

Here's the three-part operation: **Build, Validate, and Construct**.

..  parsed-literal::

    def builder_1(row: Row) -> dict[str, Any]:
        return dict(
            *key* = row['field'].vaue(),
        )
        
    def is_valid(row_dict: dict[str, Any]) -> bool:
        *All present or accounted for?*
        return *state*

    def construct_object(row_dict: dict[str, Any]) -> App_Object:
        return App_Object(\*\*row_dict)

The validation rules rarely change. The object construction doesn't always
need to be a separate function, it can often be a simple class name, or a
classmethod of the class.

Our sheet processing can include a function like this:

..  parsed-literal::

    builder = make_builder(args)
    for row in sheet:
        intermediate = builder(row)
        if is_valid(intermediate):
            yield construct_object(intermediate)
        else:
            log.error(row)

The ``builder()`` function allows processing to vary with the file's actual schema.
We need to pick the builder based on a "logical layout" command-line option.
Something like the following is used to make an application
flexible with respect to layout.

..  parsed-literal::

    def make_builder(args: argparse.Namespace) -> Callable[[Row], dict[str, Any]]:
        if args.layout in ("1", "D", "d"):
            return builder_1
        elif args.layout == "2":
            return builder_2
        else 
            raise Exception(f"Unknown layout value: {args.layout}")

The builders are tested individually.  They are subject to considerable change.
New builders are created frequently.

The validation should be common to all logical layouts.  
It's not subject to much variation.  
The validation and object construction doesn't have the change velocity that builders have.

Now that we can process individual rows, we need to provide a way to process
the collection of rows in a single sheet.

Sheet-Level Processing
------------------------

For the most part, sheets are  rows of a single logcal type.  In exceptional cases,
a sheet may have multiple types coincedentally bound into a single sheet.
We'll return to the multiple-types-per-sheet issue below.

For the single-type-per-sheet, we have a processing function like
the following.

..  py:function:: process_sheet(sheet, builder)

    Process the given sheet using the given builder.

..  parsed-literal::
        
    def process_sheet(sheet: Sheet, builder: Builder = builder_1) -> Counter:
        counts = Counter()
        object_iter = ( 
            builder(row)
            for row in sheet.row_iter()
        )
        for source in object_iter:
            counts['read'] += 1
            if is_valid(source):
                counts['valid'] += 1
                # *The real processing*
                obj = make_app_object(source)
                obj.save()
            else:
                counts['invalid'] += 1
        return counts

This kind of sheet is tested two ways.  First, this can
have a unit test with a fixture that provides
specific rows based on requirements, edge-cases and other "white-box" considerations.

Second, an integration test can be performed with live data.
The counts can be checked.  This actually tests the file as much as it tests the sheet processing function.

Workbook Processing
---------------------

The overall processing of a given workbook input looks like this.

..  py:function:: process_workbook( source, builder )

    Process all sheets of the workbook using the given builder.

..  parsed-literal::

    def process_workbook(source: Workbook, builder: Builder) -> None:
        for name in source.sheet_iter():
            # *Sheet filter?  Or multi-way elif switch?*
            sheet = source.sheet(name).set_schema_loader(HeadingRowSchemaLoader)
            counts = process_sheet(sheet, builder)
            pprint.pprint(counts)

This makes two claims about the workbook.

-   All sheets in the workbook have the same schema rules.
    In this example, it's an embedded schema in each sheet and the schema is the heading row.

-   A single :py:func:`process_sheet` function is appropriate for all sheets.

If a workbook doesn't meet these criteria, then a (potentially) more complex
workbook processing function is needed.  A sheet filter is usually necessary.

Sheet name filtering is also subject to the kind of change that
builders are subject to.  Each variant logical layout may also have
a variation in sheet names.  It helps to separate the sheet filter functions
in the same way builders are separated.   New functions are added with 
remarkable regularity

..  parsed-literal::
    
    def sheet_filter_1(name: str):
        return re.match(r'*pattern*', name)

Or, perhaps something like this that uses a shell file-name pattern instead of a
more sophisticated regular expression. 

..  parsed-literal::
    
    def sheet_filter_2(name: str):
        return fnmatch.fnmatch(name, '*pattern*')

Command-Line Interface
----------------------

We have an optional argument for verbosity and a positional argument that
provides all the files to profile.

::

    def parse_args():
        parser = argparse.ArgumentParser()
        parser.add_argument('file', nargs='+')
        parser.add_argument('-l', '--layout')
        parser.add_argument('-v', '--verbose', dest='verbosity',
            default=logging.INFO, action='store_const', const=logging.DEBUG )
        return parser.parse_args()

The overall main program looks something like this.

::

    if __name__ == "__main__":
        logging.basicConfig(stream=sys.stderr)
        args = parse_args()
        logging.getLogger().setLevel(args.verbosity)
        builder = make_builder(args)
        try:
            for file in args:
                with workbook.open_workbook(input) as source:
                    process_workbook(source, builder)
            status = 0
        except Exception as e:
            logging.exception(e)
            status = 3
        logging.shutdown()
        sys.exit(status)
        
This main program switch allows us to test the various functions (:func:`process_workbook`, :func:`process_sheet`, the builders, etc.) in isolation.

It also allows us to reuse these functions to build larger (and more complete) 
applications from smaller components.

In :ref:`demo` we'll look at two demonstration applications, as well as a unit
test.


Variant Records and COBOL REDEFINES
====================================

Ideally, a data source is in "first normal form": all the rows are a single type
of data. We can apply a **Build, Validate, Construct** sequence simply.

In too many cases, a data source has multiple types of data. In COBOL files, it's common
to have header records or trailer records which are summaries of the details
sandwiched in the middle.

Similarly, a spreadsheet may be populated with summary rows that must be discarded or
handled separately. We might, for example, write the summary to a different destination 
and use it to confirm that all rows were properly processed.

Because of the COBOL ``REDEFINES`` clause, we have multiple variants within a schema.
The JSONSchema ``oneOf`` keyword captures this. This means that some of the alternatives
may not have a valid decoding for the bytes. This suggests that lazy evaluation of each
attribute of each row is essential.

We'll look at a number of techniques for handling variant records.

Trivial Filtering
------------------

When loading a schema based on headers in the sheet,
the :py:class:`stingray.HeadingRowSchemaLoader` class will be used.
We can extend this loader to reject rows, also.

The :py:meth:`stingray.HeadingRowSchemaLoader.body` method can do simple filtering.
This is most appropriate for excluding blank rows or summary rows from a spreadsheet.


Multiple Passes and Filters
----------------------------

When we have multiple data types within a single sheet, we can process this data
using the **Multiple Passes and Filters** pattern. Each pass through the data
uses different filters to separate the various types of data.

The multiple-pass option looks like this.  Each pass applies a filter and 
then does the appropriate processing.

..  parsed-literal::
        
    def process_sheet_filter_1(sheet: Sheet):
        counts = Counter()
        for source in sheet.row_iter():
            counts['read'] += 1
            if *filter_1(row)*\ :
                intermediate = *builder(row)*
                counts['filter_1/pass'] += 1
                *processing_1(intermediate)*
            else:
                counts['filter_1/reject'] += 1
        return counts

Each filter is a simple boolean function like this.

..  parsed-literal::

    def filter_1(source: Rpw) -> bool:
        return *some condition*
        
The conditions may be small boolean expressions like ``source['column'].value() == value``,
and a lambda object can be used. It's generally a good practice to encapsulate them as distinct, named functions.

One Pass and Case
--------------------

When we have multiple data types within a single sheet,
We can make  single pass over the data, using an ``if-elif`` chain or a ``case-switch`` statement.
Each type of row is handled separately.

The one-pass option looks like this.  A "switch" function is used to 
discriminate each kind of row that is found in the sheet.

..  parsed-literal::
        
    def process_sheet_switch(sheet: Sheet) -> Counter:
        counts = Counter(int)
        for row in sheet.row_iter():
            counts['read'] += 1
            if *switch_1(row)*\ :
                intermediate_1 = *builder_1(row)*
                *processing_1(intermediate_1)*
                counts['switch_1'] += 1
            elif *switch_2(row)*\ :
                intermediate_2 = *builder_2(row)*
                *processing_2(intermediate_2)*
                counts['switch_2'] += 1
            *elif etc.*
            else:
                counts['rejected'] += 1                
        return counts

Each switch function is a simple boolean function like this.

..  parsed-literal::

    def switch_1(row: Row) -> bool:
        return *some condition*
        
The conditions may be trivial: ``source['column'].value() == value``.

It often makes sense to package switch, builder, and processing into a single class.

We may be able to build a mapping from switch function results to process function.
    
This allows us to write a sheet processing function like this>

..  parsed-literal::
        
    def process_sheet_switch(sheet: Sheet) -> Counter:
        counts = Counter()
        for source in sheet.row_iter():
            counts['read'] += 1
            processed = None
            choices: list[tuple[bool, Callable[[Row], None]] = {
                (switch_1(row), builder_1, processing_1),
                (switch_2(row), builder_2, processing_2),
                ...
            )
            for switch, builder_function, processing_function in choices:
                if switch:
                    processed = switch.__name__
                    counts[processed] += 1
                    intermediate = builder_function(row)
                    processing_function(intermediate)
            if not processed:
                counts['rejected'] += 1                
        return counts

This can more easily be extended by adding to the ``choices`` mapping.

More complex pipelines
----------------------

In many cases, we need to inject data quality validation before attempting
to build the application object.
If so, that can be added to the mapping.

It can help to define a class to contain the various pieces of the processing.

..  parsed-literal::

    class Sequence(abc.ABC):
        @abstractmethod
        def switch(self, row: Row) -> bool: ...
        @abstractmethod
        def builder(self, row: Row) -> dict[str, Any]: ...
        @abstractmethod
        def validate(self, dict[str, Any]:) -> bool: ...
        @abstractmethod
        def process(self, dict[str, Any]) -> None: ...

        def handle(self, row: Row) -> str:
            name = self.__class__.__name__
            if not self.switch(row):
                return f"{name}-reject"
            intermediate = self.builder(row)
            if not valid(intermediate):
                return f"{name}-invalid"
            self.process(intermediate)
            return f"{name}-process"

    class Record_Type_1(Sequence):
        def switch(self, row: Row) -> bool:
            return *some expression*
        def builder(self, row: Row) -> dict[str, Any]: ...
            return {
                *name* = row[*column*].value(),
                ...
            }
        def validate(self, intermediate: dict[str, Any]) -> bool:
            return *some expression*
        def process(self, intermediate: dict[str, Any]) -> None:
            *do something*

    OPTIONS = [Record_Type_1(), Record_Type_2(), ...]

This serves as the configuration for a number of processing alternatives.
New classes can be added and the ``OPTIONS`` list updated to reflect the current
state of the processing.

..  parsed-literal::

    def process_sheet_switch(sheet: Sheet) -> Counter:
        counts = Counter()
        for source in sheet.row_iter():
            counts['read'] += 1
            processed = None
            for option in OPTIONS:
                outcome = option.handle(source)
                counts[outcome] += 1
        return counts

This generic sheet processing can comfortably handle complex variant row
issues. It permits a single configuration via the ``OPTIONS`` sequence
to handle records appropriately.

This design permits the switch conditions to overlap, potentially processing
a single row multiple times. If the conditions do not overlap, then the first
outcome that ends in "-process" would exit the loop.

..  parsed-literal::

    for option in OPTIONS:
        outcome = option.handle(source)
        counts[outcome] += 1
        if outcome.endswith("-process"):
            break

With this additional feature, the order of the conditions in the ``OPTIONS`` list becomes
relevant. A general, fall-back ``switch()`` method condition must be last.

Big Data Performance
=====================

We've broken appllication processing down into separate steps which
work with generic Python data structures. This permits use of
multiprocessing to spread the pipeline into separate processors or cores.

We'll set aside the initial switch decision-making for a moment and
focus on a three step **Build, Vaidate, Process** sequence of operations.
Each stage of of this sequence can be processed concurrently.

The **Build** stage uses a Sheet object'ss ``row_iter()`` method to gather
``Row`` objects. These can be validated and an intermediate object created
and placed into a queue for processing.

The **Validate** stage dequeues intermediate objects, performs the validation
checks, and enqueues only valid objects for processing.

The **Process** stage dequeues intermediate objects and processes them.
There can be a pool of workers doing this in case the processing is very time-consuming.

This is amenable to asyncio, also. In that case, the final processing
would be a threadpool instead of a process pool. When using ``ayncio`` it's
critical to avoid updates to shared data structures. In the rare case when
this is required, explicit locking will be required and can stall the async pipeline.

File Naming and External Schema
===============================

Some data management discipline is needed be sure that the schema and file match
up properly.  Naming conventions and standardized directory structures are
*essential* for working with external schema. 

Well Known Formats
--------------------

For well-known physical formats (:file:`.csv`, :file:`.xls`, :file:`.xlsx`, :file:`.xlsm`, :file:`.ods`,
:file:`.numbers`) the filename extension describes the physical format. Additional
information is required to determine the Logical Layout.

The schema may be loaded from column headers, in which case the binding is handled 
via an embedded schema loader. If the  :py:class:`stingray.HeadingRowSchemaLoader`
is used, no more information is required. If an external schema loader is used
(because the headings are not part of the sheet), then we must
bind each application to the appropriate external schema for a given file.

When the schema is external, the schema will often require a unique meta-schema.
This means a data file must be associated with a schema file and a schema loader
for the schema.

File naming rules don't often work out for this, and some kind of explicit
configuration file may be required. In some cases, the directory structure
can be used to associate data files and schema files and meta-schema.

Fixed Formats and COBOL
------------------------

For fixed-format files,
the filename extension does **not** describe the physical layout.
There is not widely-used extension for fixed-format files. A suffix like ``.dat`` is uninformative.
Making things slightly sompler, a fixed format schema combine logical layout and physical format into
a single description. 

For fixed format files, the following conventions help
bind a file to its schema.

-   The data file suffix should be the base name of a schema file.
    For example, :file:`mydata.someschema` points to the :file:`someschema.cob` or
    :file:`someschema.json` schema.

-   Schema files must be be either JSONSchema, a COBOL DDE file, or a
    workbook in a well-known format. For example
    :file:`someschema.cob` or :file:`someschema.xlsx`.
    
**Examples**.  We might see the following file names.

.. parsed-literal::

    september_2001.exchange_1
    november_2011.some_dde_name
    october_2011.some_dde_name
    exchange_1.xls
    some_dde_name.cob
    
The ``september_2001.exchange_1`` file is a fixed format file 
which requires the ``exchange_1.xls`` metadata workbook. The metadata workbook should have
an easy-to-understand schema, ideally a heading row.

The ``november_2011.some_dde_name`` and ``october_2011.some_dde_name`` files
are fixed format files which require the ``some_dde_name.cob`` metadata.

External Schema Workbooks
-------------------------

A workbook with an external schema sheet must adhere to a few conventions to be usable.
These rules form the basis for the :py:class:`stingray.ExternalSchemaLoader`
class. To change the rules, extend that class.

The metaschema is defined in the class-level ``META_SCHEMA`` variable. This is a
JSONSchema definition with the following properties:

-   The column names "name", "description", "dataType" are used.

-   Additional columns are allowed, but will be ignored.

-   Type definitions are the JSONSchema values: "string", "number", "integer", and "boolean".

For simple column name changes, the ``META_SCHEMA`` can be replaced. For more complex changes,
the class will need to be extended.

Binding a Schema to an Application
====================================

We would like to be sure that our application's expectations for a
schema are aligned with the schema actually present.
An application has several ways to bind its schema information.

-   **Implicitly**.  The code simply mentions specific columns
    (either by name or position). If the schema definition doesn't match the code
    there will be run-time ``KeyError`` exceptions.
    
-   **Explicitly**. The code has a formal "requires" check to be sure
    that the schema provided by the input file actually matches the 
    schema required by the application.

The idea of explicit schema  parallels the configuration management issue of module
dependency. A data file can be said to *provide* a given schema and an
application *requires* a given schema.

An explicit check is far from fool proof. It's -- at best -- a minimal confirmation
that an expected set of attributes are present.

..  parsed-literal::

    valid = all(
        req in schema for req in ('some', 'list', 'of', 'required', 'columns')
    )
    
This is essential when using a spreadsheets heading row as a schema.

A better approach is to have an expected schema. We can then compare the schema built by the heading
row with the expected schema. A heading row schema has no data type or conversion information,
making it inadequate for most applications.

..  parsed-literal::

    valid = all(
        prop_name in found_schema.properties for prop_name in expected_schema.properties
    )

This assures us that the heading row schema found in the file includes the expected schema.
It may have additional columns, which will be ignored.

The more complete check is row-by-row data validation. This is often necessary.
We'll turn to data validation below.

Schema Version Numbering
=================================

JSONSchema and XSD's can have version numbers.  This is a very cool.

See http://www.xfront.com/Versioning.pdf for detailed discussion of how
to represent schema version information.

Databases, however, lack version numbering in the schema.  This leads to potential
compatibilty issues between application programs that expect version 3 of the
schema and an older database that implements version 2 of the schema.

Our file schema, similarly, don't have a tidy, unambiguous numbering.

For external schema, we can embed the version in the file names.
We might want to use something like this ``econometrics_vendor_1.2``.
This identifies the 
generic type of data, the source for that file, and the schema version
number. 

    Within a SQL database, we can easily use the schema name to carry
    version information.  We could have a :samp:`name_{version}` kind of
    convention for the database schema objects that contain our tables.
    This allows an application to confirm schema
    compatibility with a trivial SQL query.

For embedded schema in a spreadsheet, however, we have no *easy* way to provide schema identification
and version numbering.  We're forced to 
build an algorithm to examine the actual names in the embedded schema to deduce
the version.  

This problem with embedded schema leads to using data profiling to reason out what the file is.  
This may devolve to a manual examination
of the data profiling results to allow a human to determine the schema.
Then, once the schema has been identified, command-line options
can be used to bind the schema to file for correct processing.

Frequently Asked Questions
==========================

Junk Data
----------

For inexplicable reasons, we can wind up with files that are damaged in some way.

    "there is a 65-byte "header" at the start of the file, what would be the best 
    (least hacky) way to skip over the first 65 bytes?"
    
This is one of the reasons why use both a file name and an open file object as
arguments for opening a workbook.

..  parsed-literal::

    path = Path("file_with_junk.some_schema")
    with path.open(,"rb") as cobol:
        cobol.seek(66)
        wb = stingray.COBOL_EBCDIC_File(path, file_object=cobol)
        
This skips past the junk.

US ZIP Codes
------------

Spreadsheets turn US Zip codes into numbers, and the leading zeroes
get lost.

For this, we have conversion functions like ``stingray.digits_5()`` to
turn an integer into a 5-position string with leading zeroes.

Currency
--------

Spreadsheets turn currency into floating-point numbers.
Any computation can lead to horrible '3.9999999997' numbers instead of '4.00'.

For this se have a ``stingray.decimal_2()`` conversion function to
provide a decimal value rounded to two decimal places. When this is done
as early in the processing as possible, currency computations work out nicely.
