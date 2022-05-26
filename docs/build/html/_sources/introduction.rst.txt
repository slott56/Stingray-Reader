
..  _`intro`:

#################
Introduction
#################

Given a workbook -- or any other "flat" file -- how is it organized?  What does it *mean*?

How can we ignore details that are merely physical?
When is a change a logical layout variant with no semantic impact?
How do we isolate ourselves from logical and physical variations?

There are two user stories that provide a useful context.

-   **Extract Transform and Load (ETL)**.   We've got a file of data
    in the form of a workbook file or perhaps a flat (delimiter-free) COBOL file.
    We want to extract this data into some more useful form, e.g., native Python objects.

-   **Analysis**. We've got a file of data and we want to
    examine it for frequencies, ranges and relationships. There are two sub-cases:
    exploratory data profiling, as well as data validation. For the profiling
    case, there's no prior expectation. For the validation case, there's a set of
    criteria which must be met for the data to be valid.

We don't want application software to depend on the physical file format.
We want to work equally well with CSV or XLSX versions of a file.

We'd like something like this to transparently handle workbook files in a variety of formats.

..  parsed-literal::

    def process_sheet(sheet: Sheet) -> Counter:
        counts = Counter()
        for rows in sheet.rows():
            counts['input'] += 1
            *process row*
        return counts

The idea here is to properly isolate processing centered on a single sheet of w workbook.
It's often the case that a single sheet can be created in First Normal Form (1NF). This
means each field is atomic, and there are no repeating arrays within the row.

Actual data can, of course, be much more complex. The conceptual model, however, seems
to be useful for structuing data-intensive applications.

This sheet-level function is used by a main application function.

..  parsed-literal::

    def main(args: argparse.Namespace) -> None:
        for input in args.file:
            with stingray.open_workbook(input) as source:
                for sheet in source.sheets():
                    sheet.set_schema_loader(stingray.HeadingRowSchemaLoader())
                    counts = process_sheet(sheet)
                    pprint(counts)

This describes a process for handling all sheets of all of the files provided
on the command line. Some applications may be focused on a single sheet of a
single file, reducing the nested file and sheet processing.

The ``HeadingRowSchemaLoader`` class extracts the first row of each sheet
as the defacto schema for that sheet. Given only column names, the data types must
be assumed or conversion functions provided as part of the application to
assure that the column contents are -- indeed -- objects of useful Python clsses.

This schema loader parallels the way ``csv.DictReader`` extracts the first row as the keys
for each row's dictionary. It's explicit, however, permitting change for exceptional cases.
For example, a spreadsheet with complex, multi-row titles. These require a more sophisticated parser
that consumes multiple rows of heading.

In the most complex cases, multiple independent "sections" of data are present in a
single spreadsheet. This kind of data requires a "heading parser" that can decompose the sheet into sections
by locating headings and bodies.

While heading rows are common, it's also possible to have a
a separate sheet or a separate workbook with column definitions.
External schema for a spreadsheet or file may be another simple spreadsheet.

For some kinds of documents, the schema may be written as a JSONSchema document.
For COBOL files, the schema is often DDE (Data Definition Entry) in a "Copybook".
Stingray Reader can parse COBOL and create the needed JSONSchema document.

These issues are the source of numerous complications in otherwise simple applications.
The application processing doesn't vary, but dealing with various file and workbook layouts
adds a distracting (and irrelevant) layer of processing to an application.

Design Patterns
===============

We want to build applications that are flexible with respect to the
the logical layout of columns.  We'd like to be adaptable to changes to the names, number,
order and type of columns without any revision to the application software.

The core design pattern is the **Builder Function**.
These functions isolate the
logical layout from the rest of the application and do nothing more.

Here's an example.

..  parsed-literal::

    def build_record_dict(aRow: Row) -> dict[str, Any]:
        return dict(
            name=aRow['some column'].value(),
            address=aRow['another column'].value(),
            zip=digits_5(aRow['zip'].value()),
            phone=aRow['phone'].value(),
        )

The column names and data conversions are isolated to this
function only.  As the source schemata evolve, we can update an
application to add variations on this function.

The ``digits_5()`` function is necessary to cope with US ZIP codes.
These are digit strings. Spreadsheet software often transforms them into
integers. The ZIP codes in the northeast which begin with zero become
four-digit numbers. For example, ``01020`` would become 1020 in the spreadsheet.
The ``digits_5`` function recovers the original ZIP code.


A ``NamedTuple`` or a ``dataclass`` is often more useful than the ``dict[str, Any]`` structure.
We can transform a generic ``dict[str, Any]`` into something more useful like this:

..  parsed-literal::

    class SomeObject(NamedTuple):
        name: str
        address: str
        zip: str
        phone: atr

        @classmethod
        def from_dict(cls: type[SomeObject], record_dict: dict[str, Any]) -> SomeObject:
            return SomeObject(\*\*record_dict)

We've explicitly divorced the application object from the source file format using
a Python intermediary. The row dictionary is a necessary overhead to assure that
changes in the source or the application processing are isolated from each other.

A useful class is the composite of the generic builder and the specific
class conversion. We can combine the two steps like this:

..  parsed-literal::

    class SomeObjectSource:
        @staticmethod
        def build_record_dict(aRow: Row) -> dict[str, Any]: ...

        @staticmethod
        def object_iter(source: Iterable[Row]) -> Iterator[SomeObject]:
            for row in source:
                rd = SomeObjectSource.build_record_dict(row)
                yield SomeObject.from_dict(rd)

This design breaks processing into two parts. The logical layout mapping
from workbook rows to Python objects is never trivial.
The implmentaton of ``build_record_dict()`` is subject to change
with minimal notice. Transforming this to the useful ``SomeObject`` class
is often trivial. It helps to keep it separate.

This can be restated as a sequence of generator expressions.
This form is sometimes helpful for visualizing the stages in
the processing,

It looks like this:

..  parsed-literal::

    class SomeObjectSource_2:
        @staticmethod
        def build_record_dict(aRow: Row) -> dict[str, Any]: ...

        @staticmethod
        def object_iter(source: Iterable[Row]) -> Iterator[SomeObject]:
            dict_gen = (
                SomeObjectSource_2.build_record_dict(row)
                for row in source
            )
            object_gen = (
                SomeObject.from_dict(rd)
                for rd in dict_gen
            )
            return object_gen

Experience indicates that it's best to factor the input processing into at least two discrete
steps so that transformations are easier to manage and extend.

Additional steps often accrue as the application evolves. Alternatives steps accrus to support
new or modified data sources.

We can then use this iterator to process rows of a sheet.

..  parsed-literal::

    def process_sheet(self, source: SomeObjectSource, sheet: Sheet) -> Counter:
        counts = Counter()
        for some_object in source.object_iter(sheet.rows()):
            counts['input'] += 1
            *process the* ``SomeObject`` *instance*
        return counts

We'll show more concrete implementation examples in the :ref:`demo` section.

Deeper Issues
=============

Processing a workbook (or other flat file) means solving three closely-related schema problems.

-   Unpacking the *Physical Format*. We need to unpack bytes into Python objects (e.g., decode a string to lines
    of text to atomic fields). We need a **Facade** over the various workbook libraries
    to make Physical Format transparent to our applications.

-   Mapping to the *Logical Layout*. Locate the values within structures that may not
    have perfectly consistent ordering. A CSV file with column headers, for example,
    can be treated as a dictionary, making the column order irrelevant. If a schema is
    **always** used, then the Logical Layout becomes transparent to our application.

-   Understanging the *Conceptual Content*.  That is, the semantic mapping from Python
    items (e.g., strings) to meaningful data elements in our problem domain (e.g., customer zip codes.)

The physical format issue is addressed by a **Facade** that uses the well-known (or even standardized) file formats:
CSV, XLSX, XML, JSON, YAML, TOML, etc., can all be parsed readily. Numbers files can be parsed, but this
format requires some extra work because it's not standardized. Traditional .XLS files, also, are highly proprietary.
We need to include COBOL files. In many cases, they will parallel workbooks. COBOL files introduce some unique complexities.

The logical layout issue is not as easy to address as the physical format issue.
Here are some root causes for Logical Layout problems:

-   **Bullfeathers**.  Also known as *Semantic Heterogeneity*.
    You call them "customers" because they have
    a contract and pay money for services.  Marketing, however, calls their
    prospects "customers" even though there is no contract in place. 
    Same word.  Different semantics.
    
    Yes, this a "problem domain" issue.  No, there's no technical solution short
    of a complete ontology for each data item.

    We'll need a design with enough flexibility to handle semantic matching.

-   **Buffoonery**.  Is "CSTID" the "customer id number"?
    Or is it the "Commercial Status ID"?  Or is it the "Credit Score
    Time Interval Delta"?  Or is it something entirely unrelated that merely
    happens to be shoved into that field?

    Yes, this is evidence of "code rot."
    Even with careful format definitions, this kind of thing happens as software matures.
    No, there's no technical solution short of firing all the managers who make short-sighted decisions.
    
    We'll need a design that has the flexibility to cope with variant abbreviations for column names.

-   **Bad Stewardship**.  At some point in the past, the "Employees Here" and "Total Employees"
    were misused by an application.  The problem was found--and fixed--but
    there is some "legacy" data that has incorrect values.  What now?
    
    Yes, this is a data stewardship problem. No, you can't restate data you don't have.

    This leads to rather complex designs where the mapping from source to target
    is dependent on some external context to understand the source data.

-   **Bugs**.  The field named "Effective Date" is really the *Reported Date*.
    The field name "Supplied Date" is really the *Effective Date* which can be prior to the reported date.
    The field labeled "Reported Date" isn't populated consistently and doesn't
    seem to have any meaning.  Really.
    
    Is this "technical debt"? Or is it "code rot"? Does it matter?
    
    We need flexibility to handle bugs that lead to data problems.

The point here is that there is an underlying *Conceptual Schema*.  It often has numerous
variant implementations, each with a unique collection of errors and anomalies.

Misdirections
-------------------

We have an additional consideration when it comes to data conversions. 
We have to avoid the attractive nuisance of a Domain Specific Language (DSL) 
for mappings and conversions.

There's no value in creating a new mapping language. This is bad:

..  parsed-literal::

    target-doc:: source-doc WITH headers=embedded FORMATS: format-1, format-2
    name: FROM 'some column' IN format-1 AS string, FROM 'some column' IN format-2 AS string
    address: FROM 'another column' IN format-1 AS string, FROM 'some column' IN format-2 AS string
    zip: FROM 'zip' IN format-1 AS string WITH digits_5, FROM 'zip-5' IN format-2 AS string
    phone: FROM 'phone' IN format-1 AS string, FROM 'phone' IN format-2 AS string

We don't need a cumbersome language outside Python.

This kind of DSL fails when we have data structures more complex than simple spreadsheet rows.
When we  work with COBOL or Fixed Format files, we find these files are not in First Normal Form.
COBOL files have repeating groups which require numeric indexes in addition to column names.

For semi-structured data (JSON, YAML, TOML, XML, etc.) there are fewer
constraints on the data, leading to an even more complex data normalization step and possible
row validation rules. We'd like to retain a relatively simple schema 
in spite of the potential complexity of these files.  A DSL would devolve to Python-like
functionality to work with these formats.

The :py:mod:`csv` approach of **eagerly** building a row from the raw bytes doesn't work
for COBOL files because of the ``REDEFINES`` clause.  We can't reliably
build the various "cells" available in a COBOL schema, since some of
those values may turn out to be invalid. COBOL requires lazily building a row
based on which REDEFINES alias is relevant.

Historical Solutions
=======================

    "Those who cannot remember the past are condemned to repeat it."
    --George Santayana

We'll look at four solutions in their approximate historical order.

The `COBOL Schema Solution`_  is still relevant
to modern Python programmers.

The `DBMS Schema Solution`_ is available, but isn't compeletly universal.
It doesn't apply well to files outside the database.

The `CSV Schema Solution`_ often introduces more problems than it solves.

There is an `XML Non-Solution`_.  While XML is relevant, it is not a *universal* solution
that some folks seem to hope for.  At best, it offers us XSD, which may be too sophisticated 
for the problem we're trying to solve.

The `JSONSchema Approach`_. The JSONSchema standard is, perhaps, more useful than XSD
as a schema definition. Mostly because JSONSchema surfaces in OpenAPI specifications and
document data stores.

For semi-structured data (JSON, YAML and outlines), we need more than a simple
schema definition. We need processing rules to reformat and validate the inputs
as well.

COBOL Schema Solution
------------------------

A significant feature of the COBOL language is the Data Definition Entry (DDE)
used in the data and environment divisions of the COBOL source.  This is a hierarchical
structure that defined the various items in a single record of a file.

Hierarchical.  Like XML.

COBOL best practice was essentially DRY:
developers would keep the definitions as separate modules
under source code control.
Every application that worked with a given file would import the DDE for
that file. This was done via the COPY keyword. The modules were called "copybooks".

Clearly, the binding between schema and file is a two-step operation.
There's a compile-time binding between schema and application.  There's a
run-time binding between application and file.

Just as clearly, this is subject to all kinds of mysterious problems when
schema definition modules are cloned and then modified, leaving it unclear
which version is correct.  Also, when a schema definition was modified and not
all programs were properly recompiled, some programs will worked with some
files, other programs won't.

Since a schema isn't formally bound to a given file, it becomes particularly easy
to have files without any known schema.  Ideally, the file name included
some schema hint.

What's relevant for Python programmers is the central idea.

    **A schema is external to any specific application.**

To this, we would like to assure that the schema was bound to the relevant
files.  This is much more difficult to achieve in practice, but there are some
approaches that can work through manually managing file names.

DBMS Schema Solution
------------------------

A Database Management System (DBMS) -- whether relational or hierarchical
or columnar or networked or whatever -- addresses the problems with
flat files and the separation between application program, physical format,
logical layout, and operating system file.
A DBMS provides a complete logical/physical schema separation with mappings among the layers.

The physical files are managed by the DBMS.  Our applications are now
independent of all physical file structure. They're often independent of
OS considerations, too.

The logical "table structure" (or "document" or whatever is offered) is distinct
from the underlying files.  The logical schema it tightly bound to
the data itself.  When using SQL, for example, the column names and data types
are available as part of the execution of each SQL query.

    **A schema is bound to a file.**

Sadly, it doesn't apply to individual files floating around on file systems.
It only works for the database as an opaque manager of "storage."
The idea of schema-bound-to-file is an aspiration that's difficult to implement.

If file transfers are replaced with SQL queries (or web services requests)
then schema becomes discoverable from the database (or web service).
Using web services has a lot of advantages over file transfers. However, we live
in a file-transfer world, and we need to manually bind a schema to a file.

CSV Schema Solution
-------------------------

A workbook (or "spreadsheet") may or may not have schema
information inside it. There may be a header row, a separate sheet, or a separate
document. The ``csv`` module makes good use of the header row as a source of a schema.

When this is done consistently, it's a way to bind the schema to the data.

While widely used, it suffers from some problems:

-   Column titles may be omitted.
    Sometimes the titles may span multiple rows.
    Sometimes the sheet itself has a heading/body format where there's
    irrelevant rows which must be filtered out.

-   This is subject to all kinds of buffoonery.  Column titles can
    be tweaked manually.

-   The column title provides no usable type information.

    Exacerbating this is the way that anything number-like becomes a floating-point
    number.  Zip codes, punctuation-free phone numbers, social security numbers,
    etc., all become floating-point numbers.
    This means they lose their leading zeroes, making zip-code matching particularly painful.
    Currency devolves to floating point, making it inaccurate.

A less common solution is to include a separate sheet in the workbook (or worse, a separate file)
with schema information. These are not a first-class part of the ``csv`` modue.
A separate sheet in a workbook is at least bound
with the data.  A separate schema description file (even if bound in a ZIP archive) can get
unbound from the data.

While there are numerous problems, workbooks are a very common way to exchange
data.  It's not sensible to pretend they don't exist. 

..  warning::

    "Some people, when confronted with a problem, think 'I know,
    I'll use [a spreadsheet].'  Now they have two problems."

    Jamie Zawinski
    
We do need to process this data irrespective of the issues.


XML Non-Solution
---------------------

XML fans sometimes claim XML is "self-defining". It's not clear what this is supposed to mean.
XML is a physical format with delimiters.
An XML document without a referenced (or embedded) XSD lacks any semantic information.

The XSD associated with an XML document provides the needed schema definition.
It can be bound to the XML file, which is desirable. Since the XSD is also in XML,
a meta-schema lets us extract the XSD before unpacking the data.

We can -- to an extent -- leverage elements of PyXSD (http://pyxsd.org)
to create Python classes from an XSD schema.  This package could
help by providing a standard class structure for a schema
defined in Python.  In particular, the ``pyxsd.schema`` module contains
some of what we're going to use.

XSD seems a bit too complex for this problem domain. It seems awkward
to extract XSD from the XML context and apply it to workbooks and COBOL files.

JSONSchema Approach
-------------------

The JSONSchema standard provides a schema definition. See https://json-schema.org.

This is leveraged by OpenAPI specifications. See https://swagger.io/specification/.

Our objective is to leverage JSON Schema schema definitions to cover Spreadsheet Workkbooks
as well as COBOL files. This requires a few extensions to cover the details of non-delimited
physical formats.

We can convert XSD to JSON Schema. https://github.com/benscott/xsdtojson.

We can convert SQL DDL statements to JSONSchema. https://github.com/shinichi-takii/ddlparse

The Stingray Reader converts COBOL to JSONSchema.

This lets us use JSONSchema as a common schema definition. We can import the schema into our applications,
and we can -- with some discipline -- make sure the schema definitions are bound to our data files.

Summary
-------------------------

Physical format independence is available with some file formats.
Sadly, others -- which are still in active use -- require a great deal
of schema information merely to decode the physical format.

Logical layout is generally a feature of the application program
as well as the data.  In a SQL-based data access, the column
names in a ``SELECT`` statement amount to a binding between application and schema.

While we can make some kinds of simple
applications which are completely driven by metadata, we can't easily
escape the need to customize and deal with variations.
Therefore, we need to have application programs which can
tolerate changes without requiring a rewrite.

We would like an application program that can work with
"minor" variations on a logical layout.  That is, the order
of columns, or minor spelling changes to a column name can be
handled gracefully.

We'd like our batch processing applications to have a command-line
interface something like this.

..  code-block:: bash

    python -m some_app -l layout_2 some_file.xyz

The ``-l layout_2`` provides logical layout information. This defines the "application-level" schema information.

The ``some_file.xyz`` could be ``some_file.xls`` or ``some_file.ods``,
allowing transparent changes to physical format.

