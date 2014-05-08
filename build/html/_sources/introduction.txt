
..  _`intro`:

#################
Introduction
#################

Given a workbook -- or any other "flat" file -- how is it organized?  What does it *mean*?

How can we ignore details that are merely physical?
When is a change merely a logical layout variant with no semantic impact?
How do we isolate ourselves from these layout variants?

There are two use cases that we want to focus on.

-   **Extract Transform and Load (ETL)**.   We've got a file of data
    in the form of a workbook ("spreadsheet") file or perhaps a flat (delimiter-free) COBOL file.

-   **Analysis**.  Also known as Data Profiling.  We've got a file of data and we want to
    examine the data for frequencies, ranges and relationships.
    A common use for this is to "validate" or "unit test"
    a file before attempting to process it.

In these cases, we don't want our applications to depend on
the physical file format.  We want to work equally well with CSV or XLSX
versions of a file.

We'd like something like this to transparently handle workbook files in a variety of formats.

..  parsed-literal::

    def process_sheet( sheet ):
        """Separated to facilitate unit testing"""
        counts= defaultdict( int )
        for rows in sheet.rows():
            *process row*
        return counts

    def main(args):
        for input in args.file:
            with workbook.open_workbook( input ) as source:
                for name in source.sheets():
                    sheet= source.sheet( name,
                        sheet.EmbeddedSchemaSheet,
                        loader_class=schema.loader.HeadingRowSchemaLoader )
                    counts= process_sheet( sheet )
                    pprint.pprint( counts )

Note that this specifies a number of things explicitly.

-   :samp:`for name in source.sheets()` claims that *all* sheets have valid data.
    When this is not the case, more sophisticated name filtering
    needs to be inserted into this loop.

-   :py:class:`sheet.EmbeddedSchemaSheet` states that the sheet has the schema
    embedded in it.  For example, the headers in the first row of the sheet. 
    We can use :py:class:`sheet.ExternalSchemaSheet` to specify
    that an external schema must be loaded. For example, a separate sheet in this
    workbook or perhaps a separate file.
    
-   :py:class:`schema.loader.HeadingRowSchemaLoader` states that the schema is
    the first row of the sheet.  We can write other parsers for other sheets
    with more complex or atypical layouts.

Beyond physical format transparency, we want applications that are flexible with respect to the
the logical layout.  We'd like to be adaptable to changes to the names, number,
order and type of columns without a significant rewrite.   We'd like
to isolate the column names or positions from the rest of our application processing.

We can do this by definining  small "builder" functions which isolate the
logical layout from the rest of the application.

We'd like something like this.

..  parsed-literal::

    def build_record_dict( aRow ):
        return dict(
            name = aRow['some column'].to_str(),
            address = aRow['another column'].to_str(),
            zip = aRow['zip'].to_digit_str(5),
            phone = aRow['phone'].to_digit_str(),
        )

The column names and data conversions are isolated to this
function only.  As the source schemata evolve, we can update an
application to add variations on this function.

From this, we can build Python objects using the following:

..  parsed-literal::

    def build_object( record_dict ):
        return ObjectClass( \*\*row_dict )

We can combine the two steps like this:

..  parsed-literal::

    def object_iter( source ):
        for row in source:
            rd= self.build_record_dict( row )
            yield self.build_object( rd )

This design breaks processing into two parts because the logical layout mapping 
from workbook rows to Python objects is never trivial.  This is subject to change
with minimal notice.
Experience indicates that it's better to break the processing into two discrete 
steps so that transformations are easier to manage and extend.

We can then use this iterator to process rows of a sheet.

..  parsed-literal::

    def process_sheet( sheet ):
        counts= defaultdict( int )
        for app_object in object_iter( sheet.rows() ):
            *process the app_object*
        return counts


Deeper Issues
----------------

Processing a workbook (or other flat file) means solving two closely-related schema management problems.

-   Decoding the "Physical Format".  Format is the organization of bytes from
    which we can decode Python objects (e.g., decode a string to lines
    of text to atomic fields).  A workbook should make the physical format irrelevant.

-   Mapping to the "Logical Layout". That is, the semantic mapping from Python
    items (e.g., strings) to meaningful data elements in our problem domain (e.g., customer zip codes.)

Both of these are implementation details for the *Conceptual Content*.  The
conceptual schema is the *meaning* behind the encoded data.

Often, the physical format issue is addressed by using a well-known (or even standardized) file format:
CSV, XLSX, XML, JSON, YAML, etc.
We'll start with just the workbook formats CSV, XLSX and ODS.

We'll ignore JSON and YAML for the time being. We should be able to extend our
model from highly structured data to semi-structured data, including JSON, YAML,
and even documents created with outliner tools.

The logical layout issue is not as easy to address as physical format.  Why not?

-   **Bullfeathers**.  Also known as Semantic Heterogeneity.
    You call them "customers" because they have
    a contract and pay money for services.  Marketing, however, calls their
    prospects "customers" even though there is no contract in place.

    Same word.  Different semantics.
    Yes.  It's a "problem domain" issue.  No, it's not solvable.

    All we can do is cope. This means having enough flexibility to handle
    the semantic issues.

-   **Buffoonery**.  Is "CSTID" the "customer id number"?
    Or is it the "Commercial Status ID"?  Or is it the "Credit Score
    Time Interval Delta"?  Or is it something entirely unrelated that merely
    happens to be shoved into that field?

    We'll need a design that has the flexibility to cope with variant abbreviations for column names.

-   **Bad Management**.  At some point in the past, the "Employees Here" and "Total Employees"
    were misused by an application.  The problem was found--and fixed--but
    there is some "legacy" data that has incorrect data.  What now?

    This leads to rather complex designs where the mapping from source to target
    is dependent on the source data itself. Or the context for the source data.

-   **Bugs**.  The field named "Effective Date" is really the *Reported Date*.
    The field name "Supplied Date" is really the *Effective Date*.
    The field labeled "Reported Date" isn't populated consistently and doesn't
    seem to have any meaning.  Really.

    This is also a kind of mapping issue. It's analogous to the "Buffoonery" issue, above. Flexible mappings are essential.

There is an underlying *Conceptual Schema*.  However, it has numerous
variant implementations, each with a unique collection of errors and anomalies.

Directions
-------------

We need to have a representation for a schema that allows an application
*some* flexibility.
We have four plus one views of an acceptable solution to the schema
representation problem.

-   The use cases need to make sense to the actors.
    To retain the user's terminology, a data dictionary can help interpret a use case in spite of the
    user's imprescise terminology.
    Additional aliases for names and descriptions will be essential.

-   The logical view needs to focus on
    the problem domain, eschewing physical format issues.
    (If mentioned at all, format should
    be little more than a stereotype attached to a class model.)
    The logical view must allow variability in the logical layout of the workbook or file.

-   The deployment view should allow multiple applications to properly
    follow the Don't Repeat Yourself (DRY) principle and share a schema definition.
    Schemata are precious, centralized components which bridge
    data and processing.
    If a schema is bound with the data (to the extent possible), then it can be reused more effectively

-   The processing view should allow a single application program
    to work sensibly with files that have the same
    logical layout but different physical formats.  A single conceptual
    schema may have variant logical layouts (e.g., column name spelling changes).
    This layout variants should be tolerated by a single application.

-   The physical view is focused on file processing: either workbooks or
    flat data files.

Misdirections
-------------------

We have to be cautious of trying too hard to leverage the :py:mod:`csv` module's
row-as-dictionary view of data.  Above, we suggested that the :py:mod:`csv` representation
was ideal.

The :py:class:`csv.DictReader` approach -- implicitly creating a dict instead of a sequence -- 
fails us when we have to work with COBOL or Fixed Format files.
These non-spreadsheet files may not be close to First Normal Form.
COBOL files have repeating groups which
require numeric indexes in addition to column names.

For semi-structured data (JSON, YAML our an outline) there are fewer
constraints, leading to a more complex normalization step and possible
row validation rules.

Further, the :py:mod:`csv` approach of **eagerly** building a row doesn't work
for COBOL files because of the ``REDEFINES`` clause.  We can't reliably
build the various "cells" available in the COBOL schema, since some of
those values may turn out to be invalid. COBOL requires lazily building a row
based on which REDEFINES alias is relevant.

We also have to avoid the attractive nuisance of trying to create a
"data mapping DSL".  This is seductive because a data mapping
is a triple of target, source and conversion.

..  parsed-literal::

    target = source.conversion()

Since this is -- effectively -- Python code, there's no real reason
for creating a DSL.  Just use Python.

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

There is an `XML Non-Solution`_.  While XML is relevant, it is not the *trivial* solution 
that some folks seem to hope for.  At best, it offers us XSD, which may be too sophisticated 
for the problem we're trying to solve.

For semi-structured data (JSON, YAML and outlines), we need more than a simple
schema definition. We need processing rules to reformat and validate the inputs
as well.

COBOL Schema Solution
------------------------

A significant feature of the COBOL language is the Data Definition Entry (DDE)
used in the data and environment divisions of the source.  This was a hierarchical
structure that defined the various items in a single record of a file.

Hierarchical.  Like XML.

Best practice was essentially DRY.
Developers would keep the definitions as separate modules
under ordinary source code control.

Every application that worked with a given file would import the DDE for
that file.

Clearly, the binding between schema and file is a two-step operation.
There's a compile-time binding between schema and application.  There's a
run-time binding between application and file.

Just as clearly, this is subject to all kinds of mysterious problems when
schema definition modules where cloned and then modified, leaving it unclear
which is correct.  Also, when a schema definition was modified and not
all programs were properly recompiled.

Since the schema wasn't formally bound to the file, it was particularly easy
to have files without any known schema.  Ideally, the file name included
some schema hint.

What's relevant for Python programmers is the central idea of a schema
being

a.  External to any specific application.
b.  DRY.

To this, we would like to assure that the schema was bound to the relevant
files.  This is much more difficult to achieve in practice, but there are some
approaches that can work well.

We'll look at this closely, below.

DBMS Schema Solution
------------------------

A Database Management System (DBMS) -- whether relational or hierarchical
or columnar or networked or whatever -- addresses the problems with
flat files and the separation between application program, physical format,
logical layout, and operating system file.

The DBMS should provide us a logical/physical schema separation

The physical files are managed by the DBMS.  Our applications are now
independent of physical file structure (and indepenent even of OS.)

The logical "table structure" (or whatever is offered) is distinct
from the files.  The logical schema it tightly bound to
the data itself.  When using SQL, for example, the column names and data types
are available as part of the execution of each SQL query.

This binding between data is schema is ideal.

Sadly, it doesn't apply to files.  Only to databases as a whole.

If file transfers are replaced with SQL queries (or web services requests)
then schema is discoverable from the database (or web service).  However, the
problem we're addressing here is file transfer.  So this
solution is inappropriate except as a guidepost.

CSV Schema Solution
-------------------------

A workbook (or "spreadsheet") may or may not have schema
information.

The workbook may have any of a large
number of physical formats: .XLS (i.e., native) .XLSX or .CSV.
The format is irrelevant to the presence or absence of a schema.

One common way to embed schema information
is to assure that the first row of each sheet within a workbook has
the schema information.  This is the "column titles" solution.

-   This isn't done consistently.  Column titles are omitted.
    Sometimes the titles occupy multiple rows.
    Sometimes the sheet itself has a heading/body format where there's
    irrelevant rows which must be filtered out.

-   This is subject to all kinds of buffoonery.  Column titles can
    be tweaked manually.

-   The data type information is bound to each cell; the column title
    provides no usable type information.

    Exacerbating this is the way that anything number-like becomes a floating-point
    number.  Zip codes, punctuation-free phone numbers and social security numbers,
    etc., all become floating-point numbers.  This means they lose their
    leading zeroes, making zip-code matching particularly painful.

Another common case is--of course--to omit schema information entirely.
This devolves to the `COBOL Schema Solution`_ where the schema
information must be incorporated as a separate, reusable module of some kind.

A less common case is to include a separate sheet in the workbook (or worse, a separate file)
with schema information.  A separate sheet in a workbook is at least bound
with the data.  A separate schema description file (even if bound in a ZIP archive) can get
unbound from the data.

While there are numerous problems, workbooks are a very common way to exchange
data.  It's not sensible to pretend they don't exist.  We can't
paraphrase Jamie Zawinski and say
"Some people, when confronted with a problem, think 'I know,
I'll use [a spreadsheet].'  Now they have two problems."  We do need to
process this data irrespective of the issues.

We need a suitable, Pythonic solution to the schema problem when confronted
with data in a spreadsheet.

XML Non-Solution
---------------------

Weirdly, XML fans will state the XML is "self-defining".  Their claim
is that somehow this *solves* the schema problem.  This statement can be
misleading.

For our purposes, XML is a physical format.  An XML document without
a referenced (or embedded) XSD lacks any semantic information.
Even then, an XSD can be helpful, but not
sufficient.  The type definitions in an XSD could be unclear, ambiguous or
simply wrong.

An XSD document can be a good, useful schema definition.  It can also be riddled
with buffoonery, bad management and bugs.  It isn't magically perfect.  It
merely frees us from physical format considerations.

We can -- to an extent -- leverage elements of PyXSD (http://pyxsd.org)
to create Python classes from an XSD schema.  This package could
help by providing a standard class structure for a schema
defined in Python.  In particular, the ``pyxsd.schema`` module contains
some of what we're going to use.

XSD is a bit too complex for this problem domain.  Spreadsheets don't make use of the
sophisticated modeling available in XSD.  A spreadsheet is a flat list of
of a few simple types (float, string, date-encoded-as-float.)   A
COBOL DDE is a hierarchy of of a few simple types (display, comp-3).
All we can really do is borrow concepts and terminology.

Summary
-------------------------

Physical format independence is available with some file formats.
Sadly, others -- which are still in active use -- require a great deal
of schema information merely to decode the physical format.

Logical layout is generally a feature of the application program
as well as the data.  Even in a SQL-based data access, the column
names in a ``SELECT`` statement amount to a binding to a schema.

While we can make some kinds of simple
applications which are completely driven by metadata, the
general solution to information processing remains a proper
programming language.

Therefore, we need to have application programs which can
tolerate physical format changes without complaint.

We would also like an application program that can work with
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

Schema Representation Considerations
=====================================

There are several sensible way to represent schema information.  Each has
a problem.  We're forced to chose the lesser of the various evils.

1.  One (or more) rows in each workbook sheet that provides the attribute
    name.  The type is left implicit.  Other information (e.g., offset or size)
    is part of a physical format: it's only needed for fixed-format files.
    The "row-header schema" is casual and could involve numerous kinds of
    technical errors (missing names, duplicate names, incorrect names.)

#.  A distinct workbook sheet that lists name, data type information
    for each attribute.  Offset and size for fixed format files can
    also be provided.  This can involve numerous kinds of technical errors
    (missing attributes, duplicate attribute names, incorrect attribute
    descriptions.)

#.  A Python module that's built from source information.  This
    allows us to trivially ``import schema.foo`` and have lots of cool
    classes and functions in the ``schema.foo`` module.  This
    pushes the envelope the DRY principle because the module would have to be built
    from other source data.

#.  Some standardized metadata format.  XSD (or even XMI) pops into mind.
    These are detailed and potentially useful.
    Experience shows, however, that schema definitions are almost universally provided as workbook sheets.
    An external schema in a standard notation pushes the envelope on
    the DRY principle and also is vanishingly rare in practice.

Items 1 and 2 (workbook-based schema) cover over 99%
of the cases in practice.  While the data is casual and error-prone, it's
readily available and consistent with DRY.

Option 3 (a Python module) -- while cool -- breaks the DRY
principle.  Refreshing a Python schema when a source :file:`.XLS` document
changes is annoying because we're tweaking the way ``import`` works.
The imported schema module has to confirm that it's newer than the
files it's derived from and possibly rebuild itself *automagically* after
the schema source is touched.

Option 4 is rarely used in practice.  In the rare cases when an organization
will consent to providing XSD files, they're often prepared separately
from the data and do not actually reflect the application software
that is used to build the data file.

Plus, we should also be able to extend our schema representation
to cope with COBOL DDE's.  These have a place, since legacy conversion
and data extraction is still sometimes required.

Finally, we may be forced to deal with data in semi-structured formats
like JSON, YAML or an outline.

Our best approach is to load schema information from source every time
it's needed.  There are two paths:

-   Either we'll parse the embedded schema buried in each
    sheet of a workbook,

-   or we'll load an external schema definition from
    a file.

The COBOL Issues
-------------------------

When dealing with "Flat Files" from legacy COBOL problems, there are several
additional problems that need to be solved.

1.  The files have a fixed field layout, without delimiters.
    This means that the offset of each field must be used to
    decompose the record into its individual elements.

#.  Numeric fields can have an implied decimal point, making
    it difficult to determine the value of a string of digits.
    The COBOL DDE is essential for parsing the file contents.

#.  COBOL can make use of numeric data represented in a variety
    of "Computational" forms.  The "Computational-3" ("COMP-3")
    form is particularly complex because decimal digits are
    packed two per byte and the final half-byte encodes
    sign information.

#.  The string data may be encoded in EBCDIC bytes.

#.  COBOL encourages the use of data aliases (or "unions") via the ``REDEFINES`` clause.
    Without the entire unviverse of COBOL programs that work with a given file,
    the general handling of ``REDEFINES``
    data elements can become an insoluable problem.
    Only lazy field access can work;
    eager creation of individual cell values is doomed because a
    ``REDEFINES`` alternative may be defined over invalid data.
    
#.  COBOL has an ``OCCURS DEPENDING ON`` (ODO) feature where one attribute
    determines the size of another attribute. This means the data 
    of every attribute after the ODO attribute has a location which varies.
    The positions within the flat file cannot be computed statically.

Generally, COBOL files are defined by a "Data Definition Entry" (DDE)
that provides the record layout.

Developing the offsets to each field manually is tedious and
error-prone work.  In the presence of ODO, the locations become 
expressions based on the current record's data. 

It's essential to
parse the DDE, which has the original COBOL source
definition for the file.  A schema can be built from the parsed DDE.
