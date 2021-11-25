#####################
Design Considerations
#####################

There are several important data representation issues. We'll look at 
`File Schema Issues`_, `Working with a schema`_, and `Some COBOL File Issues`_ that
will inform the rest of the design.

File Schema Issues
==================

There are several ways to represent file schema information.

1.  For spreadsheets, one (or more) rows in each workbook sheet can provide the attribute
    names.  The data types and other details are implicit.  This is often bound to the
    data, making it quite useful in practice.
        
#.  A distinct workbook sheet that lists name (and optionally type) 
    for each attribute.  Offsets and sizes can
    also be provided, making this useful for fixed-format COBOL files.
    This may be bound to data in a separate sheet of the same workbook,
    making it more likely to be useful.

#.  A Python module that's built from source information.  This
    allows us to trivially ``import schema.foo`` and have lots of cool
    classes and functions in the ``schema.foo`` module.

#.  Some standardized metadata format. JSONSchema is preferred.
    XSD can also be used. While this is separate from the file,
    it can be incorporated into the application.

Options 1 and 2 (workbook-based schema) cover over 99%
of the cases in practice.  While the data is casual and error-prone, it's
often readily available.

Option 3 (a Python module) is very cool.
Using JSONSchema means the schema can be encoded as a Python ``dict``.
This module can then be used through an application ecosystem to provide a
single, consistent schema to all related applications.

Option 4 (a JSONSchema document) can be consumed by a library module
like Stingray Reader to enforce a schema throughout a suite of applications.

Option 4 is rarely used in practice.  In the rare cases when an organization
will consent to providing schema files in a standard format, they're often prepared separately
from the data and do not actually reflect the application software
that is used to build the data file.

Option 2 also covers COBOL files, where the schema is a separate document with the
Data Definition Entries (DDE's).

Working with a Schema
=====================

An application needs to load schema information when processing a file
described by that schema. There are three sources for schema information:

-   Parse the embedded schema buried in the sheet of a workbook.
    Often, this is a header row.

-   Load an external schema definition from a file.

-   Code the schema into the application. This can be an explicit JSONSchema document.
    The worst case is an implicit schema in the form of code that refers to column
    names without any formalization of the schema.

Making the schema explicit makes it testable at development time and observable when
used for production. An explicit schema allows us to partition
application design into several components.

-   Schema definition. These can be shared and pubished. Changes can be tracked explicitly
    and visibly.

-   Schema parsing and loading. For JSONSchema documents, this is relatively simple.
    For COBOL DDE files, this is a bit more complex.

-   Application data access using a schema.

The idea is to have a universal schema representation. An application
can load this from a variety of sources, including JSON, COBOL, and heading
rows of a spreadsheet.

Having a published, testable schema assures that all of the applications
can aggree to the physical format and logical layout of the data.
It also makes data quality problems observable.


Some COBOL File Issues
-------------------------

Non-spreadsheet files, including legacy COBOL files, introduce
some additional complexities that workbook files don't have.
Here's a summary:

1.  COBOL files have a fixed field layout, without delimiters.
    This means that the offset of each field into a sequence of characters or bytes must be used to
    decompose the record into its individual elements.

#.  Numeric fields can have an implied decimal point.
    The COBOL DDE ``PICTURE`` clause is essential for parsing the file contents into number values.

#.  COBOL can make use of numeric data represented in a variety
    of "Computational" forms.  The ``COMP-3``
    form, for example, is very popular: decimal digits are
    packed two per byte and the final half-byte encodes
    sign information.

#.  The string data may be encoded in EBCDIC bytes, requiring decoding.

#.  COBOL permits data aliases (or "unions") via the ``REDEFINES`` clause.
    Without the entire unviverse of COBOL programs that work with a given file,
    the general handling of ``REDEFINES``
    alternatives can become an insoluable problem. While it's clear some field's value
    discriminates among the union alternatives, that detail is not part of the COBOL DDE.
    Only lazy field access can work;
    eager creation of individual cell values is doomed because a
    ``REDEFINES`` alternative may be defined over invalid data.
    
#.  COBOL has an ``OCCURS DEPENDING ON`` feature where one attribute of a DDE
    determines the size of an array. This means every attribute after the array has a location which varies.
    The locations within the flat file can only be computed with an actual
    instance of the record.

Generally, COBOL files are defined by a "Data Definition Entry" (DDE)
that provides the record layout.
It's essential to parse this source DDE, which has the original COBOL
definition for the file.  A schema can be built from the parsed DDE.
There's no good reason to rely on any intermediate description separate from
the DDE's themselves.
