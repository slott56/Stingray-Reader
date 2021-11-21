#####################
Design Considerations
#####################

There are several important data representation issues. We'll look at 
`Workbook Schema Issues`_ and `Some COBOL File Issues`_ that
will inform the rest of the design.

Workbook Schema Issues
-------------------------------------

There are several ways to represent workbook schema information.

1.  One (or more) rows in each workbook sheet that provides the attribute
    name.  The type is often left implicit.  This is often bound to the
    data, making it quite useful in practice.
        
#.  A distinct workbook sheet that lists name (and optionally type) 
    for each attribute.  Offset and size can
    also be provided, making this useful for fixed-format COBOL files.
    This may be bound to data in a separate sheet of the same workbook,
    making it more likely to be useful. If it describes a COBOL file,
    however, the binding is more tenuous.

#.  A Python module that's built from source information.  This
    allows us to trivially ``import schema.foo`` and have lots of cool
    classes and functions in the ``schema.foo`` module.

#.  Some standardized metadata format. JSONSchema is preferred.
    XSD can also be used.

Options 1 and 2 (workbook-based schema) cover over 99%
of the cases in practice.  While the data is casual and error-prone, it's
often readily available.

Option 3 (a Python module) -- while cool -- can break the DRY
principle. Using JSONSchema means the schema can be encoded as a Python ``dict``.
This module would have to be used through an application ecosystem.

Refreshing a Python schema when a source :file:`.XLS` document
changes can be a problem. It creates a multi-step binding: document to schema module followed
by schema module to application.

Option 4 is rarely used in practice.  In the rare cases when an organization
will consent to providing schema files in a standard format, they're often prepared separately
from the data and do not actually reflect the application software
that is used to build the data file.

How do we extend this to handle COBOL DDE's? 

It seems sensible to load schema information from a source file every time
it's needed.  There are two paths:

-   Either we'll parse the embedded schema buried in each
    sheet of a workbook,

-   or we'll load an external schema definition from
    a file.
    
This causes us to partition the design into several components.

-   Schena definition.

-   Schema parsing and loading.

-   Application data access using a schema.

The idea is to have a universal schema representation. An application
can load this from a variety of sources, including JSON, COBOL, and heading
rows of a spreadsheet.


Some COBOL File Issues
-------------------------

Non-spreadsheet files, including legacy COBOL files, introduce
additional problems:

1.  The files have a fixed field layout, without delimiters.
    This means that the offset of each field must be used to
    decompose the record into its individual elements.

#.  Numeric fields can have an implied decimal point.
    The COBOL DDE is essential for parsing the file contents into number values.

#.  COBOL can make use of numeric data represented in a variety
    of "Computational" forms.  The ``COMP-3``
    form, for example, is very popular: decimal digits are
    packed two per byte and the final half-byte encodes
    sign information.

#.  The string data may be encoded in EBCDIC bytes, requiring decoding.

#.  COBOL encourages the use of data aliases (or "unions") via the ``REDEFINES`` clause.
    Without the entire unviverse of COBOL programs that work with a given file,
    the general handling of ``REDEFINES``
    data elements can become an insoluable problem. While it's clear some field
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
