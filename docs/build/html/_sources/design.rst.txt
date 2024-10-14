#####################
Design Notes
#####################

We'll start by looking at two import constraints on the design.
This is the `Essential Constraints` section, below.
First, **Stingray** depends on external packages, making data parsing opaque.
Second, schema information must be immutable, which collides with COBOL's ``OCCURS DEPENDING ON`` definition.

Additionally, there are a number of constraints on the files and the way they are described by a schema.
These are physical format issues.
We'll look at `File and Schema Binding`_, `Loading a Schema`_, and `Some COBOL File Issues`_ that inform the design.

We'll dwell on some details of COBOL field sizes and offsets in `Formal Definitions for Non-Delimited Files`_.

Also, `JSON Schema Considerations`_ describes how we can extend JSON Schema to support the additional features required for schema to describe non-delimited files.

Essential Constraints
=====================

We have two essential boundaries on the implementation.

1.  Each row of a spreadsheet or each record of a COBOL file is described by an opaque object.
    These are defined outside the control of the **Stingray Reader** package.
    This is because external libraries to read spreadsheets are wrapped by **Stingray**.
    We therefore treat COBOL files as if they are similarly opaque.

2.  The schema information must be immutable.

The opacity constraint suggests that each Workbook format uses a **Facade** to make conceal the details.
We decompose this **Facade** into two parts:

-   An :py:class:`Unpacker`.

-   A :py:class:`Workbook` wrapper that makes
    use of an appropriate :py:class:`Unpacker`.
    This wrapper often serves to provide consistent
    type hints.

In some cases, there may be unique workbook-specific processing that seems to be distinct from the details of unpacking sheets, rows, and cell values from the workbook.
This is also, properly, part of the py:class:`Workbook` wrapper,

The immutability constraint reflects a tradeoff decision.

-   One option is to have a mutable schema.
    To locate fields with non-delimited (COBOL) files,
    we can "decorate" the schema with offset locations
    into a bytes (or str) sequence. The decorations
    would change for each ``OCCURS DEPENDING ON``
    construct.

-   The chosen option is to have an immutable schema.
    To locate fields with non-delimited (COBOL) files,
    a :py:class:`Location` helper is used. This wraps the schema and offset locations into a new object.
    The decorations relate to a specific instance,
    and are discarded.

Annotating (or decorating) the schema with size and offset information makes it very difficult to keep multiple COBOL records in memory when they have ``OCCURS DEPENDING ON`` constructs with distinct values.
Keeping the size and offset information in separate objects, permits a great deal more flexibility.
The cost associated with this flexibility is the creation and destruction of numerous small :py:class:`Location` instances.


File and Schema Binding
=======================

There are several ways to represent file schema information.

1.  For spreadsheets, one (or more) rows in each workbook sheet can provide the attribute names.
    The data types and other details are implicit.
    This is often bound to the data, making it quite useful in practice.
        
#.  A distinct workbook sheet that lists name (and optionally type) for each attribute.
    Offsets and sizes can also be provided, making this useful for fixed-format COBOL files.
    This may be bound to data in a separate sheet of the same workbook, making it more likely to be useful.

#.  A Python module that's built from source information.
    This allows us to trivially ``import schema.foo`` and have lots of cool classes and functions in the ``schema.foo`` module.

#.  Some standardized metadata format.
    JSON Schema is preferred.
    XSD can also be used.
    While this is separate from the file, it can be incorporated into the application.

Options 1 and 2 (workbook-based schema) cover over 99% of the cases in practice.
While the data is casual and error-prone, it's often readily available.

Option 3 (a Python module) is very cool.
Using JSON Schema means the schema can be encoded as a Python ``dict``.
This module can then be used through an application ecosystem to provide a single, consistent schema to all related applications.

Option 4 (a JSONSchema document) can be consumed by a library module like **Stingray** to enforce a schema throughout a suite of applications.

Option 4 is rarely used in practice.
In the rare cases when an organization will consent to providing schema files in a standard format, they're often prepared separately from the data and do not actually reflect the application software
that is used to build the data file.

Option 2 also covers COBOL files, where the schema is a separate document with the Data Definition Entries (DDE's).

Loading a Schema
=====================

An application needs to load schema information when processing a file described by that schema.
There are three sources for schema information:

-   Parse the embedded schema buried in the sheet of a workbook.
    Often, this is a header row.

-   Load an external schema definition from a file.

-   Code the schema into the application. This can be an explicit JSONSchema document.
    The worst case is an implicit schema in the form of code that refers to column names without any formalization of the schema.

Making the schema explicit makes it testable at development time and observable when used for production.
An explicit schema allows us to partition application design into several components:

-   **Schema definition**. These can be shared and pubished.
    Changes can be tracked explicitly and visibly.

-   **Schema parsing and loading**.
    For JSON Schema documents, this is relatively simple.
    For COBOL DDE files, this is a bit more complex.

-   **Application data processing**. The where data is consumed using a schema.

The idea is to have a universal schema representation. An application can load this from a variety of sources, including JSON, COBOL, and heading rows of a spreadsheet.

Having a published, testable schema assures that all of the applications can agree to the physical format and logical layout of the data.
It also makes data quality problems observable.


Some COBOL File Issues
-------------------------

Non-spreadsheet files, including legacy COBOL files, introduce some additional complexities that workbook files don't have.
Here's a summary:

1.  COBOL files have a fixed field layout, without delimiters.
    This means that the offset of each field into a sequence of characters or bytes must be used to decompose the record into its individual elements.

#.  Numeric fields can have an implied decimal point.
    The COBOL DDE ``PICTURE`` clause is essential for parsing the file contents into number values.

#.  COBOL can make use of numeric data represented in a variety of "Computational" forms.
    The ``COMP-3`` form, for example, is very popular: decimal digits are packed two per byte and the final half-byte encodes sign information.

#.  The string data may be encoded in EBCDIC bytes, requiring decoding.

#.  COBOL permits data aliases (or "unions") via the ``REDEFINES`` clause.
    Without the entire unviverse of COBOL programs that work with a given file, the general handling of ``REDEFINES`` alternatives can become an insoluable problem.
    While it's clear some field's value must discriminate among the union alternatives, that detail is not part of the COBOL DDE.
    Only lazy field access can work.
    Attempting eager creation of cell values is doomed because several ``REDEFINES`` alternative may have valid interpretations of the bytes.
    
#.  COBOL has an ``OCCURS DEPENDING ON`` feature where one attribute of a DDE determines the size of an array.
    This means every attribute after the array has a location which varies.
    The locations within the flat file can only be computed with an actual instance of the record.

Generally, COBOL files are defined by a "Data Definition Entry" (DDE) that provides the record layout.
It's essential to parse this source DDE, which has the original COBOL definition for the file.
A schema can be built from the parsed DDE.
There's no good reason to rely on any intermediate description separate from the DDE's themselves.


Formal Definitions for Non-Delimited Files
===========================================

One approach to locating field values in a nested COBOL object with ``REDEFINES`` and ``OCCURS DEPENDING ON`` is to decorate the schema with instance location details for each item in the schema.
This handles ``OCCURS DEPENDING ON`` by computing unique location decorations for each instance.

We'll describe this as being done eagerly.
Pragmatically, it can be done lazily and cached.
The ``REDEFINES`` is handled by application program logic to avoid references to invalid data.

The instance, :math:`I`, is an array of :math:`b` characters or bytes.

..  math::

    I = \{ i_0, i_1, i_2, ..., i_{b-1} \}


We can slice :math:`I` using start, :math:`s`, and end, :math:`e` locations to create sub-instances.

..  math::

    I[s: e] = \{ i_x \mid s \leq x < e \}


It seems like :math:`I_{s:e}` might be slightly more traditional notation than :math:`I[s:e]`. :math:`I_{\{x \mid s \leq x < e\}}` seems fussy.

We can follow the Python convention of omitting :math:`s` when :math:`s = 0` and omitting :math:`e` when :math:`e = b`.
This means that :math:`I \equiv I[:]`.

A Schema, :math:`S`, is a static description of a number of instances, :math:`I_0, I_1, I_2, ...`.
We say a schema is an approximate model for an instance: :math:`S \tilde \models I`.
The modelling is approximate because a model may be incomplete.
Specifically, we may not know the locations of all fields because of an  ``OCCURS DEPENDING ON`` clause.

A more complete model requires resolution of ``OCCURS DEPENDING ON`` to compute locations, :math:`L`.
This combination can be stated as :math:`S \circ L \models I`.
The schema, :math:`S`, composed with additional location information, :math:`L`, models the instance, :math:`I`.

A Schema has a structure with three kinds of elements:

-   Atomic, with no further structure. This has a type conversion (or "unpack") function, :math:`t(I)`, to extract a value from an instance.
    It requires a length, :math:`l`, that defines a slice from the instance.
    We can say :math:`t_{S}(I[:l])` to refer to the type conversion function for a specific Schema, :math:`S`, used to decode an instance, :math:`I`.
    JSON Schema defines types of number, int, string, boolean, and null.
    Other types may need to be added to handle COBOL data.

-   Array, with an Items subschema, :math:`S_i` that describes a repeating sub-instance.
    The number of sub instances, :math:`r` may be provided explicitly.
    In the ``OCCURS DEPENDING ON`` case, the value of :math:`r` is the value of another field.
    This means :math:`r = t_{S_d}(I)` because the ocurrences depend on the value in field :math:`S_d`.

-   Object, with Properties structure that describes a number of subschema, :math:`S_{p_x}`.

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

- COBOL encoded data (Packed-Decimal, Binary, etc.) JSON Schema presumes delimited files with a parser's conversions of data. We are driving the parser from the JSON Schema, therefore additional details are required.

- Occurs Depending On reference. JSON Schema limits the ``maxItems`` to an unsigned integer. We need to ``$ref``, and an extension vocabulary is required.

- Workbook conversions of numeric data to durations or timestamps. This distinct from the ``format`` option on a ``"type": "string"`` where the format provides a pattern for parsing text. We need to specify an additional conversion.

See https://github.com/json-schema-org/json-schema-spec for the meta schema used to define a schema.

We have two overall strategies:

-   Update the meta schema or

-   Adapt to the existing meta schema.

We'll look at each in some detail.

Update the meta schema
-----------------------

We could make some changes to the JSON Schema meta schema through a vocabulary addition.

- Add a new ``decimal`` type to the validation vocabulary.

- Permit a ``$ref`` validation for ``maxItems`` in the array vocabulary.

- Additional unpacking details for "integer" and "number" are needed. Currently "format" is used for strings. We could expand it's use to include encoding details for numbers. This would support bigendian integers, bigendian floats, packed decimal, and zoned decimal number, integer, and decimal values.

- Additional conversion details for Workbook numbers that are encodings of durations or timestamps.

We can then use a revised meta schema to validate the schema used for Workbooks or COBOL.

Adapt to the existing meta schema
----------------------------------

We could adapt to the existing JSON Schema meta schema.

We *can* consider COBOL encoded numeric values as ``"type": "string"`` with additional ``contentEncoding`` details. Calling these values strings seems to push the envelope a bit: JSON Schema generally avoies syntax and unpacking issues. The value truly is a number.

It seems improper to require a separate type conversion to create the needed, Python-specific ``Decimal``.  A conversion can be *implicit*, using a ``"cobol"`` keyword value. Or, it can be *explicit*, using a new ``"conversion"`` keyword for extended types like timestamp, duration based on float, and ``Decimal`` based unpacking bytes.

This is the approach taken for Workbook values like durations and timestamps. A supplemental ``"format": "duration"`` is used to parse a string that encodes a duration value. Parsers, generally, don't do anything with this. Validation can be handled to conform the string matches the format. Decoding a useful object is left to the application.

The ``"contentEncoding"`` provides a way to describe COBOL Packed Decimal and Binary as strings of bytes. A ``"cobol"`` keyword gets Usage and Picture values required to decode EBCDIC.

Here's an example::

    {"type": "string",
     "contentEncoding": "packed-decimal",
     "cobol": "05 SOME-FIELD USAGE COMP-3 PIC S999V99",
     "conversion": "decimal"
    }

Other ``"contentEncoding"`` values include "bigendian-h",  "bigendian-i", "bigendian-q" (for short, int, and long long values.) Also, "bigendian-float" and "bigendian-double". And, of course, "CP037" or "EBCDIC" to decode ordinary strings from EBCDIC to native text.

This permits a little more flexibility for moving to native (non-EBCDIC) file formats. Instead of the ``"cobol"`` keyword to propvide unpacking details, we can use a ``"struct"`` keyword to provide Python's ``"struct"`` module format codes.

Also, we need to add ``"maxItemsDependsOn"`` keyword to include the ``"$ref"`` value for arrays. Here's an example::

    {"type": "array",
     "maxItemsDependsOn": {"$ref": "#/path/to/SIZE-FIELD"},
     "cobol": "05 THINGS OCCURS DEPENDING ON SIZE-FIELD",
     "items": {...}
     }

For ``REDEFINES``, we use the existing ``"oneOf"``::

    "REDEFINES-A-B-C": {
        "oneOf": [
            {"type": "object", "properties": {"A": {"type": "number"}}},
            {"type": "object", "properties": {"B": {"type": "string"}}},
            {"type": "object", "properties": {"C": {"type": "boolean"}}},
        ]
    }

To provide COBOL-style naming to a ``OneOf`` (or ``REDEFINES`` schema,) we impose a rule that each alternative must be an object, and each object must have at most one named property.
The parent item is effectively anonymous, since it's never used in COBOL.


