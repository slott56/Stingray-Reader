================================================================
The Stingray Schema-Based File Reader
================================================================

Spreadsheet format files are the *lingua franca* of data processing.
CSV, Tab, XLS, XSLX and ODS files are used widely.  Python's ``csv``
module handles two common formats. Add-on packages are required for the
variety of other physical file formats.

The problem is that each add-on package has a unique view of the underlying
data.

The Stingray Schema-Based File Reader offers several features to help
process files in spreadsheet formats.

1.  It wraps format-specific modules with a unified
    "workbook" Facade to make applications that work with any
    of the physical formats.

2.  It extends the workbook concept to include non-delimited files, including
    COBOL files encoded in EBCDIC.

3.  It provides a uniform way to load and use schema information based on JSONSchema.
    A schema can be header rows in the individual sheets of a workbook, or it can be separate
    schema information in another spreadsheet, a JSONSchema document, or COBOL "copybook"
    data definitions.

4.  It provides a suite of data conversions that cover the most common cases.

Additionally, the Stingray Reader provides some guidance on how to structure
file-processing applications so that they are testable and composable.

Stingray 5.0 requires Python >= 3.9. The code is fully anotated with type hints.

This depends on additional projects to read .XLS, .XLSX, .ODS, and .NUMBERS files.

-   CSV files are built-in.

-   COBOL files, as mentioned above, are built-in.

-   NDJSON or JSON Newline files are JSON with an extra provision that each document must be complete on one physical line. These use the built-in ``json`` module.

-   XLS files can be read via ``xlrd``.  http://www.lexicon.net/sjmachin/xlrd.htm

-   ODS and XLSX can be read via these two projects. https://openpyxl.readthedocs.io/en/stable/ and http://docs.pyexcel.org/en/v0.0.6-rc2/.

-   Numbers (v13 and higher) usees protobuf and and snappy compression. See https://pypi.org/project/numbers-parser/.

-   YAML files can be a sequence of documents, permitting a direct mapping to a Workbook with a single Sheet.

-   TOML files are -- in effect -- giant dictionaries with flexible syntax and can be described by a JSONSchema.

-   XML files can be wrapped in a Workbook. There's no automated translation from XSD to JSONSchema here.
    A sample is provided, but this may not solve very many problems in general.

A file-suffix registry is used to map a suffix to a Workbook subclass that handles the physical format.
A decorator is used to add or replace file suffix mappings.

Environments
============

::

    conda create -n stingray python=3.9
    conda activate stingray
    python -m pip install tox
    python -m pip install --requirement requirements.txt
