================================================================
The Stingray Schema-Based File Reader
================================================================

Spreadsheet format files are the *lingua franca* of data processing.
CSV, Tab, XLS, XSLX and ODS files are used widely.  Python's `csv` module
and the XLRD project (http://www.lexicon.net/sjmachin/xlrd.htm) help
us handle spreadsheet files.

By themselves, however, they aren't a very complete solution.

The Stingray Schema-Based File Reader offers several features to help
process files in spreadsheet formats.

1.  It wraps `csv`, `xlrd`, plus several XML parsers into a single, unified
    "workbook" structure to make applications that work with any
    of the common physical formats.

2.  It extends the workbook to include fixed format files (with no delimiters)
    and even COBOL files in EBCDIC.

3.  It provides a uniform way to load and use schema information.  This can
    be header rows in the individual sheets of a workbook, or it can be separate
    schema information.

4.  It provides a suite of data conversions that cover the most common cases.

Additionally, stringray provides some guidance on how to structure
file-processing applications so that they are testable and composable.

Stingray 4.4.6 requires Python 3.3

It depends on this project to read .XLS files

-   xlrd.  http://www.lexicon.net/sjmachin/xlrd.htm

If you want to build from scratch and create documentation, you'll need these
other two projects.

-   PyLit3.  https://github.com/slott56/PyLit-3

-   Sphinx.  http://sphinx.pocoo.org/

Since Stingray is a *Literate Programming* project, the documentation is also
the source.  And vice-versa.

http://stingrayreader.sourceforge.net/index.html

http://sourceforge.net/projects/stingrayreader/files/stingray.pdf

https://sourceforge.net/p/stingrayreader/code/

Here's the license for using Stingray

http://creativecommons.org/licenses/by-nc-sa/4.0/


