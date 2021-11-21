.. The Stingray Schema-Based File Reader documentation master file, created by
   sphinx-quickstart on Sat Aug 27 11:46:24 2011.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

================================================================
The Stingray Schema-Based File Reader
================================================================

The Stingray Reader tackles four fundamental issues in
processing a file's data:

-   How are the bytes organized?  What is the Physical Format?
-   Haw are the data objects organized?  What is the Logical Layout?
-   What do the bytes *mean*?  What is the Conceptual Content?
-   How can we assure ourselves that our applications will work with this file?

The problem is caused because the file's schema is not always bound
to the file nor is the schema clearly bound to an application program.
For example, a spreadsheet that lacks column titles is devoid of useful
logical or conceptual schema information. The physical spreadsheet file
format has a schema that's bound to the spreadsheet processing tools.

An application that reads spreadsheet data can treat each row as a list
of objects, and process items by index within the row. The schema is, therefore,
implicit in the code.

One goal of good software is to cope reasonably well with variability
of user-supplied inputs.  Providing data by spreadsheet workbook is
often a desirable choice for users.  Since workbook data can be tweaked manually, it
may not have a simple, fixed schema or logical layout. 

A workbook (the container of individual sheets)
can be encoded in any of a number of physical
formats: XLS, CSV, XLSX, ODS to name a few.  We would like our applications
to be independent of these physical formats, focusing on the logical layout.

Additionally, data supplied in the form of a workbook can suffer from data quality issues.
We need to be assured that a file actually conforms to the expected
schema.

A COBOL file parallels a workbook sheet in several ways. It also introduces
some unique complications. **Stingray Reader** works
well with common spreadsheets as well as COBOL files, allowing some 
uniformity in processing data from a variety of sources.

Contents
=========


.. toctree::
   :maxdepth: 1
   
   introduction
   design
   developer
   demo/index
   implementation/index
   history
   testing/index
   installation
   license

License
========

..	raw:: html

	<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.

The TODO List
===============

..  todolist::

Indices and Tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

