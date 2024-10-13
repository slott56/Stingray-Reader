..  _`demo_profile`:

##########################################################
Data Profiling
##########################################################

This is a data profiling application that can be applied to workbooks to examine the data.
This can help design builder functions for applications.
The idea is that when a data profiling run fails, there are three possible problems:

-   The data has changed and this is the first we've heard of the change.
    We need to extend the application to handle this.

-   Our provious understanding of the data was incomplete, and this data shows a previously unknown case.
    This suggests our application had a latent bug.

-   The data really **is** bad.
    The data must be rejected and reworked.

Finding out which of these three things requires concrete details from a data profiling application.

This produces simple RST-format output on stdout.
A common use case is the following:

..  code-block:: bash

    python demo/profile.py sample/\*.csv >profile_csv.rst
    rst2html.py profile_csv.rst profile_csv.html
    
This gives us an HTML-formatted report showing distributions
of values in each column.

This will follow a number of the design patterns shown earlier.
See the :file:`demo/profile.py` file in the Git repository for the complete source.

Core Processing
===============

The core proessing is to gather counts of individual sample values.

..  literalinclude:: ../../../demo/profile.py
    :lines: 40-69

The serialization uses the :py:func:`print` function to create a file with the needed report.
This lets us tinker with  string formatting, and use the ``string.Template`` or even the **Jinja** template processing tool.
It also permits using ``contextlib.redirect_stdout`` to capture the print output into a file.

Processing Context
==================

A data profiling application doesn't, generally, produce much stateful output.
It doesn't often update a database.

This is a "reporting" application.
In effect, it's an elaborate query.
This means it has two important features:

-   The output is meant for human consumption.

-   The processing is fully idempotent.
    We can run the application as often as needed without worrying about corrupting a database with out-of-order operations or duplicate operations.

Because there isn't a significant, persistent state change a persistence management class is -- perhaps -- unnecessary.

It helps, however, to follow the overall design pattern in case some persistent state change *does* become part of this application.


..  literalinclude:: ../../../demo/profile.py
    :lines: 75-93

This design lets us write a subclass that provides an alternative definition of the :py:meth:`save_stats` method.
We can then use the alternative subclass to change the output processing.

Sheet Processing
=================

The :py:func:`process_sheet` function the heart of the application.
This handles all the rows present in a given sheet.

..  literalinclude:: ../../../demo/profile.py
    :lines: 99-115

        
Some applications will have variant processing for workbooks that contain different types of sheets.
This leads to different ``process_this_sheet``  and ``process_that_sheet`` functions.  
Each  will follow the above template to process all rows of the sheet.

Workbook Processing
===================

..  literalinclude:: ../../../demo/profile.py
    :lines: 131-136


Command-Line Interface
======================

We have an optional argument for verbosity and a positional argument that
provides all the files to profile.
This function parses the command-line arguments:

..  literalinclude:: ../../../demo/profile.py
    :lines: 142-154

Here's the overall :py:func:`main` function:

..  literalinclude:: ../../../demo/profile.py
    :lines: 157-167

        
Running the Demo
================

We can run this program like this:

..  code-block:: bash

    python3 demo/profile.py sample/\*.csv >profile_csv.rst
    rst2html.py profile_csv.rst profile_csv.html

The RST output file looks like this:

..	code-block:: text

    sample/Anscombe_quartet_data.csv
    ====================================

    x123
    ----

    AtomicSchema({'title': 'x123', '$anchor': 'x123', 'type': 'string', 'position': 0})

    ..  csv-table::

        "10.0","1"
        "8.0","1"
        "13.0","1"
        "9.0","1"
        "11.0","1"
        "14.0","1"
        "6.0","1"
        "4.0","1"
        "12.0","1"
        "7.0","1"
        "5.0","1"

    y1
    --

    AtomicSchema({'title': 'y1', '$anchor': 'y1', 'type': 'string', 'position': 1})

    ..  csv-table::

        "8.04","1"
        "6.95","1"
        "7.58","1"
        "8.81","1"
        "8.33","1"
        "9.96","1"
        "7.24","1"
        "4.26","1"
        "10.84","1"
        "4.82","1"
        "5.68","1"

    y2
    --

    AtomicSchema({'title': 'y2', '$anchor': 'y2', 'type': 'string', 'position': 2})

    ..  csv-table::

        "9.14","1"
        "8.14","1"
        "8.74","1"
        "8.77","1"
        "9.26","1"
        "8.10","1"
        "6.13","1"
        "3.10","1"
        "9.13","1"
        "7.26","1"
        "4.74","1"

    y3
    --

    AtomicSchema({'title': 'y3', '$anchor': 'y3', 'type': 'string', 'position': 3})

    ..  csv-table::

        "7.46","1"
        "6.77","1"
        "12.74","1"
        "7.11","1"
        "7.81","1"
        "8.84","1"
        "6.08","1"
        "5.39","1"
        "8.15","1"
        "6.42","1"
        "5.73","1"

    x4
    --

    AtomicSchema({'title': 'x4', '$anchor': 'x4', 'type': 'string', 'position': 4})

    ..  csv-table::

        "8.0","10"
        "19.0","1"

    y4
    --

    AtomicSchema({'title': 'y4', '$anchor': 'y4', 'type': 'string', 'position': 5})

    ..  csv-table::

        "6.58","1"
        "5.76","1"
        "7.71","1"
        "8.84","1"
        "8.47","1"
        "7.04","1"
        "5.25","1"
        "12.50","1"
        "5.56","1"
        "7.91","1"
        "6.89","1"


    sample/Anscombe_schema.csv
    ==============================

    x123
    ----

    AtomicSchema({'title': 'x123', '$anchor': 'x123', 'type': 'string', 'position': 0})

    ..  csv-table::

        "y1","1"
        "y2","1"
        "y3","1"
        "x4","1"
        "y4","1"

    X values for series 1, 2, and 3.
    --------------------------------

    AtomicSchema({'title': 'X values for series 1, 2, and 3.', '$anchor': 'X_values_for_series_1_2_and_3.', 'type': 'string', 'position': 1})

    ..  csv-table::

        "Y value for series 1.","1"
        "Y value for series 2.","1"
        "Y value for series 3.","1"
        "X value for series 4.","1"
        "Y value for series 4.","1"

    number
    ------

    AtomicSchema({'title': 'number', '$anchor': 'number', 'type': 'string', 'position': 2})

    ..  csv-table::

        "number","5"


    sample/csv_workbook.csv
    ===========================

    Col 1 - int
    -----------

    AtomicSchema({'title': 'Col 1 - int', '$anchor': 'Col_1_-_int', 'type': 'string', 'position': 0})

    ..  csv-table::

        "42","1"
        "9973","1"

    Col 2.0 - float
    ---------------

    AtomicSchema({'title': 'Col 2.0 - float', '$anchor': 'Col_2.0_-_float', 'type': 'string', 'position': 1})

    ..  csv-table::

        "3.1415926","1"
        "2.7182818","1"

    Column "3" - string
    -------------------

    AtomicSchema({'title': 'Column "3" - string', '$anchor': 'Column_3_-_string', 'type': 'string', 'position': 2})

    ..  csv-table::

        "string","1"
        "data","1"

    Column '4' - date
    -----------------

    AtomicSchema({'title': "Column '4' - date", '$anchor': 'Column_4_-_date', 'type': 'string', 'position': 3})

    ..  csv-table::

        "09/10/56","1"
        "01/18/59","1"

    Column 5 - boolean
    ------------------

    AtomicSchema({'title': 'Column 5 - boolean', '$anchor': 'Column_5_-_boolean', 'type': 'string', 'position': 4})

    ..  csv-table::

        "TRUE","1"
        "FALSE","1"

    Column 6 - empty
    ----------------

    AtomicSchema({'title': 'Column 6 - empty', '$anchor': 'Column_6_-_empty', 'type': 'string', 'position': 5})

    ..  csv-table::

        "","2"

    Column 7 - Error
    ----------------

    AtomicSchema({'title': 'Column 7 - Error', '$anchor': 'Column_7_-_Error', 'type': 'string', 'position': 6})

    ..  csv-table::

        "#DIV/0!","1"
        "#NAME?","1"


    sample/simple.csv
    =====================

    name
    ----

    AtomicSchema({'title': 'name', '$anchor': 'name', 'type': 'string', 'position': 0})

    ..  csv-table::

        "Col 1 - int","1"
        "Col 2.0 – float","1"
        "Column “3” - string","1"
        "Column '4' – date","1"
        "Column 5 – boolean","1"
        "Column 6 – empty","1"
        "Column 7 – Error","1"

    offset
    ------

    AtomicSchema({'title': 'offset', '$anchor': 'offset', 'type': 'string', 'position': 1})

    ..  csv-table::

        "1","1"
        "12","1"
        "23","1"
        "34","1"
        "45","1"
        "56","1"
        "67","1"

    size
    ----

    AtomicSchema({'title': 'size', '$anchor': 'size', 'type': 'string', 'position': 2})

    ..  csv-table::

        "11","7"

    type
    ----

    AtomicSchema({'title': 'type', '$anchor': 'type', 'type': 'string', 'position': 3})

    ..  csv-table::

        "int","1"
        "float","1"
        "str","3"
        "datetime","1"
        "bool","1"

This can be processed by **pandoc** or **docutils** to create an HTML report.
