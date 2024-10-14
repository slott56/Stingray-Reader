..  _`demo_sqa`:

#######################################################
Schema-Based Access Validation
#######################################################

We validate that both applications and files share a schema.
This can be done through a three-tier process:

1.  Validate application's use of a schema via conventional unit testing.
    This is the `Unit Test The Builder Function`_ section.

2.  Validate file conformance to a schema via "live-file testing".
    This is the `Live File Test The Builder Function`_ section.
    This is a kind of accceptance test to confirm the application can work with a data file.

3.  Validate the three-way application-schema-file binding by including a
    **Data Validation** mode in every file processing application.
    This is in a separate :ref:`demo_validate` demo application.

This demo application will focus on the schema and workbook processing.
This lets us confirm that the application will handle inputs correctly.
See the :file:`demo/test_demo.py` file in the Git repository for the complete source.

Of course, the rest of the application also requires good unit testing.
We'll assume the remainder of the application is also tested properly.
Failure to test is simply failure.

The first two levels of testing validate the application-to-schema binding.
The third level of testing validates the file-to-schema binding.

The Application, Capture, and Persistence Layers
===================================================

We need to distinguish between three sepaarate
but closely-related concepts.

-   The foundational objects for our application.
    It can help to separate the validation rules
    and object builders from the application objects
    themselves. The objects may have complex interactions.
    The validation can also be complex, but is utterly
    distinct from other behavior.

-   Peristence for the application objects.
    This may include object-relational mapping
    or other serialization techniques.

-   Capture of the source data for the application
    objects. This is schema-dependent processing.
    Experience shows that there is wide variability
    here when working with spreadsheets. It helps
    to disentangle this from the essential application
    processing and persistence.

The intent here is to follow the SOLID design
principles and minimize the interfaces as data
moves from the source document structure to
an common intermediate dictonary format.

This common dictionary structure can then be validated
and used to create the application objects
that define *only* the unique behavior of the
application, without the clutter of source decoding or
valdiation.

..  uml::

    @startuml

    package application <<Folder>> {
        class This {
            unique: str
            fields: int
            serialize(Database)
        }
        class That {
            more: float
            unique: bool
            fields: str
            serialize(Database)
        }

        class ThisForm {
            create(): This
        }

        class ThatForm {
            create(): That
        }
    }

    package persistence <<Database>> {
        class Database_Or_File {
            create(object)
            retrieve(object)
            update(object)
            delete(object)
        }
    }

    ThisForm --> This : "creates"
    ThatForm --> That : "creates"

    This --> Database_Or_File : "persists"
    That --> Database_Or_File : "persists"

    class Row

    package "schema based processing" <<Rectangle>> {
        class build_this << (F,orchid) Function >>
        class build_that << (F,orchid) Function >>
        hide build_this members
        hide build_that members

        class dict {
            key: str
            value: Any
        }

        build_this --> dict
        build_that --> dict
    }

    Row --> build_this
    Row --> build_that

    dict --> ThisForm
    dict --> ThatForm
    @enduml

The use of a ``ThisForm`` to validate the ``This`` instances follows the **Django** design pattern.
This isn't the only approach.
We can bundle the validation into the class definition, and follow the **Pydantic** design pattern.

In this document, we'll focus on the
builder functions and some higher-level processing.
There are two tiers of testing, and we'll show
how to structure the functions and the testing
to provide confidence that an application will
support a wide variety of source file formats.


Unit Test the Builder Function
==============================

See :ref:`using` for background. We're going to need a "builder function."
This transforms the source row object into the target object or collection.

Here's the essential function we want to test in isolation.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 7,12-21,31-35


To test this, we need a mock of a :py:class:`stingray.Row` object.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 8,37-43

This mock object uses a simple dictionary as a mock
for a :py:class:`Row` object. For the simplest
cases, this is a valid assumption.

The test is effectively this:

..  code-block:: gherkin

    SCENARIO: Build an intermediate object from a source document
    GIVEN a row instance
    WHEN fields are extracted
    THEN an intermediate object is built

Here's the test case.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 46-48

This is the essential unit test for a builder.

This works because a Stingray :py:class:`Row` is designed to behave like a dictionary.

We can, of course, create more elaborate mock :py:class:`Row` instances including attributes that need to be ignored.
We can also test rows that are missing attributes.

For more complex cases, it can become awkward
to mock all the features of a :py:class:`Row`.
For this, an integration-style test can be
easier to write.

Integration Test the Builder Function
=====================================

When more features of the :py:class:`Row` object are
used by a builder, then an integration test
can be easier to build. There are several ways to tackle this.
One approach is create an actual :py:class:`Row` using a mocked :py:class:`Sheet`
object. This works out well for examining more complex issues with navigation
and data conversion.

A :py:class:`Row` instance uses :py:class:`Nav` objects as helpers to navigate the data structure.
There are distinct :py:class:`Nav` subclasses for non-delimited data, delimited data,
and workbooks.

The :py:class:`Row` object requires a :py:class:`Sheet` instance. The :py:class:`Sheet` instance
requires a schema. Here's how we can build up the required fixture from pieces.

First, the schema.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 55-72

This is used by the mocked :py:class:`Sheet` instance.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 75-82

The :py:class:`Sheet` instance becomes part of the mock :py:class:`Row` instance.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 85-88

This mocked :py:class:`Sheet` instance is then used in the test case.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 91-93

This kind of integration test assures us that
the more complex navigation features are being
tested properly.


Live File Test the Builder Function
====================================

Step two is to test the :py:func:`some_builder` function with all rows in a given workbook.
In this demo, we're using :file:`sample/excel97_workbook.xls`.
Generally, we want to compute some aggregate (like a checksum) of various data items to be sure we've read and converted them properly.

Here's a fixture based on a file:

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 133-140

The fixture creates both a workbook and a sheet.
Some tests will use the workbook, others will use
the sheet.
It's handy to have a single fixture create both for us.

Here's the test case to assure all rows get built from the sheet.
In this case, the example data has two rows.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 143-150

This tells us that our builder function works properly with the sample file.
The application consists of additional functions.
We can test them both as isolated units and as an integration with the ``some_builder()`` function.

Unit Test the Sheet Function
============================
        
See :ref:`using` for background.
We're going to need a "sheet process function."
This transforms the source sheet into the target collection.
It applies the row builder function, named :py:func:`some_builder` and whatever
processing goes with that to all of the rows of a sheet.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 157-163

For complete isolation, we can provide
a mock sheet to supply mock rows
to a mock builder. Yes, that's a lot of mocking.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 166-174

We can then assure ourselves that the
overall sheet processing takes rows
from the sheet and provides them to
the builder.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 177-181


Live File Test The Sheet Function
====================================

An integration test can check the overall row counts from processing a live file.
This confirms that the sheet-level processing works as expected.

In this demo, we're using :file:`sample/excel97_workbook.xls`. 

The test opens the workbook.
It selects a sheet from the workbook using the class that extracts the schema from the row headers.
The test then uses the :py:func:`process_some_sheet` function on the given sheet to extract data.
In this case, the extraction is a frequency table.

..  literalinclude:: ../../../demo/test_demo.py
    :lines: 201-217

This shows how the sheet processing is tested
with a live file to be sure it will step
through all of the rows.

The counter provides some assurance that
all of the processing was correct.

Once we have the foundations of our application
tested in isolation and with live files,
we can move on to creating an application
that includes more processing steps and more validation rules.
