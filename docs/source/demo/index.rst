
..  _`demo`:

######################################
Demo Applications
######################################

Perhaps the most important use case for **Stingray** is to write data extract applications.
This is shown below in :ref:`demo_validate`.
We can also handle COBOL file conversions.
This is shown below in :ref:`demo_cobol`.

Additionally, we can use **Stingray** to provide
assurance that  our files and our applications both use the same conceptual schema.
We need several kinds of assurance.

-   How do we **confirm** the schema of a given file? 

-   More importantly, how do we confirm that an application can successfully use a file?

The answers to these questions involve a two-phased approach.
First, essential unit testing and integration testing must be done.
Second, the data itself must be validated.

We'll look an examples to show how to do basic unit and integration testing.
This is followed by several examples to show how to do write data validation applications, and how to provide data validation (or a "dry-run" option) as part of a converntional file-processing application.

..  toctree::
    :maxdepth: 1

    data_quality
    validation
    profile
    conversion
    cobol_reader
