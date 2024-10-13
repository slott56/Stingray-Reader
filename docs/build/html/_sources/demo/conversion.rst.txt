..  _`demo_conversion`:

#######################################################
Data Conversion -- ETL with Validation
#######################################################

Data conversions -- from one file format (or database) to another -- involve an Extract from the source, a possible transformation, and an load into the target.
ETL is an apt generalization of a great many data movement operations.

The Extract and Load ends of the process are clearly schema-driven and need to be independent of file format.
The Transformation in the middle might be nothing more than changing representation from source schema type to target schema type.
The Transformation may be more complex, and involve multiple source joins, or normalizations, or computation of derived fields.
In general, the transformation portion is pure Python, and doesn't need to be explored in any detail.

The Extract and Load steps, however, do benefit from having a schema.
This follows the design patterns shown earlier.
See the :file:`demo/conversion.py` file in the Git repository for the complete source.

Builder
=======

Here's the builder function to create an intermediate document from the data source.

..  literalinclude:: ../../../demo/conversion.py
    :lines: 33-38

This is used as the Extract part of ETL.

Persistence
===========

Here's context manager that handles persistence.
This provides a :py:meth:`save_json` method.
A subclass can remove this to implement a "dry-run" data validation operating mode.

This is one version of the Load part of ETL.

..  literalinclude:: ../../../demo/conversion.py
    :lines: 41-57

Validation
===========

This can be viewed as part of Extract or part of Transform.
It seems to make sense to think of data validation as the first stage of any Transformation processing.

..  literalinclude:: ../../../demo/conversion.py
    :lines: 60-70

Target Schema for Output
========================

This schema is used to define the Load operation.
It -- in conjunction with the :py:class:`Persistent_Processing` class -- implements the "load" part.
In this case, the load writes a file.

..  literalinclude:: ../../../demo/conversion.py
    :lines: 73-84

Process Sheet
=============

The source has one (or more) sheets of data.
This will extract, transform, and load all of them.

..  literalinclude:: ../../../demo/conversion.py
    :lines: 87-104

Main Program
============

Argument parsing looks like this:

..  literalinclude:: ../../../demo/conversion.py
    :lines: 107-120

The overall :py:func:`main` function:

..  literalinclude:: ../../../demo/conversion.py
    :lines: 123-132
