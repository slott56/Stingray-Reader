.. _`installation`:

##############################
Installation
##############################

Stingray requires Python 3.12.

Via a clone
===========

Use the following to get the latest code::

    git clone https://github.com/slott56/Stingray-Reader.git

This should createe a ``stingray-reader`` directory.

This can then be installed with::

    pip install -e stingray-reader/src

If using ``uv``::

    uv add stingray-reader

Via PIP from PyPI
=================

The alternative is to access it from PyPI::

    python -m pip install stingray-reader

If using ``uv``::

    uv add stingray-reader

Dependencies
============

The project has a number of dependencies.

Here is a summary:

-   "jsonschema>=4.23.0" to help work with JSON Schema definitions

-   "numbers-parser>=4.13.2" to read ``.numbers`` files

-   "openpyxl>=3.1.5" to read .XLSX files

-   "xlrd>=2.0.1" to read ``.XLS`` files.

-   "pyexcel[ods]>=0.7.0" to read ODS files.
    This package provides a handy wrapper for a number of file formats.

Development
============

The ``pyproject.toml`` lists the development tools required.
If you're using ``uv``, then these can be be installed by using ``uv add --dev``.

The ``tox.toml`` file provides a helpful test environment.
