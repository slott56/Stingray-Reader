.. _`installation`:

##############################
Installation
##############################

Stingray requires Python 3.9.

Use the following to get the latest code::

    git clone git://git.code.sf.net/p/stingrayreader/code stingrayreader-code
   
The alternative is to use PIP::

    python -m pip install stingray-reader

This installation will also download and install a number of dependencies.
See ``requirements.txt`` for the full list.

Here is a summary:

-   jsonschema>=4.2

-   xlrd>=2.0 to read ``.XLS`` files.

-   openpyxl>=3.0 to read ``.XLSX`` files.

-   pyexcel>=0.6 and pyexcel-ods3>=0.6 to read ``.ODS`` files.

-   numbers-parser>=2.1 to read ``.numbers`` files.

With additional ``from __future__ import annotations`` it may be able to work with Python 3.8.

Development
=============================

The following tools are used for development

-   Sphinx.  http://sphinx.pocoo.org/

-   pytest.

-   pytest-cov.

-   tox.

-   mypy.
