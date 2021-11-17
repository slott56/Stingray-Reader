#!/usr/bin/env python3

# .. _`installation`:
#
# ##############################
# Installation via ``setup.py``
# ##############################
#
# Use the following link to get the latest code:
#
#     ``git clone git://git.code.sf.net/p/stingrayreader/code stingrayreader-code``
#   
# It's also possible to get an archive distribution kit. These may be 
# slightly out of date.
#
# Optional Build-from-Scratch
# =============================
#
# The Stingray distribution kit (minimally) is just the following.
#
# -   :file:`source`.  The RST-formatted source used by PyLit3 to 
#     to create code and documentation.
#
# -   :file:`sample`.  Several sample workbooks used for testing.
#
# -   :file:`build.py`.  The build procedure.  
#
# Given these two directories, the build procedure uses PyLit3 to create ``stingray`` package.  
# It uses Sphinx to create the documentation.
#
# See :ref:`build` for more information on the build procedure.  
# This is an optional step to recreate the Python code from the documentation.
#
# The build process required PyLit3 and Sphinx to be installed.
# Setuptools can be used to simplify installation.
#
# -   PyLit.  https://github.com/slott56/PyLit-3
#
# -   Sphinx.  http://sphinx.pocoo.org/
#
# -   Setuptools.  http://pypi.python.org/pypi/setuptools
#
# ..  code-block:: bash
#
#     python3 build.py
#   
# Installation via Distutils
# =============================
#   
# To install ``stingray`` you can use the following.
# On Linux, this may require privileges via ``sudo``.
#
# ..  code-block:: bash
#
#     sudo python3 setup.py install
#   
# In some cases, you might want to break this down into a build step that
# doesn't require privileges and the final install step, which does require
# privileges.
#
# ..      code-block:: bash
#       
#         python3 setup.py build
#         sudo python3 setup.py install
#
# The ``setup.py`` File
# ======================
#
# To use Stingray, you must have the following package installed.
#
# -   xlrd.  http://www.lexicon.net/sjmachin/xlrd.htm
#
#     Version 0.9.2 is Python 3 compatible. https://pypi.python.org/pypi/xlrd/0.9.2
#
# This provides Python package information.
#
# ::

import setuptools

setuptools.setup(
    name='stingray',
    version='5.0',
    description='Schema-Based File Reader, COBOL, EBCDIC Conversion, ETL, Data Profiling',
    author='S.Lott',
    author_email='slott56@gmail.com',
    url='https://github.com/slott56/Stingray-Reader',
    packages=[
        'stingray', 
        ],

# XLRD is required for working with Excel .xls spreadsheets.
# JSONSchema is required to validate schema definitions.

    install_requires=[
        'jsonschema>=4.1',
        "xlrd>=0.9"
    ],

# Here are some `trove classifiers <http://pypi.python.org/pypi?%3Aaction=list_classifiers>`_.
#
# ::

    classifiers=[
        "Development Status :: 6 - Mature",
        "Environment :: Console",
        "Intended Audience :: Developers",
        "Operating System :: OS Independent",
        "Programming Language :: Python",
        "Programming Language :: COBOL",
        "Topic :: Database",
        "Topic :: Software Development :: Libraries",
        "Topic :: Software Development :: Quality Assurance",
        ],
    )

