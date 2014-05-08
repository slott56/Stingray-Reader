#!/usr/bin/env python3

# ..  _`build`:
#
# #########################
# Stingray Build
# #########################
#
# The source for :mod:`stingray` is a Sphinx project that depends on PyLit.
# Yes.  The documentation spawns the code.
#
# ..  important:: PyLit Feature
#
#     The first line of each file should be ``..    #!/usr/bin/env python3``.
#    
#     The four spaces between ``..`` and ``#!`` defines the indent used for
#     each line of code in the file.  
#    
#     Four spaces.
#
# Stingray depends on the following
#
# -   xlrd.  http://www.lexicon.net/sjmachin/xlrd.htm
#
#     Version 0.9.2 is Python 3 compatible. https://pypi.python.org/pypi/xlrd/0.9.2
#
# In addition to Python 3.3, there are two other projects used to build.
#
# -   PyLit.  https://github.com/slott56/PyLit-3
#
# -   Sphinx.  http://sphinx.pocoo.org/
#
# The PyLit install is little more than a download and move the :file:`pylit.py` file to
# the Python :file:`site-packages` directory.
#
# Sphinx and XLRD should be
# installed with `easy_install <http://peak.telecommunity.com/DevCenter/EasyInstall>`_.
#
# ..  code-block:: bash
#
#     easy_install xlrd
#     easy_install sphinx
#    
# In the case of having Python2 and Python3 installed, ``easy_install-3.3`` may be required.
# On most systems, ``sudo`` is also required.
#
# The diagrams are done with YUML. See http://yuml.me.
#
# Build Procedure
# ==================
#
# 1.  Bootstrap the :file:`build.py` script by running PyLit.
#
#     ..  code-block:: bash
#    
#         python3.3 -m pylit -t source/build.rst build.py
#
#     This reports that an extract was written to :file:`build.py`.
#    
# 2.  Use the :file:`build.py` script to create the ``stingray`` source, unit
#     tests, demonstration applications.  
#     Build the Sphinx documentation.  
#     And run the unit test, too.
#
#     ..  code-block:: bash
#    
#         python3.3 build.py
#        
#     At the end of this step, the directory tree will include the following.
#    
#     -   :file:`build`.  The documentation.  In HTML.
#     -   :file:`demo`.   Some demonstration applications that use ``stingray``. 
#         See :ref:`demo`.
#     -   :file:`stingray`.  The Python library, ready for installation.
#     -   :file:`test`.  The unit test script.
#
#     This reports, also, that 139 tests were run.
#    
# In general (i.e., any OS except Windows), it's sensible to do this:
#
# ..    code-block:: bash
#
#     chmod +x build.py
#
# This allows us to use the following for a rebuild:
#
# ..    code-block:: bash
#
#     ./build.py 
#    
#    
# Build Script Design
# =====================
#
# This is a platform-independent :file:`build.py` file for the build script.
# This can use ``from sphinx.application import Sphinx``
# and ``import pylit`` to access these modules from within Python
# instead of using command-line shell script techniques.
#
# Overheads
# -------------
#
# We're going to make use of three "applications".
#
# -   Sphinx top-level application.
#
# -   PyLit top-level application.
#
# -   Unittest top-level test runner.
#
# ::

"""Platform-independent build script"""
from __future__ import print_function
import os
import sys
import errno
from sphinx.application import Sphinx
import pylit
import unittest
import logging
import shutil

# Sphinx Build
# ---------------
#
# ..  py:function:: sphinx_build( srcdir, outdir, buildername='html' )
#
# This function handles the simple use case for the ``sphinx-build`` script.
#
# ::

def sphinx_build( srcdir, outdir, buildername='html' ):
    """Essentially: ``sphinx-build $* -b html source build/html``"""
    confdir= srcdir= os.path.abspath( srcdir )
    outdir= os.path.abspath( outdir )
    doctreedir = os.path.join(outdir, '.doctrees')
    app = Sphinx(srcdir, confdir, outdir, doctreedir, buildername)
    app.build(force_all=False, filenames=[])
    return app.statuscode

# PyLit Build
# ---------------
#
# ..  py:function:: pylit_build( srcdir, outdir )
#
# This function handles the simple use case for PyLit.
#
# This also handles the necessary rewrite to modify standard paths to Windows paths.
#
# ::

def pylit_build( infile, outfile ):
    """Essentially: ``python3 -m pylit -t source/demo/data_quality.rst demo/test.py``
    
    The issue here is that we need to provide platform-specific paths.
    """
    if os.sep != '/':
        # Fix windows paths.
        infile= os.path.join( *infile.split('/') )
        outfile= os.path.join( *outfile.split('/') )
    pylit.main( txt2code= True, overwrite="yes", infile= infile, outfile= outfile )

# Make Directories
# -------------------
#
# ..  py:function:: mkdir( path )
#
# This function handles the simple use case for assuring that the directory
# tree exists.
#
# This also handles a rewrite to modify standard paths to Windows paths.
#
# ::

def mkdir( path ):
    if os.sep != '/':
        # Fix windows paths.
        path= os.path.join( *path.split('/') )
    try:
        os.makedirs( path )
    except OSError as e:
        if e.errno == errno.EEXIST: 
            pass
        else:
            raise

# Copy Data File
# ---------------
#
# ..  py:function:: copy_file( srcdir, outdir )
#
# This function handles the simple use case for copying a file
#
# ::

def copy_file( srcdir, outdir ):
    """Essentially: ``cp srcdir outdir``"""
    shutil.copy2( srcdir, outdir )

# Run the Test Script
# -----------------------
#
# ..  py:function:: run_test( )
#
# In effect, this does ``python3 test/main.py``
#
# ::

def run_test():
    from test.main import suite
    from test import Logger
    with Logger( stream=sys.stdout, level=logging.WARN ):
        unittest.TextTestRunner().run(suite())

# The Build Sequence
# ---------------------
#
# ::
    
def build():
    mkdir( 'stingray/schema' )
    mkdir( 'stingray/cobol' )
    
    pylit_build( 'source/stingray_init.rst', 'stingray/__init__.py' )
    pylit_build( 'source/cell.rst', 'stingray/cell.py' )
    pylit_build( 'source/sheet.rst', 'stingray/sheet.py' )
    pylit_build( 'source/workbook.rst', 'stingray/workbook.py' )
    pylit_build( 'source/schema.rst', 'stingray/schema/__init__.py' )
    pylit_build( 'source/schema_loader.rst', 'stingray/schema/loader.py' )
    pylit_build( 'source/cobol_init.rst', 'stingray/cobol/__init__.py' )
    pylit_build( 'source/cobol_loader.rst', 'stingray/cobol/loader.py' )
    pylit_build( 'source/cobol_defs.rst', 'stingray/cobol/defs.py' )
    pylit_build( 'source/snappy.rst', 'stingray/snappy.py' )
    pylit_build( 'source/protobuf.rst', 'stingray/protobuf.py' )
    pylit_build( 'source/installation.rst', 'setup.py' )
    
    copy_file( 'source/Numbers.json', 'stingray/Numbers.json' )
    copy_file( 'source/Common.json', 'stingray/Common.json' )
    
    mkdir( 'test' )
        
    pylit_build( 'source/testing/test_init.rst', 'test/__init__.py' )
    pylit_build( 'source/testing/main.rst', 'test/main.py' )
    pylit_build( 'source/testing/cell.rst', 'test/cell.py' )
    pylit_build( 'source/testing/sheet.rst', 'test/sheet.py' )
    pylit_build( 'source/testing/schema.rst', 'test/schema.py' )
    pylit_build( 'source/testing/schema_loader.rst', 'test/schema_loader.py' )
    pylit_build( 'source/testing/workbook.rst', 'test/workbook.py' )
    pylit_build( 'source/testing/cobol.rst', 'test/cobol.py' )
    pylit_build( 'source/testing/cobol_loader.rst', 'test/cobol_loader.py' )
    pylit_build( 'source/testing/cobol_2.rst', 'test/cobol_2.py' )
    pylit_build( 'source/testing/snappy_protobuf.rst', 'test/snappy_protobuf.py' )
    
    mkdir( 'demo' )
        
    pylit_build( 'source/demo/data_quality.rst', 'demo/test.py' )
    pylit_build( 'source/demo/validation.rst', 'demo/app.py' )
    pylit_build( 'source/demo/profile.rst', 'demo/profile.py' )
    pylit_build( 'source/demo/cobol_reader.rst', 'demo/cobol_reader.py' )
    
    run_test()

    sphinx_build( 'source', 'build/html', 'html' )
    sphinx_build( 'source', 'build/latex', 'latex' )

# Main Program Switch
# ---------------------
#
# When the :file:`build.py` script is run from the command line,
# it will execute the :py:func:`build` function.  When it is imported,
# however, it will do nothing special.
#
# ::

if __name__ == "__main__":
    build()
    
# Additional Builds
# =====================
#
# Sometimes it's desriable to refresh the documentation.
#
# The HTML pages are built with this command.
#
# ..  code-block:: bash
#
#     sphinx-build $* -b html source build/html
#    
# The LaTeX document is built with this command.
#
# ..  code-block:: bash
#
#     sphinx-build $* -b latex source build/latex
