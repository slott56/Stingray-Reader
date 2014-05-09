#!/usr/bin/env python3

# .. _`workbook_init`:
#
# ###############################################################
# Workbook __init__ Module -- Wrapper for all implementations
# ###############################################################
#
# ..  py:module:: workbook
#
# A *Workbook* is a collection of *Sheets*.  It's also a set of decoding
# rules required to translate bytes (or XML text) into meaningful *Cell* instances.
#
# Access to cells of a Workbook requires two levels of schema:
#
# -   *Physical Format*.  The format required to locate cells.
#     CSV, XLS, XLSX, ODS, are all well-known physical formats and the physical
#     schema is implied by the file type.
#     Fixed format and COBOL format, are not well-known, and a physical
#     schema is required.
#
# -   *Logical Layout*. The columns or data elements present in the file.
#     This may depend on an embedded schema in the first rows of a Sheet.
#     Or it may depend on an external schema defined in another Workbook.
#
# This package addresses the physical format issues. It provides a common
# abstraction over a number of forms of workbook data.  It makes the physical
# format largely transparent to an application.
#
# It's difficult to make the logical layout transparent.
# See :ref:`developer` for guidelines on developing applications that
# are flexible with respect to logical layout.
#
# In a way, a Workbook is a factory for :py:class:`sheet.Sheet` and
# :py:class:`sheet.Row` objects.
#
# More interestingly, a Workbook is a factory for :py:class:`cell.Cell` instances.
# This is because the decoding of bytes to create a cell is entirely a feature
# of the Workbook.
#
# Use Case
# ==============
#
# See :ref:`intro` for our physical-format independence use case.
# A :py:func:`workbook.open_workbook` function allows a program to be
# independent of physical format.
#
# ..  parsed-literal::
#
#     def process_workbook( input ):
#         with workbook.open_workbook( input ) as source:
#             process_workbook( source );
#
#     if __name__ == "__main__":
#         *application startup*
#         for input in args.file:
#             process_workbook( input )
#
#
# This does not address logical layout issues, however, which are handled by a
# :py:class:`schema.Schema`.  We might load an embedded schema or an external schema.
#
# ..  parsed-literal::
#
#     def process_sheet( sheet ):
#         """Separated to facilitate unit testing"""
#         counts= defaultdict( int )
#         for rows in sheet.rows():
#             *process row*
#         return counts
#
#     def process_workbook( source ):
#         for name in source.sheets():
#             sheet= source.sheet( name,
#                 sheet.EmbeddedSchemaSheet,
#                 loader_class=schema.loader.HeadingRowSchemaLoader )
#             counts= process_sheet( sheet )
#             pprint.pprint( counts )
#
#
# Physical Formats
# =======================================
#
# Much data is transferred via formats
# tied to desktop spreadsheet software or
# informed by legacy mainframe design patterns.
# Data that comes from spreadsheet applications
# will have all the rich variety of desktop tools.
#
# -   CSV.  This includes the "quote-comma" dialetcs as used by spreadsheets
#     as well as "tab" or "pipe" dialetcs favored by Linux applications.
#
# -   ODS.  This is a zipped archive of XML documents from which data can be extracted.
#     This is an ECMA standard.  This is the Open Office Spreadsheet structure.
#     Most of the relevant data is in a content.xml member.
#
# -   XLSX or XLSM.  This is a zipped archive of XML documents from which data can be extracted.
#     This is an ECMA standard.
#
# -   XLS.  This is the proprietary "Horrible Spreadsheet Format" (HSSF) as used by
#     Microsoft products.  
#     We require `xlrd <http://www.lexicon.net/sjmachin/xlrd.htm>`_ to extract data from these files.
#
# -   Apple iWorks '09 Numbers formats. 
#     The iWorks '09 physical format is a simple ZipFile with a big XML document.
#
# -   Apple iWorks '13 Numbers formats. 
#     iWorks '13 physical format is the "bundle" or "package" format; the document
#     is a directory, which contains a zip archive of .IWA files. These use snappy
#     compression and protobuf object representation.
#
# -   Fixed Format, COBOL-style.  Yes, these files still exist.  For
#     these files, schema information is *required* to determine where
#     the fields are, since there's no puctuation. We can convert EBCDIC bytes or work
#     in Unicode-compatible text. ASCII encoding is usually handled trivially by
#     Python's ``io`` module.
#
# -   Other XML. For example, an Omni Outliner outlines with a normalized format.
#     This is a possible future direction.
#
#
# We'll call ``CSV``, ``XLS``, ``XLSX`` / ``XLSM`` and ``ODS``
# the "well-known physical formats."
# They don't require physical schema information in order
# to identify the data items.
#
# The Fixed and COBOL format files, on the other hand, require physical schema information.  
# We'll look at COBOL in depth, in :ref:`cobol`.
#
# iWork Numbers
# ===============
#
# The Stingray model of sheet/row/cell structure does not
# easily fit the Numbers sheet/table/row/cell structure.
#
# Option 1:
#
#     Workbook -> new layer (Numbers "Workspace") -> Sheet (Numbers "Table") -> Row -> Cell
#
# Option 2:
#
#     Combine (Workspace,Table) into a 2-tuple, and call this a "sheet" name
#    
#     This will fit with Stingray acceptably. 
#
#
# Model
# ======
#
# ..  code-block:: none
#
#     http://yuml.me/diagram/scruffy;/class/
#     #workbook,
#     [Workbook]^[CSV_Workbook],
#     [Workbook]^[XLS_Workbook],
#     [Workbook]^[XLSX_Workbook],
#     [Workbook]^[Fixed_Workbook],
#     [Workbook]^[ODS_Workbook],
#     [Workbook]<>-[Sheet],
#     [Sheet]<>-[Row],
#     [Workbook]->[Schema].
#
#
# ..  image:: workbook.png
#     :width: 6in
#
# Implementation Overheads
# ================================
#
# ..  todo:: Refactor workbook package
#    
#     This module needs to be rebuilt into a package which 
#     imports a number of subsidiary modules. It's too large
#     as written.
#    
#     Adding Numbers '13 will make this module even more monstrous.
#     Adding future spreadsheets will on exacerbate the problem.
#    
#     It should become (like :py:mod:`cobol`) a high-level package
#     that imports top-level classes from modules within the package.
#    
#         ``from workbook.csv import CSV_Workbook``
#        
#         ``from workbook.xls import XLS_Workbook``
#        
#         ... *etc.* ...
#        
#     This should make a transparent change from module to package.
#    
#     The top-level definition for :py:class:`cobol.Workbook` must
#     to be refactored into a ``base`` module that can be shared by
#     all the modules in the package that extend this base definition.
#    
# A few Python overheads that must be physically first in the
# resulting module.
#
# ::

"""stingray.workbook -- Opens workbooks in various
formats, binds their associated schema, accesses them as Sheets with
Rows and Cells.

This is a kind of **Wrapper** or **Facade** that unifies :py:mod:`csv` and
:py:mod:`xlrd`. It handles a number of file formats including
:file:`.xlsx`, :file:`.ods`, and Numbers.
"""

# We depend on the following
#
# ``xlrd``    https://pypi.python.org/pypi/xlrd/0.9.2 http://www.lexicon.net/sjmachin/xlrd.htm
#
# We'll rely on definitions of :py:mod:`cell`, :py:mod:`sheet`,
# and :py:mod:`schema.loader`. We have an implicit dependency on :py:mod:`schema`: 
# we'll be given schema objects to work with.
#
# ::

import xlrd

import xml.etree.cElementTree as dom
from collections import defaultdict
import zipfile
import datetime
from io import open
import os.path
import pprint
import re
import glob
import logging
import decimal

import stingray.cell
import stingray.sheet
import stingray.schema.loader

from stingray.workbook.csv import CSV_Workbook
from stingray.workbook.xls import XLS_Workbook
from stingray.workbook.xlsx import XLSX_Workbook
from stingray.workbook.ods import ODS_Workbook
from stingray.workbook.numbers_09 import Numbers09_Workbook
from stingray.workbook.numbers_13 import Numbers13_Workbook
from stingray.workbook.fixed import Fixed_Workbook

# Workbook Subclasses
# =====================
#
# We have a number of concrete subclasses of :py:class:`workbook.Workbook`.
#
# -   :py:class:`workbook.CSV_Workbook`.  This is a degenerate case, where the workbook appears to contain
#     a single sheet.  This sheet is the CSV file, accessed via the built-in
#     :py:func:`csv.reader`.
#
# -   :py:class:`workbook.XLS_Workbook`.  This is the workbook as processed by :py:mod:`xlrd`.  These classes
#     wrap :py:mod:`xlrd` classes to which the real work is delegated.
#
# -   :py:class:`workbook.XLSX_Workbook`.  This is the workbook after unzipping and using an XML parser
#     on the various document parts.  Mostly, this is a matter of unzipping
#     and parsing parts of the document to create a DOM which can be traversed
#     as needed.
#
# -   :py:class:`workbook.Fixed_Workbook`.  This is actually a fairly complex case.  The workbook will appear to
#     contain a single sheet; this sheet is the fixed format file.  Schema information
#     was required up front, unlike the other formats.
#
# -   :py:class:`workbook.Numbers09_Workbook`.
#     This handles the iWork '09 Numbers files with multiple 
#     workspaces and multiple tables in each workspace. 
#
# -   :py:class:`workbook.Numbers13_Workbook`
#     These handle the iWork '13 Numbers files with multiple 
#     workspaces and multiple tables in each workspace. 
#    
# -   :py:class:`workbook.ODS_Workbook`.
#
# Extensions will handle various kinds of COBOL files. They're sumular to Fixed Workbooks.
#
# We'd each of these to be a context manager, so we include
# the necessary methods.
#
# Note that workbooks are rarely simple files.  Sometimes they are ZIP archive
# members.  Sometimes, they must be processed through gzip. Sometimes they involve
# Snappy compression.
#
# In order to minimize the assumptions, we try to handle two forms of file processing:
#
# -   By name. In this case, the file name is provided. The file is opened and closed by
#     the Workbook using the context manager interface.
#
# -   By file-like object. An open file-like object is provided. No additional
#     context management is performed. This is appropriate when a workbook is itself
#     a member of a larger archive.
#
#
# Workbook Factory
# =================
#
# ..  py:class:: No_Schema
#
# The :py:class:`No_Schema` exception is raised if there's a problem
# loading a schema.
#
# ::

class No_Schema( Exception ):
    """A valid schema could not be loaded."""
    pass

# ..  py:class:: Opener
#
# An opener **Factory** class.  A subclass can extend this to handle other file
# extensions and physical formats.
#
# ::

class Opener:
    """An extensible opener that examines the file extension."""
    def __call__( self, name, file_object=None,
        schema_path='.', schema_sheet= None, **kw ):
        """Open a workbook.
        :param name: filename to open.
        :param file_object: File-like object to process.  If not
        provided the named file will be opened.
        :param schema_path: Directory with external schema files
        :param schema_sheet: A sheet in an external schema workbook.
        """
        _, ext = os.path.splitext( name )
        ext = ext.lower()
        if ext == ".xls": return XLS_Workbook( name, file_object )
        elif ext in ( ".xlsx", ".xlsm" ):
            return XLSX_Workbook( name, file_object )
        elif ext in ( ".csv", ):
            return CSV_Workbook( name, file_object, **kw )
        elif ext in ( ".tab", ):
            return CSV_Workbook( name, file_object, delimiter='\t', **kw )
        elif ext in ( ".ods", ):
            return ODS_Workbook( name, file_object )
        elif ext in ( ".numbers", ):
            # Directory? It's Numbers13_Workbook; Zipfile? It's Numbers09_Workbook
            if os.path.is_dir( name ):
                return Numbers13_Workbook( name, file_object )
            else:
                return Numbers09_Workbook( name, file_object )
        else:
            # Ideally somefile.schema is the file
            # and schema.csv can be tracked down.
            schema_pat= os.path.join(schema_path, ext[1:]+".*")
            schema_choices= glob.glob( schema_pat )
            if schema_choices:
                schema_name= schema_choices[0]
                schema_wb= open_workbook( schema_name )
                esl= stingray.schema.loader.ExternalSchemaLoader( schema_wb, schema_sheet )
                schema= esl.schema()
                return Fixed_Workbook( name, file_object, schema=schema )
            else:
                raise No_Schema( schema_pat )

# ..  py:function:: open_workbook( name, file_object, schema_path, schema_sheet )
#
# The default :py:func:`workbook.open_workbook` is simply an instance
# of the :py:class:`workbook.Opener`.
#
# ::

open_workbook= Opener()

# This allows a user to create subclasses to handle the various other file name extensions.
# Often, there are application-specific rules, or command-line options that
# will determine a mapping bewtween filename and physical format.
#
# Also, an application may require external schema, or there may be an optional
# external schema with application-specific rules for handling this.
#
# For fixed format files, we attempt to track down and load the relevant
# schema.  An application might have narrower and more specific rules
# for binding file and schema.  See below for the :py:class:`schema.loader.ExternalSchemaLoader` class.
#
