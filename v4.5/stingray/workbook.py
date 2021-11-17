#!/usr/bin/env python3

# .. _`workbook`:
#
# ###############################################################
# Workbook Module -- Uniform Wrapper on All Workbooks
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
#     Adding Numbers '13 will make it even more monstrous.
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
#     to be refactored into a "defs" module that can be shared by
#     all the modules in the package.
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
import csv
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


# Workbook Structure
# ====================
#
# ..  py:class:: Workbook
#
# We note that these physical formats all encode a single, common data structure.
# Here are some abstract definitions.
#
# ::

class Workbook:
    """A workbook file; a collection of Sheets."""
    def __init__( self, name, file_object=None ):
        """Prepare the workbook for reading.

        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
        """
        self.name, self.file_obj= name, file_object
        self.the_file = None # Any internal files
        self.datemode= 0 # For xlrd
        self.log= logging.getLogger( self.__class__.__qualname__ )
    def __repr__( self ):
        return "{0}({1!r})".format( self.__class__.__qualname__, self.name )
        
# ..  py:method:: Workbook.sheet( sheet_name, sheet_type, *args, **kw )
#
#     There are two varieties of sheets, depending on the presence or
#     absence of a schema.
#
# ::

    def sheet( self, sheet_name, sheet_type=None, *args, **kw ):
        """Returns a :py:class:`sheet.Sheet`, ready for processing."""
        if sheet_type is None: sheet_type= stingray.sheet.Sheet
        sheet = sheet_type( self, sheet_name, *args, **kw )
        return sheet

# ..  py:method:: Workbook.sheets( )
#
# ::

    def sheets( self ):
        """List of sheet names.
        The filename is a handy default for CSV and Fixed files.
        """
        nm, _ = os.path.splitext( os.path.basename(self.name) )
        return [ nm ]

# The Context Manager interface.
#
# ::

    def __enter__( self ):
        return self
    def __exit__( self, exc_type, exc_val, exc_tb ):
        if self.the_file:
            self.the_file.close()
        if exc_type is not None: return False

# ..  py:method:: Workbook.rows_of( sheet )
#
# ::

    def rows_of( self, sheet ):
        """An iterator over all rows of the given sheet."""
        raise NotImplementedError

# ..  py:method:: Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attribute ):
        """Create a Cell from the row's data."""
        raise NotImplementedError


# There are distinct subclasses of :py:class:`workbook.Workbook`, based on the
# physical file format.
#
# Many of our physical formats don't require any physical schema information.
# A **Fixed** file, however, requires
# a physical format schema definition in order to decompose
# each line into cells.
#
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
# CSV Workbook
# ---------------
#
# ..  py:class:: CSV_Workbook
#
# We're wrapping the :py:func:`csv.reader`.  We need to create proper
# :py:class:`cell.TextCell` instances instead of the default string values
# that :py:mod:`csv` normally creates.
#
# ::

class CSV_Workbook( Workbook ):
    """Uses ``csv.reader``.  There's one sheet only."""
    def __init__( self, name, file_object=None, **kw ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
            If provided, it must be opened with  newline='' to permit non-standard
            line-endings.

        The kw are passed to :py:func:`csv.reader`
        to provide dialect information."""
        super().__init__( name, file_object )
        if self.file_obj:
            self.the_file= None
            self.rdr= csv.reader( self.file_obj, **kw )
        else:
            self.the_file = open( name, 'r', newline='' )
            self.rdr= csv.reader( self.the_file, **kw )

# We can build an eager :py:class:`sheet.Row` or a :py:class:`sheet.LazyRow` from
# the available data.
# The eager Row includes the conversions.  The :py:class:`sheet.LazyRow` defers
# the conversions until the callback to :py:meth:`workbook.Workbook.row_get`.
#
# ..  py:method:: CSV_Workbook.rows_of( sheet )
#
# ::

    def rows_of( self, sheet ):
        """An iterator over all rows of the named sheet.
        For CSV files, the sheet.name is simply ignored.
        """
        for data in self.rdr:
            logging.debug( pprint.pformat( data, indent=4 ) )
            row = stingray.sheet.Row(sheet, *(stingray.cell.TextCell(col, self) for col in data))
            yield row

# ..  py:method:: CSV_Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attribute ):
        """Create a Cell from the row's data."""
        return row[attribute.position]

# Since :py:mod:`csv` is eager, returning an individual :py:class:`cell.TextCell`
# is easy.
#
# XLS Workbook
# --------------
#
# ..  py:class:: XLS_Workbook
#
# This definition of a workbook wraps ``xlrd`` so that it fits the Stingray framework.   
# We'll use proper :py:class:`cell.Cell` subclass instances instead of the default ``xlrd.Cell``
# values that ``xlrd`` normally creates.
#
# ::

class XLS_Workbook( Workbook ):
    """Uses ``xlrd``."""
    def __init__( self, name, file_object=None, **kw ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.

        The kw arguments are passed to :py:func:`xlrd.open_workbook`.
        """
        super().__init__( name, file_object )
        if self.file_obj:
            self.wb= xlrd.open_workbook( self.name, file_contents=self.file_obj.read(), **kw )
        else:
            self.wb= xlrd.open_workbook( self.name, **kw )
        self.datemode= self.wb.datemode

# ..  py:method:: XLS_Workbook.sheets( )
#
# ::

    def sheets( self ):
        """List of sheet names."""
        return self.wb.sheet_names()

# We can build an eager :py:class:`sheet.Row` or a :py:class:`sheet.LazyRow` from the available data.
# The eager Row includes the conversions.  The LazyRow defers the conversions
# until the callback to :py:meth:`XLS_Workbook.row_get`.
#
# ..  py:method:: XLS_Workbook.rows_of( sheet )
#
# ::

    def rows_of( self, sheet ):
        """An iterator over all rows of the given sheet."""
        self.sheet= self.wb.sheet_by_name(sheet.name)
        for n in range(self.sheet.nrows):
            data = self.sheet.row(n)
            row = stingray.sheet.Row(sheet, *(self.cell(col) for col in data))
            yield row

# ..  py:method:: XLS_Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attribute ):
        """Create a Cell from the row's data."""
        return row[attribute.position]

# In :py:meth:`XLS_Workbook.rows_of` we built a row eagerly.
# That way, returning an individual Cell is easy.
#
# Convert a single ``xlrd.Cell`` to a proper subclass of :py:class:`cell.Cell`
#
# ::

    def cell( self, xlrd_cell ):
        if xlrd_cell.ctype == xlrd.XL_CELL_EMPTY:
            return stingray.cell.EmptyCell('', self)
        elif xlrd_cell.ctype == xlrd.XL_CELL_TEXT:
            return stingray.cell.TextCell(xlrd_cell.value, self)
        elif xlrd_cell.ctype == xlrd.XL_CELL_NUMBER:
            return stingray.cell.NumberCell(xlrd_cell.value, self)
        elif xlrd_cell.ctype == xlrd.XL_CELL_DATE:
            return stingray.cell.FloatDateCell(xlrd_cell.value, self)
        elif xlrd_cell.ctype == xlrd.XL_CELL_BOOLEAN:
            return stingray.cell.BooleanCell(xlrd_cell.value, self)
        elif xlrd_cell.ctype == xlrd.XL_CELL_ERROR:
            return stingray.cell.ErrorCell(
                xlrd.error_text_from_code[xlrd_cell.value], self )
        elif xlrd_cell.ctype == xlrd.XL_CELL_BLANK:
            return stingray.cell.EmptyCell('', self)
        else:
            raise ValueError( "Damaged Workbook" )

# XLSX or XLSM Workbook
# -----------------------
#
# ..  py:class:: XLSX_Workbook
#
# We're opening a ZIP archive and parsing the various XML documents
# that we find therein.
#
# The :py:class:`ElementTree` incremental parser provides
# parse "events" for specific tags, allowing for lower-memory parsing of
# the sometimes large XML documents.
#
# See http://effbot.org/zone/element-iterparse.htm
#
# The class as a whole defines some handy constants like XML namespaces
# and a pattern for parsing Cell ID's to separate the letters from the numbers.
#
# ::

class XLSX_Workbook( Workbook ):
    """ECMA Standard XLSX or XLSM documents.
    Locate sheets and rows within a given sheet.

    See http://www.ecma-international.org/publications/standards/Ecma-376.htm
    """
    # Relevant subset of namespaces used
    XLSX_NS = {
    "main":"http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    "r":"http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "rel":"http://schemas.openxmlformats.org/package/2006/relationships",
    }
    cell_id_pat = re.compile( r"(\D+)(\d+)" )
    def __init__( self, name, file_object=None ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
        """
        super().__init__( name, file_object )
        self.zip_archive= zipfile.ZipFile( file_object or name, "r" )
        self._prepare()

# The are two preparation steps required for reading these files.  First, the
# sheets must be located.  This involves resolving internal rID numbers.
# Second, the shared strings need to be loaded into memory.
#
# ::

    def _prepare( self ):
        self._locate_sheets()
        self._get_shared_strings()

# Locate all sheets involves building a :py:data:`name_to_id` mapping and  and :py:data:`id_to_member` mapping.  This allows is to map the
# user-oriented name to an id and the id to the XLSX zipfile member.
#
# ::

    def _locate_sheets( self ):
        """Locate the name to id mapping and the id to member mapping.
        """
        # 1a. Open "workbook.xml" member.
        workbook_zip= self.zip_archive.getinfo("xl/workbook.xml")
        workbook_doc= dom.parse( self.zip_archive.open(workbook_zip) )
        # 1b. Get a dict of sheet names and their rIdx values.
        key_attr_id= 'name'
        val_attr_id= dom.QName( self.XLSX_NS['r'], 'id' )
        self.name_to_id = dict(
            ( s.attrib[key_attr_id], s.attrib[val_attr_id] )
            for s in workbook_doc.findall("*/main:sheet", namespaces=self.XLSX_NS)
        )
        logging.debug( self.name_to_id )

        # 2a. Open the "_rels/workbook.xml.rels" member
        rels_zip= self.zip_archive.getinfo("xl/_rels/workbook.xml.rels")
        rels_doc= dom.parse( self.zip_archive.open(rels_zip) )
        # 2b. Get a dict of rIdx to Target member name
        logging.debug( dom.tostring( rels_doc.getroot() ) )
        key_attr_id= 'Id'
        val_attr_id= 'Target'
        self.id_to_member = dict(
            ( r.attrib[key_attr_id], r.attrib[val_attr_id] )
            for r in rels_doc.findall("rel:Relationship", namespaces=self.XLSX_NS)
        )
        logging.debug( self.id_to_member )

# Get Shared Strings walks a fine line.  Ideally, we'd like to parse
# the document and simply use ``itertext`` to gather all of the text
# within a given string instance (:samp:`<si>`) tag.  **However.**
#
# In practice, these documents can be so huge that they don't fit
# in memory comfortably.  We rely on incremental parsing via the ``iterparse`` function.
#
# ::

    def _get_shared_strings( self ):
        """Build ``strings_dict`` with all shared strings.
        """
        self.strings_dict= defaultdict(str)
        count= 0
        text_tag= dom.QName( self.XLSX_NS['main'], "t" )
        string_tag= dom.QName( self.XLSX_NS['main'], "si" )
        # 1. Open the "xl/sharedStrings.xml" member
        sharedStrings_zip= self.zip_archive.getinfo("xl/sharedStrings.xml")
        for event, element in dom.iterparse(
            self.zip_archive.open( sharedStrings_zip ), events=('end',) ):
            logging.debug( event, element.tag )
            if element.tag == text_tag:
                self.strings_dict[ count ]+= element.text
            elif element.tag == string_tag:
                count += 1
            element.clear()
        logging.debug( self.strings_dict )

# The shared strings may be too massive for in-memory incremental parsing.
# We can create a temporary extract file to handle this case. Here's
# the kind of code we might use.
#
# ..  parsed-literal::
#
#     with tempfile.TemporaryFile( ) as temp:
#         self.zip_archive.extract( sharedStrings_mbr, temp.filename )
#         for event, element in dom.iterparse( temp.filename ):
#             *process event and element*
#
# ..  py:method:: XLSX_Workbook.sheets( )
#
# ::

    def sheets( self ):
        return self.name_to_id.keys()

# Translate a col-row pair from :samp:`({letter}, {number})` 
# to proper 0-based Python index of :samp:`({row}, {col})`.
#
# ::

    @staticmethod
    def make_row_col( col_row_pair ):
        col, row = col_row_pair
        cn = 0
        for char in col_row_pair[0]:
            cn = cn*26 + (ord(char)-ord("A")+1)
        return int(row), cn-1

# We can build an eager :py:class:`sheet.Row` or a  :py:class:`sheet.LazyRow` from the available data.
# The eager :py:class:`sheet.Row` is built from :py:class:`cell.Cell` objects.  
# The :py:class:`sheet.LazyRow` delegates the creation
# of :py:class:`cell.Cell` objects to :py:meth:`Workbook.row_get`.
#
# This uses an incremental parser, also.  There are four kinds of tags that
# have to be located.
#
# -   :samp:`<row>{row}</row>`, end event.  Finish (and yield) the row of cells.
#     Since XLSX is sparse, missing empty cells must be filled in.
#
# -   :samp:`<c t="{type}" r="{id}">{cell}</c>`.
#
#     -   Start event for ``c``.  Get the cell type and id.  Empty the value accumulator.
#
#     -   End event for ``c``.  Save the accumulated value.  This allows the cell to have
#         mixed content model.
#
# -   :samp:`<v>{value}</v>`, end event. Use the :py:meth:`cell` method to track down
#     enough information to build the Cell instance.
#
# ..  py:method:: XLSX_Workbook.rows_of( sheet )
#
# ::

    def rows_of( self, sheet ):
        """Iterator over rows as a list of Cells for a named worksheet."""
        # 1. Map user name to member.
        rId = self.name_to_id[sheet.name]
        self.sheet_member_name = self.id_to_member[rId]
        # 2. Open member.
        sheet_zip= self.zip_archive.getinfo("xl/"+self.sheet_member_name)
        self.row= {}
        # 3. Assemble each row, allowing for missing cells.
        row_tag= dom.QName(self.XLSX_NS['main'], "row")
        cell_tag= dom.QName(self.XLSX_NS['main'], "c")
        value_tag= dom.QName(self.XLSX_NS['main'], "v")
        format_tag= dom.QName(self.XLSX_NS['main'], "f")
        
        for event, element in dom.iterparse(
            self.zip_archive.open(sheet_zip), events=('start','end') ):
            logging.debug( element.tag, repr(element.text) )
            if event=='end' and element.tag == row_tag:
                # End of row: fill in missing cells
                if self.row.keys():
                    data= stingray.sheet.Row(sheet, *(
                        self.row.get(i, stingray.cell.EmptyCell('', self))
                        for i in range(max(self.row.keys())+1) ))
                    yield data
                else:
                    yield stingray.sheet.Row(sheet)
                self.row= {}
                element.clear()
            elif event=='end' and element.tag == cell_tag:
                # End of cell: consolidate the final string
                self.row[self.row_col[1]] = self.value
                self.value= stingray.cell.EmptyCell('', self)
            elif event=='start' and element.tag == cell_tag:
                # Start of cell: collect a string in pieces.
                self.cell_type= element.attrib.get('t',None)
                self.cell_id = element.attrib['r']
                id_match = self.cell_id_pat.match( self.cell_id )
                self.row_col = self.make_row_col( id_match.groups() )
                self.value= stingray.cell.EmptyCell('', self)
            elif event=='end' and element.tag == value_tag:
                # End of a value; what type was it?
                self.value= self.cell( element )

            elif event=='end' and element.tag == format_tag:
                pass # A format string
            else:
                pass
                logging.debug( "Ignoring", end="" ) # Numerous bits of structure exposed.
                logging.debug( dom.tostring(element) )

# ..  py:method:: XLSX_Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attribute ):
        """Create a Cell from the row's data."""
        return row[attribute.position]

# Build a subclass of :py:class:`cell.Cell` from the current value tag content plus the
# containing cell type information.
#
# ::

    def cell( self, element ):
        """Create a proper :py:class:`cell.Cell` subclass from cell and value information."""
        logging.debug( self.cell_type, self.cell_id, element.text )
        if self.cell_type is None or self.cell_type == 'n':
            try:
                return stingray.cell.NumberCell(float(element.text), self)
            except ValueError:
                print( self.cell_id, element.attrib, element.text )
                return None
        elif self.cell_type == "s":
            try:
                # Shared String?
                return stingray.cell.TextCell(self.strings_dict[int(element.text)], self)
            except ValueError:
                # Inline String?
                logging.debug( self.cell_id, element.attrib, element.text )
                return stingray.cell.TextCell(element.text, self)
            except KeyError:
                # Not a valid shared string identifier?
                logging.debug( self.cell_id, element.attrib, element.text )
                return stingray.cell.TextCell(element.text, self)
        elif self.cell_type == "b":
            return stingray.cell.BooleanCell(float(element.text), self)
        elif self.cell_type == "d":
            return stingray.cell.FloatDateCell(float(element.text), self)
        elif self.cell_type == "e":
            return stingray.cell.ErrorCell(element.text, self)
        else:
            # 'str' (formula), 'inlineStr' (string), 'e' (error)
            print( self.cell_type, self.cell_id, element.attrib, element.text )
            logging.debug( self.strings_dict.get(int(element.text)) )
            return None

# Fixed-Format Workbook
# -----------------------
#
# ..  py:class:: Fixed_Workbook
#
# Like a CSV workbook, this is a kind of degenerate case.  We don't have
# a lot of sheets, or a lot of data types.
#
# A subclass might do EBCDIC conversion and possibly even decode
# packed decimal numbers.  To do this, a COBOL-language DDE would be
# required as the schema definition.
#
# ::

class Fixed_Workbook( Workbook ):
    """A file with fixed-sized, no-punctuation fields.

    A schema is **required** to parse the attributes.

    The rows are defined as :py:class:`stingray.sheet.LazyRow` instances so that
    bad data can be gracefully skipped over.
    """
    row_class= stingray.sheet.LazyRow
    
    def __init__( self, name, file_object=None, schema=None ):
        """Prepare the workbook for reading.

        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
        :param schema: Schema required for processing.
        """
        super().__init__( name, file_object )
        if self.file_obj:
            self.the_file= None
            self.wb= self.file_obj
        else:
            self.the_file = open( name, 'rt' )
            self.wb= self.the_file
        self.schema= schema

# ..  py:method:: Fixed_Workbook.sheet( sheet )
#
# ::

    def sheet( self, sheet_name ):
        """sheet_type is ignored; it must be an external schema."""
        return stingray.sheet.ExternalSchemaSheet(self, sheet_name, schema=self.schema)

# We can build eager :py:class:`sheet.Row` instances for some
# kinds of flat files.  Eager rows, however, don't generalize well to COBOL structures.
#
# Therefore, we must build  :py:class:`sheet.LazyRow` objects here and defer the
# data type conversion until :py:meth:`workbook.Fixed_Workbook.row_get`.
# Or :py:meth:`cobol.COBOL_File.row_get`, which can be more complex still.
#
# ..  py:method:: Fixed_Workbook.rows_of( sheet )
#
# ::

    def rows_of( self, sheet ):
        """An iterator over all rows of the named sheet.
        For Fixed files, the sheet.name is simply ignored.
        """
        self.sheet= sheet
        for data in self.wb:
            logging.debug( pprint.pformat( data, indent=4 ) )
            row = self.row_class( sheet, data=data )
            yield row

# ..  py:method:: Fixed_Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attr ):
        """Create a :py:class:`cell.Cell` from the row's data."""
        extract= row._state['data'][attr.offset:attr.offset+attr.size]
        return attr.create( extract.rstrip(), self )

# ODS Workbook
# ---------------
#
# ..  py:class:: ODS_Workbook
#
# We should use ``iterparse`` rather than simply parsing the entire document.
# If the document is large, then we can't hold it all in memory.
#
# ::

class ODS_Workbook( Workbook ):
    """Standard OOO ODS document.
    Locate sheets and rows within a given sheet.
    """
    ODS_NS = {
    "office":"urn:oasis:names:tc:opendocument:xmlns:office:1.0",
    "table":"urn:oasis:names:tc:opendocument:xmlns:table:1.0",
    "text":"urn:oasis:names:tc:opendocument:xmlns:text:1.0",
    }
    date_format = "%Y-%m-%d"
    def __init__( self, name, file_object=None ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
        """
        super().__init__( name, file_object )
        self.zip_archive= zipfile.ZipFile( file_object or name, "r" )
        self._prepare()

# As preparation for reading these files, we locate all the sheet names.
#
# ::

    def _prepare( self ):
        self._locate_sheets()

# Locating all the sheets is a matter of doing an XPath search for
# :samp:`body/spreadsheet/table` and getting the *name* attribute
# from the  :samp:`<table name="{name}">` tags.
#
# ::

    def _locate_sheets( self ):
        """Create ``tables`` map from name to table."""
        workbook_zip= self.zip_archive.getinfo("content.xml")
        workbook_doc= dom.parse( self.zip_archive.open(workbook_zip) )
        name_attr_id= dom.QName( self.ODS_NS["table"], "name" )
        logging.debug( dom.tostring( workbook_doc.getroot() ) )
        self.tables= dict(
            (t.attrib[name_attr_id],t)
            for t in workbook_doc.findall("office:body/office:spreadsheet/table:table", 
                namespaces=self.ODS_NS) )

# An ``iterparse`` version to locate sheets
# would look for start of ``table`` tags and then get
# the name attribute from that tag.
#
# ..  py:method:: ODS_Workbook.sheets( )
#
# ::

    def sheets( self ):
        return self.tables.keys()

# We can build an eager :py:class:`sheet.Row` or a :py:class:`sheet.LazyRow` from
# the available data.  The eager Row includes the conversions.
# The LazyRow defers the conversions to :py:meth:`ODS_Workbook.row_get`.
#
# In ODS documents, the cell's value can be carried in the value attribute or
# it can be a mixed content value of the element.  There are three cases.
#
# -   :samp:`<table-cell value-type="{type}" value="{value}">...</table-cell>`
#
# -   :samp:`<table-cell value-type="{type}" date-value="{value}">...</table-cell>`
#
# -   :samp:`<table-cell value-type="{type}">{value}</table-cell>`
#
# ..  py:method:: ODS_Workbook.rows_of( )
#
# ::

    def rows_of( self, sheet ):
        """Iterator over rows as a list of Cells for a named worksheet."""
        for r, row_doc in enumerate(
            self.tables[sheet.name].findall( "table:table-row", namespaces=self.ODS_NS ) ):
            row= []
            for c, cell_doc in enumerate( row_doc.findall( "table:table-cell", namespaces=self.ODS_NS ) ):
                row.append( self.cell(cell_doc) )
            yield row

# ..  py:method:: ODS_Workbook.row_get( row, attribute )
#
# ::

    def row_get( self, row, attribute ):
        """Create a Cell from the row's data."""
        return row[attribute.position]

# Build a subclass of :py:class:`cell.Cell` from the current type name and value.
#
# ..  todo:: Refactor this, it feels clunky.
#
# ::

    def cell( self, cell_doc ):
        logging.debug( dom.tostring(cell_doc) )
        value_attr_id= dom.QName( self.ODS_NS['office'], 'value' )
        date_attr_id= dom.QName( self.ODS_NS['office'], 'date-value' )
        type_attr_id= dom.QName( self.ODS_NS['office'], 'value-type' )
        # Get the type
        try:
            type_name= cell_doc.attrib[type_attr_id]
        except KeyError:
            return stingray.cell.EmptyCell('', self)
        value= None
        # Date value as attribute?
        if not value:
            try:
                value= cell_doc.attrib[date_attr_id]
            except KeyError:
                pass
        # Other value as attribute?
        if not value:
            try:
                value= cell_doc.attrib[value_attr_id]
            except KeyError:
                pass
        # No value attributes, get *all* the text content.
        if not value:
            value= "".join( x for x in cell_doc.itertext() )
        if not value:
            # TODO: Proper warning.
            dom.dump( cell_doc )
        logging.debug( type_name, repr(value) )
        if type_name == "string":
            return stingray.cell.TextCell(value, self)
        elif type_name == "float":
            return stingray.cell.NumberCell(float(value), self)
        elif type_name == "date":
            theDate= datetime.datetime.strptime(
                value, ODS_Workbook.date_format )
            return stingray.cell.FloatDateCell(theDate, self)
        elif type_name == "boolean":
            return stingray.cell.BooleanCell(
                float(value.upper()=='TRUE'),  self )
        elif type_name == "empty":
            return stingray.cell.EmptyCell('', self)
        else: 
            raise Exception( "Unknown cell {0}".format( dom.tostring(cell_doc) ) )
            
# An ``iterparse`` version of building a row
# would look for start of ``table`` tags and then get
# the name attribute from that tag just to locate the right sheet.
#
# Once the sheet was located, then the row and cell tags would be used
#
# -   At :samp:`<table-row` start: increment row number, reset buffer
#
# -   At :samp:`<table-row` end: yield the row
#
# -   At :samp:`<table-cell` start: check for empty, date, float, boolean types,
#     which are available as an attribute at start.
#     For strings, start accumulating string values.
#
# -   At :samp:`<table-cell` end: finalize the accumulated value.
#
# Numbers 09 Workbook
# ---------------------
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
# The iWork 09 format is a (simpler) Zip file with an XML document inside it.
# There may be slight variations between native Numbers '09 and Numbers '13 doing
# a "save as" in Numbers '09 format.
#
# Numbers '13 is entirely different. See `Numbers 13 Workbook`_.
#
# ..  py:class:: Numbers09_Workbook
#
# ::

class Numbers09_Workbook( Workbook ):
    """Mac OS X Numbers Workbook for iWork 09.
    
    The ``.numbers`` "file" is a ZIP file.
    The index.xml is the interesting part of this.
    """
    NUMBERS_NS = {
    "ls":"http://developer.apple.com/namespaces/ls",
    "sf":"http://developer.apple.com/namespaces/sf",
    "sfa":"http://developer.apple.com/namespaces/sfa",
    }
    row_debug= False
    def __init__( self, name, file_object=None ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object. Ignored for v3.2 numbers files.
        """
        super().__init__( name, file_object )
        self.zip_archive= zipfile.ZipFile( file_object or name, "r" )
        self._prepare()

# As preparation for reading these files, we locate all the sheet names
# and all the number styles.
#
# ::

    def _prepare( self ):
        """Locate sheets/tables and styles."""
        root= dom.parse( self.zip_archive.open('index.xml') ).getroot()
        self._locate_sheets(root)
        self._get_styles(root)

# Locating all the sheets is a matter of doing an XPath search for
# :samp:`workspace-array/workspace` and getting the ``workspace-name`` attribute
# from the  :samp:`<table name="{name}">` tags.
#
# Within each workspace we have to find :samp:`page-info/tabular-info/tabular-model` to 
# get the tables within the workspaces.
#
# ::

    def _locate_sheets( self, root ):
        """Create ``workspace_table`` map from name to workspace and table."""
        self.workspace= dict()

        ws_name_attr= dom.QName( self.NUMBERS_NS["ls"], 'workspace-name' )
        name_attr= dom.QName( self.NUMBERS_NS["sf"], 'name' )
        workspace_array= root.find("ls:workspace-array", namespaces=self.NUMBERS_NS )
        for workspace in workspace_array.findall('.//ls:workspace', namespaces=self.NUMBERS_NS ):
            # Populate tables within this workspace.
            tables= dict()
            page_info = workspace.find('ls:page-info', namespaces=self.NUMBERS_NS)
            for tabular_info in page_info.findall('.//sf:tabular-info', namespaces=self.NUMBERS_NS):
                tabular_model = tabular_info.find( 'sf:tabular-model', namespaces=self.NUMBERS_NS)
                tables[ tabular_model.get(name_attr) ] = tabular_model
            self.workspace[ workspace.get(ws_name_attr) ]= workspace, tables

# Locate a "data source" within the XML document. Create ``Cell`` instances.
#
# ::

    def _datasource( self, grid ):
        """The data source for cell values within a grid.
        This yields each individual cell value, transformed into
        string, Decimal, datetime.
        """            
        datasource = grid.find('.//sf:datasource', namespaces=self.NUMBERS_NS)
        for cell_doc in datasource:
            yield self.cell( cell_doc )
        # or return map( self.cell, datasource )

# Create a ``Cell`` instance from the decoded data.
#
# ::

    def cell( self, cell ):
        logging.debug( dom.tostring(cell) )

        date_tag= dom.QName( self.NUMBERS_NS["sf"], 'd' )
        date_attr= dom.QName( self.NUMBERS_NS["sf"], 'cell-date' )
        formula_tag= dom.QName( self.NUMBERS_NS["sf"], 'f' )
        s_attr= dom.QName( self.NUMBERS_NS["sf"], 's' )
        v_attr= dom.QName( self.NUMBERS_NS["sf"], 'v' )
        general_tag= dom.QName( self.NUMBERS_NS["sf"], 'g' )
        number_tag= dom.QName( self.NUMBERS_NS["sf"], 'n' )
        text_tag= dom.QName( self.NUMBERS_NS["sf"], 't' )
        o_tag= dom.QName( self.NUMBERS_NS["sf"], 'o' )
        span_tag= dom.QName( self.NUMBERS_NS["sf"], 's' )
        bool_tag= dom.QName( self.NUMBERS_NS["sf"], 'b' )
        popup_menu_tag= dom.QName( self.NUMBERS_NS["sf"], 'pm' )
        IDREF_attr= dom.QName( self.NUMBERS_NS["sfa"], 'IDREF' )
        ID_attr= dom.QName( self.NUMBERS_NS["sfa"], 'ID' )
        fs_attr= dom.QName( self.NUMBERS_NS["sf"],"fs")

        if cell.tag == date_tag: 
            seconds= int(cell.attrib[date_attr])
            epoch= datetime.datetime(2001, 1, 1)
            delta= datetime.timedelta( seconds=seconds )
            theDate= epoch + delta
            return stingray.cell.FloatDateCell(theDate, self)
            
        elif cell.tag == formula_tag: # formula or error.
            s= cell.get(s_attr)
            fo= cell.find('sf:fo', namespaces=self.NUMBERS_NS)
            # Numeric Result? What about non-numeric results?
            r= cell.find('sf:r', namespaces=self.NUMBERS_NS)
            if r:
                # Result:
                rn= r.find('sf:rn', namespaces=self.NUMBERS_NS)
                try:
                    value_txt= rn.attrib[v_attr]
                    value= self._to_decimal( value_txt, s )
                except KeyError as ex:
                    #self._cell_warning("Formula with no value", cell)
                    value= self._to_decimal( '0', s )
                return stingray.cell.NumberCell(value, self)
            else:
                # Error: 
                #self._cell_warning("Formula error", cell)
                value= "#Error in {0}".format(fo.get(fs_attr))
                return stingray.cell.ErrorCell(value, self)
                
        elif cell.tag == general_tag: # General?
            return stingray.cell.EmptyCell('', self)
        elif cell.tag == number_tag: # number
            value= self._decode_number( cell )
            return stingray.cell.NumberCell(value, self)
        elif cell.tag == o_tag: #??
            self._cell_warning("Unknown cell type", cell)
            return stingray.cell.EmptyCell('', self)
        elif cell.tag == span_tag: #span?
            self._cell_warning("Unknown cell type", cell)
            return stingray.cell.EmptyCell('', self)
        elif cell.tag == text_tag: # text
            value= self._decode_text( cell )
            return stingray.cell.TextCell(value, self)
        elif cell.tag == bool_tag: # boolean
            value= self._decode_number( cell )
            return stingray.cell.BooleanCell(value, self)
        elif cell.tag == popup_menu_tag: # popup menu
            # TODO:: Better Xpath query: ``menu-choices/*[@ID='name']``
            value= None # In case we can't find anything.
            selected= cell.find('sf:proxied-cell-ref', namespaces=self.NUMBERS_NS)
            name= selected.get(IDREF_attr)
            mc= cell.find('sf:menu-choices', namespaces=self.NUMBERS_NS)
            for t in mc:
                if t.get(ID_attr) == name:
                    # t's tag cold end in Could be "t", or "n".
                    if t.tag.endswith('t'): # Text
                        value= self._decode_text( t )
                        return stingray.cell.TextCell(value, self)
                    elif t.tag.endswith('n'): # Number
                        value= self._decode_number( t )
                        return stingray.cell.NumberCell(value, self)
                    else:
                        raise Exception( "Unknown popup menu {0}".format(dom.tostring(cell)))
        else:
            raise Exception( "Unknown cell {0}".format( dom.tostring(cell) ) )

# Some lower-level conversions. 
#
# ::

    def _to_decimal( self, value_txt, style_id ):
        """Convert a given numeric value_text using the named style.

        TODO: From the style, get the number of decimal places, use that to
        build a string version of the float value.
        """
        fdp_attr= dom.QName( self.NUMBERS_NS["sf"], 'format-decimal-places' )
        fs_attr= dom.QName( self.NUMBERS_NS["sf"], 'format-string' )
        cell_style= self.cell_style.get(style_id)
        #print( "TO_DECIMAL", value_txt, style_id, "=", cell_style )

        fs= None # cell_style.get(fs_attr) # Doesn't seem correct
        fdp= None # cell_style.get(fdp_attr) # Doesn't seem correct
        # Transform fs into proper Python format, otherwise, use the number of 
        # decimal places.
        if fs is not None:
            fmt= self._rewrite_fmt( fs )
            #print( "Decimal: {{0:{0}}}.format({1}) = ".format( fmt, value_txt ), end="" )
            value= decimal.Decimal( "{:{fmt}}".format(float(value_txt), fmt=fmt) )
            #print( value )
            return value
        elif fdp is not None:
            #fmt= "{{0:.{0}f}}".format(fdp)
            value= decimal.Decimal( "{:.{fdp}f}".format(float(value_txt), fdp=fdp) )
            #print( "Decimal: {0}.format({1}) = {2!r}".format( fmt, value_txt, value ) )
            return value
        else:
            value= decimal.Decimal( value_txt )
            #print( "Decimal: {0} = {1!r}".format( value_txt, value ) )
        return value

    def _decode_text( self, cell ):
        """Decode a <t> tag's value."""
        sfa_s_attr= dom.QName( self.NUMBERS_NS["sfa"], 's' )
        ct= cell.find( 'sf:ct', namespaces=self.NUMBERS_NS )
        value= ct.get(sfa_s_attr)
        if value is None:
            value= "\n".join( cell.itertext() )
        return value

    def _decode_number( self, cell ):
        """Decode a <n> tag's value, applying the style."""
        s_attr= dom.QName( self.NUMBERS_NS["sf"], 's' )
        v_attr= dom.QName( self.NUMBERS_NS["sf"], 'v' )
        s= cell.get(s_attr)
        cell_style= self.cell_style.get(s)
        try:
            value_txt= cell.attrib[v_attr]
            value= self._to_decimal( value_txt, s )
        except KeyError as ex:
            #self._cell_warning("Number with no value", cell)
            value= self._to_decimal( '0', s )
        return value


# The styles are also important because we can use them to parse the numbers more
# precisely.
#
# ::

    def _get_styles( self, root ):
        """Get the styles."""
        ID_attr= dom.QName( self.NUMBERS_NS["sfa"], 'ID' )
        ident_attr= dom.QName( self.NUMBERS_NS["sf"], 'ident' )
        parent_ident_attr= dom.QName( self.NUMBERS_NS["sf"], 'parent-ident' )

        self.cell_style= {}
        for cs in root.findall('.//sf:cell-style', namespaces=self.NUMBERS_NS):
            #print( "STYLE", dom.tostring(cs) )
            ID= cs.get(ID_attr)
            ident= cs.get(ident_attr)
            parent_ident= cs.get(parent_ident_attr)
            property_number_format= cs.find('.//sf:SFTCellStylePropertyNumberFormat', namespaces=self.NUMBERS_NS)
            if property_number_format is None:
                if parent_ident is not None:
                    self.cell_style[ID]= self.cell_style[parent_ident]
            else:
                number_format= property_number_format.find('sf:number-format', namespaces=self.NUMBERS_NS)
                if number_format is None:
                    if parent_ident is not None:
                        self.cell_style[ID]= self.cell_style[parent_ident]
                else:
                    self.cell_style[ID]= number_format.attrib
                    if ident is not None:
                        self.cell_style[ident]= number_format.attrib
                #print( ID, self.cell_style.get(ID,None) )
        
# Rewrite a number format from Numbers to Python
#
# ::

    def _rewrite_fmt( self, format_string ):
        """Parse the mini-language: '#,##0.###;-#,##0.###' is an example.
        This becomes "{:10,.3f}"
        """
        positive, _, negative = format_string.partition(";")
        fmt= negative or positive
        digits= len(fmt)
        comma= "," if "," in fmt else ""
        whole, _, frac= fmt.partition(".")
        precision= len(frac)
        return "{digits}{comma}.{precision}f".format(
            digits= digits, comma=comma, precision=precision )
                    
# The "sheets" are the ``[ (`` *workspace*\ `,` *table* ``), ... ]`` pairs.
#
# ::

    def sheets( self ):
        """Build "sheet" names from workspace/table"""
        sheet_list= []
        for w_name in self.workspace:
            ws, tables = self.workspace[w_name]
            for t_name in tables:
                sheet_list.append( (w_name, t_name) )
        return sheet_list

# Picking a sheet involves matching a two-part name: (workspace, table).
#
# ::

    def rows_of( self, sheet ):
        """Iterator over rows.

        Two parallel traversals:

        Internal iterator over grid/datasource/* has d, t, n, pm, g, o and s
            yields individual cell values.

        Iterator over grid/rows/grid-row may have ``nc``, number of columns in that row.
            Each grid-row fetches a number of cell values to assemble a row.
            Row's may be variable length (sigh) but padded to the number of columns
            specified in the grid.
        """
        #print( self.__class__.__qualname__, sheet, sheet.name )
        ws_name, t_name = sheet.name
        ws, tables= self.workspace[ws_name]
        tabular_model= tables[t_name]
        
        grid= tabular_model.find( 'sf:grid', namespaces=self.NUMBERS_NS )
        numrows_attr= dom.QName( self.NUMBERS_NS["sf"], 'numrows' )
        numcols_attr= dom.QName( self.NUMBERS_NS["sf"], 'numcols' )
        numrows = int(grid.attrib[numrows_attr])
        numcols = int(grid.attrib[numcols_attr])
        
        nc_attr= dom.QName( self.NUMBERS_NS["sf"], 'nc' )
        
        datasource= iter( self._datasource(grid) )
        
        rows = grid.find('sf:rows', namespaces=self.NUMBERS_NS)
        for n, r in enumerate(rows.findall( 'sf:grid-row', namespaces=self.NUMBERS_NS )):
            #print( "ROW", dom.tostring(r) )
            self.debug_row= n
            # Is this really relevant for Numbers '09?
            nc= int(r.get(nc_attr,numcols)) 
            try:
                row= [ next(datasource) for self.debug_col in range(nc) ]
            except StopIteration as e:
                pass # Last row will exhaust the datasource.
            if len(row) == numcols:
                yield row
            else:
                yield row + (numcols-nc)*[None]
           
# Numbers 13 Workbook
# ----------------------
#
# ..  todo:: Implement Numbers13_Workbook
#
#     Use snappy and protobuf to read the IWA members of the archive.
#
# The iWork 13 format is a directory with an ``index.zip`` file. The ZIP contains
# a number of ``.IWA`` files. Each ``.IWA`` is compressed using the Snappy protocol.
# The uncompressed data is messages in Protobuf format.
#
# We could depend on proper Snappy and Protobuf implementations. We provide
# our own fall-back implementation in case there's nothing better available.
#
# We should import other implementations first, and then fall back to our own implementation.
# Instead, we'll simply import a local snappy and protobuf reader.
#
#
#
# ..  py:class:: Numbers13_Workbook
#
# ::

class Numbers13_Workbook( Workbook ):
    """Mac OS X Numbers Workbook for iWork '13.
    
    The ``.numbers`` "file" is a directory bundle or package.
    The ``index.zip`` file is the interesting part of the bundle.
    """
    def __init__( self, name, file_object=None ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object. Ignored for iWork13 files.
            Although, we might be able to use the internal handle to open 
            it as a proper directory.
        """
        super().__init__( name, file_object )
        self.numbers_package= os.path.join( name, "index.zip" )
        with zipfile.ZipFile( self.numbers_package ) as index:
            for n in index.namelist():
                log.info( n )
                with index.open( n ) as member:
                    pass    # snappy/protobuf reader.

# Workbook Factory
# -------------------
#
# ..  py:class:: No_Schema
#
# The :py:class:`No_Schema` exception is raised if there's a problem
# loading the schema.
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
                esl= stingray.schema.loader.ExternalSchemaLoader(schema_wb, schema_sheet)
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
