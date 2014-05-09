#!/usr/bin/env python3

# .. _`workbook_base`:
#
# Workbook Base Definition
# ==========================
#
# ::

import logging
import os

import stingray.sheet

# ..  py:module:: workbook
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
