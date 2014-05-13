#!/usr/bin/env python3

# .. _`workbook_fixed`:
#
#
# Fixed-Format Workbook
# -----------------------
#
# ::

import logging
import pprint

from stingray.workbook.base import Workbook
import stingray.sheet
import stingray.cell

# ..  py:module:: workbook
#
# ..  py:class:: Fixed_Workbook
#
# Like a CSV workbook, this is a kind of degenerate case.  We don't have
# a lot of sheets, or a lot of data types.
#
# A subclass might do EBCDIC conversion and possibly even decode
# packed decimal numbers.  To do this, a COBOL-language DDE would be
# required as the schema definition. See :ref:`cobol`.
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
        return stingray.sheet.ExternalSchemaSheet( self, sheet_name, schema=self.schema )

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
    
