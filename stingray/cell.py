#!/usr/bin/env python3

# .. _`cells`:
#
# ########################################################
# Cell Module -- Data Element Containers and Conversions
# ########################################################
#
# ..  py:module:: cell
#
# The point of a :py:class:`cell.Cell` is two-fold.
#
# -   **Capture**.  That is, decode the information in the source file into
#     a Python object that represents the source spreadsheet value.
#     For XLS or XLSX formats, there are a variety of cell data types.
#     For CSV, all cells are text.
#     For a fixed format file, we may have to exploit the physical format information
#     to properly decode the bytes or characters.  We may even have to
#     cope with EBCDIC or packed decimal conversions.
#
# -   **Convert**. Provide the cell value coerced to another type.
#
# Capture Use Case
# ====================
#
# The **Capture** use case is defined by our physical formats.
# ``xlrd`` identifies the follow cell types found in XLS workbooks.
#
# ..  csv-table::
#     :header: "Type symbol","Type number","Python value"
#     :widths: 33, 5, 55
#
#     XL_CELL_EMPTY,0,"empty string ''"
#     XL_CELL_TEXT,1,"a Unicode string"
#     XL_CELL_NUMBER,2,"float"
#     XL_CELL_DATE,3,"float"
#     XL_CELL_BOOLEAN,4,"int; 1 means TRUE, 0 means FALSE"
#     XL_CELL_ERROR,5,"int representing internal Excel codes; for a text representation, refer to the supplied dictionary error_text_from_code"
#     XL_CELL_BLANK,6,"empty string ''. Note: this type will appear only when open_workbook(..., formatting_info=True) is used."
#
# An XLSX (per ECMA 376, section 18.18.11) or ODS provides a similar list of cell types.  The data is always encoded as a proper string that can be converted (if necessary) based on the type code.
#
# ..  csv-table::
#     :header: "Enumeration Value","Description"
#     :widths: 22, 55
#
#     ``b`` (Boolean),Cell containing a boolean.
#     ``d`` (Date),Cell contains a date in the ISO 8601 format.
#     ``e`` (Error),Cell containing an error.
#     ``inlineStr`` (Inline String),"Cell containing an (inline) rich string, i.e., one not in the shared string table. If this cell type is used, then the cell value is in the is element rather than the v element in the cell (c element)."
#     ``n`` (Number),Cell containing a number.
#     ``s`` (Shared String),Cell containing a shared string.
#     ``str`` (String),Cell containing a formula string.
#
# Dates formatted as strings are -- always -- a problem. There's no generic solution.  An application may need to write suitable extensions to handle this.
# See below under `Conversion Functions`_.
#
# We should depend on the :py:mod:`locale` module to provide proper format strings for converting between date and string.
#
# A Numbers spreadsheet appears to have the following cell types. Some of the
# tags appear to have no value, so their purpose is unclear.
#
# ..  csv-table::
#     :header: "Tag","Description"
#     :widths: 22, 55
#
#     ``d`` (Date),Cell containing a date
#     ``f`` (Formula),Cell containing a formula
#     ``g`` (Empty),Two or more empty cells
#     ``n`` (Number),Cell containing a number
#     ``o`` (?),One empty cell
#     ``s`` (?),
#     ``t`` (Text),Cell containing text
#     ``pm`` (Popup Menu),A popup menu of otherc cell values
#
# Convert Use Cases
# =======================
#
# There are several use cases for output conversion (or "transformation").
#
# -   Trivial.  ``float``, ``str`` or an empty cell.  Essentially,
#     we're using the captured data type directly.
#
# -   Easy.  ``float`` or ``str`` to ``decimal``.  Generally,  currency
#     fields are stored as ``float`` in the workbook; this needs to be coverted to
#     ``decimal`` to be useful.
#
# -   Obscure.  ``datetime`` based on ``float``.  ``xlrd`` handles
#     this elegantly.
#
# -   Variable.  ``datetime`` based on ``str``.  The variability
#     becomes rather  complex.  It's also application-specific, since
#     it depends on the source of the data.
#
# -   Horrible.  Digit strings.  US Zip codes.  Social Security Numbers.
#     Phone numbers without punctuation.  These are digit strings which
#     a spreadsheet application may transform to a floating point number;
#     these need to be rebuilt as proper digit strings with leading zeroes.
#
# We'd like code that looks like these examples:
#
# ..  parsed-literal::
#
#     "{0} has {1}".format( foo.to_str(), foo.to_float() )
#
#     today= bar.to_datetime().date()
#
#     zip_code.to_digit_str(5)
#
# This **Convert** aspect of a :py:class:`cell.Cell` is part of *using* the logical layout.
# We'll address that under :ref:`developer`, below.
#
# Model
# ======
#
# ..  code-block:: none
#
#     http://yuml.me/diagram/scruffy;/class/
#     #cell,
#     [Cell]^[EmptyCell],
#     [Cell]^[TextCell],
#     [Cell]^[NumberCell],
#     [Cell]^[FloatDateCell],
#     [Cell]^[BooleanCell],
#     [Cell]^[ErrorCell].
#
# ..  image:: cell.png
#     :width: 6in
#
# Overheads
# ===========
#
# We required ``xlrd`` from http://www.lexicon.net/sjmachin/xlrd.htm 
#
# This is the easiest way to read legacy Microsoft ``.XLS`` files.
#
# ::

"""stingray.cell -- Defines Cell as the atomic data element in a sheet
of a workbook.

A cell has a value, it's part of a workbook.
"""

import locale
import decimal
import datetime
import time
from collections import Hashable
import xlrd

# Just to be sure that any locale-based processing will actually
# work, we establish a default locale.
#
# ::

locale.setlocale(locale.LC_ALL, '')

# Cell
# =======
#
# ..  py:class:: Cell
#
#     The :py:class:`cell.Cell` class hierarchy extends this base class.  Note that we have
#     a relatively short list of built-in conversions.  
#     For more complex, application-specific conversions, the raw :py:attr:`value` is available as a property.
#
# ::

class Cell( Hashable ):
    """A class hierarchy for each kind of Cell."""
    def __init__( self, value=None, workbook=None ):
        """Build a new Cell; the atomic data element of a  workbook.

        :param _value: the proper Python object, correctly converted from the source.
        :param workbook: the :py:class:`workbook.Workbook` that created this Cell.
            This is largely used for Excel date conversions, but there
            could be other context needs.
        """
        self._value, self.workbook = value, workbook
    def __repr__( self ):
        return "{0}({1!r})".format(
            self.__class__.__name__, self._value )
    def is_empty( self ):
        return self._value is None
    def to_int( self ): return NotImplemented
    def to_float( self ): return NotImplemented
    def to_decimal( self, digits=None ): return NotImplemented
    def to_str( self ): return NotImplemented
    def to_datetime( self, format=None ): return NotImplemented
    def to_digit_str( self, len=5 ): return NotImplemented

# One feature of a cell that's required when we do data profiling is to
# create a usable hash from the cell class and raw data value.
#
# ::

    def __hash__( self ):
        return hash(self._value) ^ hash(self.__class__)
    def __eq__( self, other ):
        return self.__class__ == other.__class__ and self._value == other._value
    def __ne__( self, other ):
        return self.__class__ != other.__class__ or self._value != other._value

# We make a token effort at making a cell more-or-less immutable.  This makes it
# hashable.
#
# ::

    @property
    def value( self ):
        return self._value

# ..  todo:: Test hashable interface of Cell
#
# EmptyCell
# ============
#
# ..  py:class:: EmptyCell
#
#     An ``EmptyCell`` implements empty cells.  :py:mod:`xlrd` may report them as a type ``XL_CELL_EMPTY``. 
#     A Numbers spreadsheet may use the ``<o>`` or ``<g>`` tag.
#
# ::

class EmptyCell( Cell ):
    """The *value* will be '', but we ignore that."""
    def is_empty( self ): return True
    def to_int( self ): return None
    def to_float( self ): return None
    def to_decimal( self, digits=None ): return None
    def to_str( self ): return None
    def to_datetime( self, format=None ): return None
    def to_digit_str( self, len=None ): return None

# TextCell
# ============
#
# ..  py:class:: TextCell
#
#     A ``TextCell`` implements the cells with text values.
#     :py:mod:`xlrd` may report them as a type ``XL_CELL_TEXT``.
#     It's often possible to interpret the text as some other value,
#     so the conversions make reasonable attempts at that.
#
#     This is used for CSV workbooks as well as XLS workbooks.
#     This is the default type for Fixed format files, also.
#
#     Note that COBOL files will explicitly have bytes values, not
#     string values.
#
# ::

class TextCell( Cell ):
    """A Cell which contains a Python string value."""
    def to_int( self ):
        return int( self.value )
    def to_float( self ):
        return float( self.value )
    def to_decimal( self, digits=0 ):
        return decimal.Decimal( self.value )
    def to_str( self ):
        return self.value
    def to_datetime( self, format=None ):
        if format is None:
            try:
                format = locale.nl_langinfo(locale.D_FMT)
            except AttributeError as e:
                # Windows
                format = "%x"
        return datetime.datetime.strptime(self.value,format)
    def to_digit_str( self, length=5 ):
        fmt= "{{0:0>{0}d}}".format(length)
        return fmt.format( int(self.value) )

# NumberCell
# ============
#
# ..  py:class:: NumberCell
#
#     A ``NumberCell`` implements the cells with a float value.
#     :py:mod:`xlrd` may report them as a type ``XL_CELL_NUMBER``.
#     A variety of conversions make sense for a number value.
#     Note that the ``to_datetime()`` conversion depends on ``xlrd``.
#     This may be a faulty assumption for some species of workbooks.
#
# ::

class NumberCell( Cell ):
    """A cell which contains a Python float value."""
    def to_int( self ):
        return int( self.value )
    def to_float( self ):
        if isinstance(self.value,float):
            return self.value
        # likely, it's Decimal!
        return float(self.value) 
    def to_decimal( self, digits=0 ):
        if isinstance(self.value,float):
            fmt= "{0:0.{digits}f}"
            return decimal.Decimal( fmt.format(self.value, digits=digits) )
        elif isinstance(self.value,decimal.Decimal):
            return self.value
        else:
            return decimal.Decimal(self.value)
    def to_str( self ):
        return str(self.value)
    def to_datetime( self, format=None ):
        assert format is None, "Format is not used."
        try:
            dt= xlrd.xldate_as_tuple(self.value, self.workbook.datemode)
        except xlrd.xldate.XLDateAmbiguous as e:
            ex= ValueError( "Ambiguous Date: {0}".format(self.value) )
            ex.__cause__= e
            raise ex
        return datetime.datetime(*dt)
    def to_digit_str( self, length=5 ):
        fmt= "{{0:0>{0}d}}".format(length)
        return fmt.format( int(self.value) )

# FloatDateCell
# ===============
#
# ..  py:class:: FloatDateCell
#
#     A ``FloatDateCell`` implements the cells with ``XL_CELL_DATE``.
#     Since the conversions are all identical to number,
#     we simply inherit the features of a number.
#
# ::

class FloatDateCell( NumberCell ):
    """A cell which contains a float value that is actually an Excel date."""
    pass

# Other formats have other kinds of date cells that aren't simply
# dressed-up floating-point numbers.
#
# BooleanCell
# ============
#
# ..  py:class:: BooleanCell
#
#     A ``BooleanCell`` implements the cells with ``XL_CELL_BOOLEAN``.
#     Since the conversions are all identical to number,
#     we simply inherit the features.
#
# ::

class BooleanCell( NumberCell ):
    """A cell which contains a boolean value."""
    pass

# ErrorCell
# ============
#
# ..  py:class:: ErrorCell
#
#     An ``ErrorCell`` implements the cells with ``XL_CELL_ERROR``.
#     The only sensible conversion is :py:meth:`ErrorCell.to_str` which reports
#     the error string for the cell.
#
# ::

class ErrorCell( Cell ):
    """A cell which contains an error code."""
    def to_int( self ):
        raise ValueError( self.value )
    def to_float( self ):
        raise ValueError( self.value )
    def to_decimal( self, digits=0 ):
        raise ValueError( self.value )
    def to_str( self ):
        return self.value
    def to_datetime( self, format=None ):
        raise ValueError( self.value )
    def to_digit_str( self, length=5 ):
        raise ValueError( self.value )

# DateCell
# ============
#
# ..  py:class:: DateCell
#
#     A ``DateCell`` implements a cell with a proper date-time value.
#     This is a value which did not come from a workbook float value.
#
#     This could be a parsed string, for example.
#
#     A variety of conversions make sense for a proper date value.
#
# ::

class DateCell( Cell ):
    """A cell which contains a proper :mod:`datetime` value."""
    def to_int( self ):
        return int(self.to_float())
    def to_float( self ):
        timetuple= self.value.timetuple()[:6]
        xl= xlrd.xldate.xldate_from_datetime_tuple(
            timetuple,
            self.workbook.datemode)
        return xl
    def to_decimal( self, digits=0 ):
        fmt= "{0:0.{digits}f}"
        return decimal.Decimal( fmt.format(self.to_float(),digits=digits) )
    def to_str( self ):
        return str(self.value)
    def to_datetime( self, format=None ):
        return self.value
    def to_digit_str( self, length=5 ):
        fmt= "{{0:0>{0}d}}".format(length)
        return fmt.format( self.to_int() )


# For numbers, the ``<d>`` cells have a native date format.
#
# This is an extension to basic :py:mod:`xlrd`, XLSX and ODS workbook processing
# because no cell has this as its data type.
#
# Conversion Functions
# =======================
#
# There is a need for functions for various kinds of conversions.  These are
# mostly focused on processing Fixed format files, where the data all originates
# as text.
#
# These can also be applied to CSV or TAB files to create cells other than the default
# :py:class:`cell.TextCell`.
#
# Dates
# --------
#
#
# ..  py:function:: date_from_string(format)
#
#     A closure based on a format string
#     that returns a single-argument conversion function.
#    
#     :param format: the format string to use.
#
#
# ::

def date_from_string( format ):
    def the_conversion( string ):
        return datetime.datetime.strptime( string, format )
    return the_conversion

# This forms the factory for the :py:class:`cell.DateCell` class.
#
# ..  py:function:: datecell_from_string(format)
#
#     A closure based on a format string
#     that returns a single-argument conversion function.
#    
#     :param format: the format string to use.
#
#
# ::

def datecell_from_string( format ):
    dt_conv= date_from_string( format )
    def the_conversion( string, workbook ):
        return DateCell( dt_conv( string ), workbook )
    return the_conversion

# This could be used like this in a schema definition.
#
# ..  parsed-literal::
#
#     d = Attribute( name="mm-dd-yy", size=*n*, offset=*m*,
#         create=stingray.cell.datecell_from_string("%m/%d/%y") )
#
# The function :py:func:`date_from_float` is an additional closure based on a ``workbook.datemode`` that returns a single-argument conversion function.
#
# ..  py:function:: date_from_float(datemode)
#
#     A closure based on a datemode setting
#     that returns a single-argument conversion function.
#    
#     :param datemode: the data mode for the workbook document as a whole.
#
#
# ::

def date_from_float( datemode ):
    def the_conversion( value ):
        try:
            dt= xlrd.xldate_as_tuple(value, datemode)
        except xlrd.xldate.XLDateAmbiguous as e:
            ex= ValueError( "Ambiguous Date: {0!r}".format(value) )
            ex.__cause__= e
            raise ex
        return datetime.datetime(*dt)
    return the_conversion

# This function has similar applications for converting data to a more useful
# :py:class:`cell.DateCell` instance.
#
# ..  parsed-literal::
#
#     float2date= stingray.cell.date_from_float(workbook.datemode)
#     d = Attribute( name="mm-dd-yy", size=*n*, offset=*m*,
#         create=lambda x, w: stingray.cell.DateCell( float2date(x), w ) )
#
# Numbers
# ----------
#
# A fixed-format file can have any of a variety of numbers, encoded in a
# variety of ways.
#
# The ordinary number-as-string, is trivially handled by :py:class:`cell.TextCell`.  No real need for a more sophisticated conversion.
#
# The less-ordinary case of COBOL data, in computational-3 format, or display
# format with an assumed decimal place is more difficult.  We'll defer
# this to :ref:`cobol`.
