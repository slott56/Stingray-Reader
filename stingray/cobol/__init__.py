#!/usr/bin/env python3

# ..  _`cobol_init`: 
#
# #######################################################
# COBOL Package -- Extend Schema to Handle EBCDIC
# #######################################################
#
# The COBOL package is a (large) Python ``__init__.py`` module which
# includes much of the public API for working with COBOL files.
#
# This module extends Stingray in several directions.
#
# -   A new :py:class:`schema.Attribute` subclass, :py:class:`cobol.RepeatingAttribute`.
#
# -   A handy :py:func:`cobol.dump` function.
#
# -   The hierarchy of classes based on :py:class:`cobol.COBOL_File` which provide
#     more sophisticated COBOL-based workbooks.
#    
# Within the package we have the :py:mod:`cobol.loader` module which parses DDE's
# to create a schema. 
#
# Module Overheads
# =================
#
# ..  py:module:: cobol
#
# We depend on :py:mod:`cell`, :py:mod:`schema`, and :py:mod:`workbook`.
# We'll also import one class definition from :py:mod:`cobol.defs`.
#
# ::

"""stingray.cobol -- Extend the core Stingray definitions to handle COBOL
DDE's and COBOL files, including packed decimal and EBCDIC data.
"""
import codecs
import struct
import decimal
import copy
import warnings
import pprint
import logging

import stingray.schema
import stingray.sheet
from stingray.workbook.fixed import Fixed_Workbook


from stingray.cobol.defs import TextCell

# RepeatingAttribute Subclass of Attribute
# =========================================
#
# A new :py:class:`schema.Attribute` subclass is required to carry all the 
# additional attribute information developed during COBOL DDE parsing.  
#
# An attribute that has an occurs clause (or who's parent has an occurs clause)
# can accept an :py:meth:`cobol.RepeatingAttribute.index` method to provide index values used to compute
# effective offsets.
#
# ..  code-block:: none
#
#     http://yuml.me/diagram/scruffy;/class/
#     #cobol.attribute,
#     [Attribute]^[RepeatingAttribute],
#     [Schema]<>-[Attribute],
#     [Fixed_Workbook]-uses->[Attribute],
#     [Fixed_Workbook]^[COBOL_File],
#     [COBOL_File]-uses->[RepeatingAttribute].
#
# ..  image:: cobol_attribute.png
#
# In order to fetch data for ODO, the attribute offsets and sizes
# cannot **all** be computed in advance during parsing.
#
# They must be computed lazily during data fetching. See the :py:class:`ODO_LazyRow` 
# definition for how the attributes sizes and offsets are computed lazily 
# when there's an Occurs Depending On.
#
# Here are the attributes inherited from :py:class:`schema.Attribute`.
#
# :name: 
#     The attribute name. Typically always available for most kinds of schema.
#    
# :create: Cell class to create.  If omitted, the class-level
#     :py:data:`Attribute.default_cell` will be used.
#     By default, this refers to :py:class:`cell.TextCell`.
#    
# :position: 
#     Optional sequential position. This is set by the :py:class:`schema.Schema`
#     that contains this object.
#
# The additional values commonly provided by simple fixed format file schemata.
# These can't be treated as simple values, however, since they're
# clearly changed based on the ODO issues.
#   
# :size: 
#     Size within the buffer.
#
# These two properties can be tweaked by the :py:meth:`index` method. If left alone, they simply
# a delegation to the DDE. If :py:meth:`index` is used, these may be modified based on the index arguments.
#
# :dimensionality:
#     A tuple of DDE's that defines the dimensionality pushed down to this
#     item through the COBOL DDE hierarchy.
#
# :offset: 
#     Optional offset into a buffer. This may be statically defined,
#     or it may be dynamic because of variably-located data supporting
#     the Occurs Depends On.
#    
#     This meay be tweaked by the :py:meth:`index` method.
#     
# This subclass introduces yet more attribute-like properties that simply
# delegate to the DDE.
#
# :dde:
#     A weakref to a :py:class:`cobol.loader.DDE` object.
#
# :path:
#     The "."-separated path from top-level name to this element's name.
#    
# :usage:
#     The original DDE.usage object, an instance of :py:class:`cobol.defs.Usage`
#    
# :redefines:
#     The original DDE.allocation object, an instance of :py:class:`cobol.loader.Allocation`
#    
# :picture:
#     The original DDE.picture object, an instance of :py:class:`cobol.defs.Picture`
#    
# :size_scale_precision:
#     The original DDE.sizeScalePrecision object, a tuple with size, scale and precision derived
#     from the picture.
#
#
#
# ..  py:class:: RepeatingAttribute
#
# ::


class RepeatingAttribute( stingray.schema.Attribute ):
    """An attribute with dimensionality.  A potential "OCCURS" clause
    may define repeating values. A potential "OCCURS DEPENDING ON"
    clause may define variably located values. 
    """
    default_cell= TextCell
    def __init__(self, name, dde, offset=None, size=None, create=None, position=None, **kw):
        self.dde= dde
        self.name, self.size, self.create, self.position = name, size, create, position
        if not self.create:
            self.create= self.default_cell
        if offset is not None:
            warnings.warn( "Offset {0} is ignored; {1} used".format(offset, self.dde().offset), stacklevel=2 )
        self.__dict__.update( kw )
    def __repr__( self ):
        dim= ", ".join( map( repr, self.dimensionality ) )
        return "Attribute( name={0.name!r}, position={0.position}, offset={0.offset}, size={0.size}, dimensionality=({1}) )".format( 
            self, dim )

# ..  py:method:: RepeatingAttribute.index( *values )
#
# If the number of index values matches the dimensionality, we'll return a tweaked
# attribute which has just the offset required and a dimensionality of ``tuple()``.
#
# If the number of index values is insufficient, we'll return a tweaked attribute
# with which has the starting offset and the dimensions left otherwise unspecified.
#
# If the number of index values is excessive, we'll attempt to pop from an empty
# list.
#
# Note that py:meth:`index` is applied incrementally when the application supplies some
# of the indices.
#
# -   First, the application supplies some of the indices, creating
#     a tweaked :py:class:`RepeatingAttribute` with an initial offset.
#
# -   Second, the :py:class:`COBOL_File` supplies the remaining indices,
#     creating yet more temporary :py:class:`RepeatingAttribute` based on the initial offset.
# 
# ::    

    def index( self, *values ):
        """"Apply possibly incomplete index values to an attribute.
        We do this by cloning this attribute and setting a modified 
        dimensionality and offset.
        
        :param values: 0-based index values.  Yes, legacy COBOL language is 1-based.
            For Python applications, zero-based makes more sense.
        :returns: A clone of this attribute with modified offset
        and dimensionality that can be used with :py:meth:`COBOL_File.row_get`.
        """
        assert values, "Missing index values"
        # Previosly tweaked Attribute? Or originals?
        offset= self.offset
        dim_list= list(self.dimensionality)
        # Apply given index values.
        val_list= list(values)
        while val_list:
            index= val_list.pop(0)
            dim= dim_list.pop(0)
            offset += dim.size * index
        # Build resulting clone version with indexes applied.
        cloned= copy.copy( self )
        cloned._offset= offset
        cloned._dimensionality= dim_list # any left-over dimensions.
        return cloned

# With this, a ``row.cell(schema.get('name').index(i))`` will compute a proper offset.
#
# We clone the attribute to assure that each time we apply (or don't apply)
# the index, nothing stateful will have happened to the original attribute.
#
# Note that an incomplete set of index values forces the underlying 
# workbook to create a Python tuple (or tuple of tuples) structure to
# contain all the requested values.
#
# The additional properties which are simply shortcuts so that a 
# generic :py:class:`cobol.RepeatingAttribute` has access to the DDE details.
#
# ::

    @property
    def dimensionality(self):
        """tuple of DDE's"""
        try:
            # Tweaked by ``attribute.index()``
            return self._dimensionality
        except AttributeError:
            return self.dde().dimensionality
    @property
    def offset(self):
        """Influenced by index as well as occurs depending on"""
        try:
            # Tweaked by ``attribute.index()``
            return self._offset
        except AttributeError as e:
            return self.dde().offset
    @property
    def path(self):
        return self.dde().pathTo()
    @property
    def usage(self):
        return self.dde().usage
    @property
    def redefines(self):
        return self.dde().allocation
    @property
    def picture(self):
        return self.dde().picture
    @property
    def size_scale_precision(self):
        return self.dde().sizeScalePrecision

# COBOL LazyRow
# ==============
#
# The :py:class:`sheet.LazyRow` class is blissfully unaware of the need to compute
# sizes and offsets for COBOL.
#
#
# ..  py:class:: ODO_LazyRow
#
# This subclass of :py:class:`sheet.LazyRow` to provide add the feature to recompute sizes
# and offsets in the case of a variable-located DDE due to an Occurs Depending On.
#
# ::

class ODO_LazyRow( stingray.sheet.LazyRow ):
    """If the DDE is variably-located, tweak the sizes and offsets."""
    
    def __init__( self, sheet, **state ):
        """Build the row from the bytes.
        
        :param sheet: the containing sheet.
        :param **state: worksheet-specific state value to save.
        """
        super().__init__( sheet, **state )
        for dde in self.sheet.schema.info.get('dde',[]):
            if dde.variably_located:
                dde.setSizeAndOffset(self) 
            self._size= dde.totalSize
        else:
            self._size= len(self._state['data'])


# Dump a Record
# ===============
#
# ..  py:function:: dump_iter
#
# To support dumping raw data from a record, this will iterate through all items
# in an original DDE. It will a five-tuple with (dde, attribute, indices, bytes, Cell)
# for each DDE.
#
# If the DDE does not have an OCCURS clause, the indices will be an empty tuple.
# Otherwise, each individual combination will be yielded. For big, nested tables, this
# may turn out to be a lot of combinations.
#
# The bytes is the raw bytes for non-FILLER and non-group elements. 
#
# The Cell will be a Cell object, either with valid data or an :py:class:`cobol.defs.ErrorCell`.
#
#    
# ::

def dump_iter( aDDE, aRow ):
    """Yields iterator over tuples of (dde, attribute, indices, bytes, Cell)"""
    def expand_dims( dimensionality, partial=() ):
        if not dimensionality: 
            yield partial
            return
        top = dimensionality[0]
        rest= dimensionality[1:]
        for i in range(top):
            for e in expand_dims( rest, partial+(i,) ):
                yield e
    attr= aDDE.attribute() # Final size and offset details
    if aDDE.dimensionality: 
        for indices in expand_dims( aDDE.dimensionality ):
            yield aDDE, aDDE.attribute, indices, aRow.cell(attr,indices).raw, aRow.cell(attr,indices)
    elif aDDE.picture and aDDE.name != "FILLER":
        yield aDDE, aDDE.attribute(), (), aRow.cell(attr).raw, aRow.cell(attr)
    else: # FILLER or group level without a picture: no data is available
        yield aDDE, aDDE.attribute, (), None, None
    for child in aDDE.children:
        #pprint.pprint( child )
        for details in dump_iter( child, aRow ):
            yield details

# ..  py:function:: dump
#
# Dump data from a record, driven by the original DDE structure.
#
# ::

def dump( schema, aRow ):
    print( "{:45s} {:3s} {:3s} {!s} {!s}".format("Field", "Pos", "Sz", "Raw", "Cell" ) )
    for record in schema.info['dde']:
        for aDDE, attr, indices, raw_bytes, cell in dump_iter(record, aRow):
            print( "{:45s} {:3d} {:3d} {!r} {!s}".format(
                aDDE.indent*'  '+str(aDDE), aDDE.offset, aDDE.size, 
                raw_bytes, cell) )
    

# COBOL "Workbook" Files
# ========================
#
# A COBOL file is -- in effect -- a single-sheet workbook with an external schema.
# It looks, then, a lot like :py:class:`workbook.Fixed_Workbook`.  
#
# -   A pure character file, encoded UNICODE characters in some standard encoding
#     like UTF-8 or UTF-16.  This cannot include COMP or COMP-3 fields because
#     the codec would make a mess of the bit patterns.
#
# -   An EBCDIC-encoded byte file.  This can include COMP or COMP-3 fields.
#
# -   An ASCII-encoded byte file.  This can include COMP or COMP-3 fields.  
#     While this may exist, it seems to be very rare. We don't implement it.
#
# Note that each cell creation involves two features. This leads to a kind of **Double Dispatch** algorithm.  
#
# -   The cell type.  :py:class:`cobol.defs.TextCell`, 
#     :py:class:`cobol.defs.NumberDisplayCell`, 
#     :py:class:`cobol.defs.NumberComp3Cell` or :py:class:`cobol.defs.NumberCompCell`.
#
# -   The workbook encoding type.  Character or EBCDIC (or ASCII).
#
# The issue here is we're stuck with a complex "double-dispatch" problem.
# Each workbook subclass needs to implement methods for ``get_text``, ``number_display``,
# ``number_comp`` and ``number_comp3``.  
#
# The conversions, while tied to the workbook encoding, aren't properly tied to
# stateful sheet and row processing in the workbook.  They're just bound to the 
# encoding.  Consequently, we can make them static methods, possibly even 
# making this a mixin strategy.
#
# The use case looks like this.
#
# 1.  The application uses ``row.cell( schema[n] )``.  
#     The ``cell()`` method is simply ``sheet.workbook.row_get( buffer, attribute )``.  
#     It applies the cell type (via the schema item's attribute) and the raw data in the row's buffer.
#
# 2.  ``row_get( buffer, attribute )`` has to do the following.
#
#     -   Convert the buffer into a proper value based on the ``attribute`` type
#         information **and** the worksheet-specific methods for unpacking the 
#         various types of data.  The various :py:mod:`cobol` Cell subclasses
#         can refer to the proper conversion methods.
#    
#     -   Create the required :py:class:`cell.Cell` based on the ``attribute.create(sheet, value)`` function.
#
# ..  code-block:: none
#
#     http://yuml.me/diagram/scruffy;/class/
#     #cobol,
#     [Fixed_Workbook]^[COBOL_File],
#     [COBOL_File]^[Character_File],
#     [COBOL_File]^[EBCDIC_File].
#
# ..  image:: cobol_file.png
#     :width: 6in
#    
# COBOL File
# --------------
#
# ..  py:class:: COBOL_File
#
# This class introduces the expanded version of ``row_get`` that honors
# a schema attribute with dimensionality.
#
# ::

class COBOL_File( Fixed_Workbook ):
    """A COBOL "workbook" file which uses :py:class:`RepeatingAttribute` and
    creates COBOL Cell values.  This is an abstraction which
    lacks specific decoding methods.
    
    This is a :py:class:`Fixed_Workbook`: a file with fixed-sized, no-punctuation fields.
    A schema is required to parse the attributes.
    
    The rows are defined as :py:class:`ODO_LazyRow` instances so that
    bad data can be gracefully skipped over and Occurs Depending On offsets
    can be properly calculated.
    """
    row_class= ODO_LazyRow

# ..  py:method:: COBOL_File.row_get_index( row, attr, *index )
#
# Returning a particular Cell from a row, however, is more interesting for COBOL
# because the Attribute may contains an "OCCURS" clause.  In which case, we may need
# to assemble a tuple of values.
#
# If there is dimensionality, then take the top-level dimension (``dim[0]``) and
# use it as an iterator to fetch data based on the rest of the dimensions (``dim[1:]``).
#
# This can assemble a recursive tuple-of-tuples if there are multiple levels
# of dimensionality. 
#
# If too few index values are provided, a tuple of results is built around the missing values.
#
# If enough values are provided, a single result object will be built.
#
# ::

    def row_get_index( self, row, attr, *index ):
        """Emit a nested-tuple structure of Cell values using the given index values.
        :param row: the source Row.
        :param attr: the :py:class:`RepeatingAttribute`; possibly tweaked to 
            have an offset and partial dimensions. Or possibly the original tuple
            of dimensions.
        :param index: optional tuple of index values to use.
            Instead of ``row_get( schema.get('name').index(i) )``
            we can use ``row_get_index( schema.get('name'), i )``
        :returns: a (possibly nested) tuple of Cell values matching the dims that lacked
            index values.
        """
        if attr.dimensionality and index:
            # ``attr.index()`` probably not previously used.
            # Apply all remaining values and get the resulting item.
            final= attr.index( *index )
            return self.row_get( row, final )
        elif attr.dimensionality:
            # ``attr.index()`` previously used with partial arg values.
            # Build composite result.
            d= attr.dimensionality[0].occurs.number(row)
            result= []
            for i in range(d):
                sub= attr.index(i)
                result.append( self.row_get( row, sub ) )
            return tuple(result)
        else:
            # Doesn't belong here, delegate.
            return self.row_get( row, attr ) 
            
# ..  py:method:: COBOL_File.row_get( row, attr )
#
# The API method will get data from a row described by an attribute.
# If the attribute has dimensions, then indices are used or multiple values are returned
# by :py:meth:`COBOL_File.row_get_index`.
#
# If the attribute is has no dimensions, then it's simply pulled from the source row.
#
# There's a subtlety here: what if the row isn't big enough?
#
# :: 

    def row_get( self, row, attr ):
        """Create a Cell(s) from the row's data.
        :param row: The current Row
        :param attr: The desired Attribute; possibly tweaked to 
            have an offset and partial dimensions. Or possibly the original.
        :returns: A single Cell or a nested tuple of Cells if indexes
            were not provided.
        """ 
        if attr.dimensionality:
            return self.row_get_index( row, attr )
        else:
            extract= row._state['data'][attr.offset:attr.offset+attr.size]
            return attr.create( extract.rstrip(), self, attr=attr ) 

# Note that this depends on the superclass, which depends ordinary Unicode/ASCII line breaks.
# This will not work for EBCDIC files, which may lack appropriate line break characters.
# For that, we'll need to use specific physical format parsing helpers based on the 
# Z/OS RECFM parameter used to define the file.
#
# Character File
# -----------------
#
# This is subclass of :py:class:`COBOL_File` that handles COBOL data parsing
# where the underlying file is text. Since the file is text, Python handles
# any OS-level bytes-to-text conversions.
#
# ..  py:class:: Character_File
#
# ::

class Character_File( COBOL_File ):
    """A COBOL "workbook" file with decoding functions for
    proper character data.
    """

# The following functions are used to do data conversions for COBOL Character files.  
# Text is easy, Python's ``io.open`` has already handled this.
#
# ::

    @staticmethod
    def text( buffer, attr ): 
        """Extract a text field's value."""
        return buffer 

# Numeric data with usage ``DISPLAY`` requires handling implicit decimal points.
#
# ::

    @staticmethod
    def number_display( buffer, attr ):
        """Extract a numeric field's value."""
        final, alpha, length, scale, precision, signed, dec_sign = attr.size_scale_precision
        try:
            if precision != 0:
                if dec_sign == '.' or precision == 0:
                    display= buffer
                    return decimal.Decimal( buffer )
                else: # dec_sign == "V" or None
                    # Insert the implied decimal point.
                    display= buffer[:-precision]+"."+buffer[-precision:] 
                    return decimal.Decimal( display )
            else: # precision == 0:
                display= buffer
                return decimal.Decimal( buffer )
        except Exception:
            Character_File.log.debug( "Can't process {0!r} from {1!r}".format(display,buffer) )
            raise

# COMP-3 in proper character files may not make any sense at all.  
# A codec would make a hash of the bit patterns required.  
#
# ::

    @staticmethod
    def number_comp3( buffer, attr ):
        """Decode comp-3, packed decimal values.
        
        Each byte is two decimal digits.
        
        Last byte has a digit plus sign information: 'd' is <0, 'f' is unsigned, and 'c' >=0.
        """
        final, alpha, length, scale, precision, signed, dec_sign = attr.size_scale_precision
        digits= []
        for n in buffer:
            digits.append( n//16 )
            digits.append( n%16 )
        #print( repr(buffer), "from", repr(display) )
        sign= "-" if digits[-1]==13 else " "
        text= "".join( map(str,digits[:-1]) )
        # Adjust to include Precision.
        if precision:
            display= sign + text[:-precision]+"."+text[-precision:]
        else:
            display= sign + text
        try:
            return decimal.Decimal( display )
        except Exception:
            Character_File.log.debug( "Can't process {0!r} from {1!r}".format(display,buffer) )
            raise

# COMP in proper character files may not make any sense, either. 
# A codec would make a hash of the bit patterns required.  
#
# ::

    @staticmethod
    def number_comp( buffer, attr ):
        """Decode comp, binary values."""
        final, alpha, length, scale, precision, signed, dec_sign = attr.size_scale_precision
        if length <= 4:
            sc, bytes = '>h', 2
        elif length <= 9:
            sc, bytes = '>i', 4
        else:
            sc, bytes = '>q', 8
        n= struct.unpack( sc, buffer )
        return decimal.Decimal( n[0] )
    
# Class-level logger
#
# ::

Character_File.log= logging.getLogger( Character_File.__qualname__ )

# EBCDIC File
# ---------------
#
# The EBCDIC files require specific physical "Record Format" (RECFM) assistance.
# These classes define a number of Z/OS RECFM conversion. We recognize four
# actual RECFM's plus an additional special case.
#
# -   F - Fixed.
#
# -   FB - Fixed Blocked.
#
# -   V - Variable, data must have the RDW word preserved.
#
# -   VB - Variable Blocked, data must have BDW and RDW words.
#
# -   N - Variable, but no BDW or RDW words. This involves some buffer management
#     magic to recover the records properly.
#
# Note: "IBM z/Architecture mainframes are all big-endian".
#
# ..  py:class:: RECFM_Parser
#
# This class hierarchy breaks up EBCDIC files into records. 
#
#
# ::

class RECFM_Parser:
    """Parse a physical file format."""
    def record_iter( self ):
        """Return each physical record."""
        raise NotImplementedError
    def used( self, bytes ):
        """The number of bytes actually consumed.
        Only really relevant for RECFM_N subclass to handle variable-length
        records with no RDW/BDW overheads.
        """
        pass

# ..  py:class:: RECFM_F
#
# Simple fixed-length records. No header words.
#
# ::

class RECFM_F(RECFM_Parser):
    """Parse RECFM=F; the lrecl is the length of each record."""
    def __init__( self, source, lrecl=None ):
        """
        :param source: the file
        :param lrecl: the record length.
        """
        super().__init__()
        self.source= source
        self.lrecl= lrecl
    def record_iter( self ):
        data= self.source.read(self.lrecl)
        while len(data) != 0:
            yield data
            data= self.source.read(self.lrecl)

# ..  py:class:: RECFM_FB
#
# Simple fixed-blocked records. No header words.
#
# ::

class RECFM_FB( RECFM_F ):
    """Parse RECFM=FB; the lrecl is the length of each record.
    
    It's not clear that there's any difference between F and FB.
    """
    pass
    
# ..  py:class:: RECFM_V
#
# Variable-length records. Each record has an RDW header word with the length.
#
# ::

class RECFM_V(RECFM_Parser):
    """Parse RECFM=V; the lrecl is a maximum, which we ignore."""
    def __init__( self, source, lrecl=None ):
        """
        :param source: the file
        :param lrecl: a maximum, but it's ignored.
        """
        super().__init__()
        self.source= source
    def record_iter( self ):
        rdw= self.source.read(4)
        while len(rdw) != 0:
            size = struct.unpack( ">H2x", rdw )[0]
            data= self.source.read( size-4 )
            yield data
            rdw= self.source.read(4)
            
# We might want to implement the :py:meth:`RECFM_Parser.used` method to compare the number of bytes
# used against the RDW size.
#
# ..  py:class:: RECFM_VB
#
# Variable-length, blocked records. Each block has a BDW; each record has an RDW header word.
# These BDW and RDW describe the structure of the file.
#
# ::

class RECFM_VB(RECFM_Parser):
    """Parse RECFM=VB; the lrecl is a maximum, which we ignore."""
    def __init__( self, source, lrecl=None ):
        """
        :param source: the file
        :param lrecl: a maximum, but it's ignored.
        """
        super().__init__()
        self.source= source
    def record_iter( self ):
        bdw= self.source.read(4)
        while len(bdw) != 0:
            blksize = struct.unpack( ">H2x", bdw )[0]
            block_data= self.source.read( blksize-4 )
            offset= 0
            while offset != len(block_data): 
                assert offset+4 < len(block_data), "Corrupted Data Block {!r}".format(block_data)
                lrecl= struct.unpack( ">H2x", block_data[offset:offset+4] )[0]
                yield block_data[offset+4:offset+lrecl]
                offset += lrecl
            bdw= self.source.read(4)
            
# We might want to implement the :py:meth:`RECFM_Parser.used` method to compare the number of bytes
# used against the RDW size.
#
# ..  py:class:: RECFM_N
#
# Variable-length records without RDW's. Exasperating because we have to feed 
# bytes to the buffer as needed until the record is complete.
#
# ::

class RECFM_N:
    """Parse RECFM=V without RDW (or RECFM=VB without BDW or RDW).
    The lrecl is ignored.
    """
    def __init__( self, source, lrecl=None ):
        """
        :param source: the file
        :param lrecl: a maximum, but it's ignored.
        """
        super().__init__()
        self.source= source
        self.buffer= self.source.read( 32768 )
    def record_iter( self ):
        while len(self.buffer) != 0:
            yield self.buffer
            # What if used() is not called? This will loop forever!
    def used( self, bytes ):
        #print( "Consumed {0} Bytes".format(bytes) )
        self.buffer= self.buffer[bytes:]+self.source.read(32768-bytes)

# ..  py:class:: EBCDIC_File
#
# This subclass handles EBCDIC conversion and COMP-3
# packed decimal numbers.  For this to work, the schema needs to use slightly different Cell-type conversions.  
#
# Otherwise, this is similar to processing simple character data.
#
#
# ::

class EBCDIC_File( Character_File ):
    """A COBOL "workbook" file with decoding functions for
    EBCDIC data. If a file_object is provided, it must be 
    opened in byte mode, and no decoder can be used.
    """
    decoder= codecs.getdecoder('cp037')
    def __init__( self, name, file_object=None, schema=None, RECFM="N" ):
        """Prepare the workbook for reading.
        :param name: File name
        :param file_object: Optional file-like object.  If omitted, the named file is opened.
            The object must be opened in byte mode; no decoder should be used.
        :param schema: The schema to use.
        :param RECFM: The legacy Z/OS RECFM to use. This must be one
            of "F", "FB", "V", "VB". This is translated to an appropriate
            RECFM class: RECFM_F, RECFM_FB, RECFM_V, or RECFM_VB.
        """
        super().__init__( name, file_object, schema )
        if self.file_obj:
            self.the_file= None
            self.wb= self.file_obj
        else:
            self.the_file = open( name, 'rb' )
            self.wb= self.the_file
        self.schema= schema
        parser_class= {
            "F" : RECFM_F, 
            "FB": RECFM_FB, 
            "V" : RECFM_V,
            "VB": RECFM_VB,
            "N":  RECFM_N,
            }[RECFM]
        self.parser= parser_class(self.wb, schema.lrecl())

# ..  py:method:: EBCDIC_File.rows_of( sheet )
#
# We must extend the :py:meth:`workbook.Character_File.rows_of` method to deal with 
# two issues:
#
# -   If the schema depends on a variably located DDE, then we need to do the 
#     :py:func:`cobol.defs.setSizeAndOffset` function using the DDE.
#     This is done automagically by the :py:class:`ODO_LazyRow` object.
#    
# -   The legacy Z/OS RECFM details. 
#
#     *   We might have F or FB files, which are simply
#         long runs of EBCDIC bytes with no line breaks.
#         The LRECL must match the DDE.
#        
#     *   We might have V (or VB) which have 4-byte header on each row (plus a 4-byte header on each block.)
#         The LRECL doesn't matter.
#        
#     *   We can tolerate the awful situation where it's variable length (Occurs Depending On)
#         but there are no RECFM=V or RECFM=VB header words. We call this RECFM=N.
#         We fetch an oversized buffer and push back bytes beyond the end of the record.
#    
#     This means that the ``super().rows_of( sheet )`` has been replaced with a RECFM-aware
#     byte-parser. This byte parser may involve a back-and-forth to handle RECFM=N.
#     In the case of RECFM=N, we provide an overly-large buffer (32768 bytes) and after
#     any size and offset calculations, the ``row._size`` shows how many bytes were
#     actually used.
#
# ::

    def rows_of( self, sheet ):
        """Iterate through all "rows" of this "sheet". 
        Really, this means all records of this COBOL file.
        
        Note the handshake with RECFM parser to show how many
        bytes were really needed.  For RECFM_N, this is important.
        For other RECFM, this is ignored.
        
        :py:class:`ODO_LazyRow` may adjust the schema 
        if it has an Occurs Depending On.
        """
        for data in self.parser.record_iter():
            row= ODO_LazyRow( sheet, data=data )
            self.parser.used(sheet.schema.lrecl())
            yield row

# The following functions are used to do data conversions for COBOL EBCDIC files.  
# Text requires using a codec to translate EBCDIC-encoded characters.
#
# ::

    @staticmethod
    def text( buffer, attr ): 
        """Extract a text field's value."""
        text, size = EBCDIC_File.decoder(buffer)
        return text

# ::

    @staticmethod
    def number_display( buffer, attr ):
        """Extract a numeric field's value."""
        text, size = EBCDIC_File.decoder(buffer)
        return Character_File.number_display( text, attr )        

# ASCII File
# ------------------
#
# We could define a subclass for files encoded in ASCII which contain COMP and COMP-3 values.
#
# This is left as a future extension.   
