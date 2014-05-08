#!/usr/bin/env python3

# .. _`sheets`:
#
# ######################################
# Sheet Module -- Sheet and Row Access
# ######################################
#
# ..  py:module:: sheet
#
# A *Sheet* is a generator
# of *Row* objects.  A *Row* is a sequence of :py:class:`cell.Cell` instances, identified by position.
#
# We have three variations on :py:class:`sheet.Sheet`.
#
# -   A simple :py:class:`sheet.Sheet`  lacks a schema.
#     (This corresponds with :py:func:`csv.reader`.)
#     For workbooks with a well-known physical format, the schema can be optional.
#     Each :py:class:`sheet.Row` object can be built eagerly and accessed
#     by position.
#
# -   A sheet with a schema.  There are two variations.
#
#     -   :py:class:`sheet.EmbeddedSchemaSheet` contains a schema.
#         This could be a simple as column titles in the first row.
#         (This corresponds to :py:class:`csv.DictReader`.)
#         Or it could be considerably more complex.
#
#     -   :py:class:`sheet.ExternalSchemaSheet` requires an external schema.
#         This schema may be simply a list of column titles supplied externally.
#         More often, the schema is a complete physical format description for
#         Fixed or COBOL format files.
#
# A known physical format (like a workbook) can build :py:class:`sheet.Row` objects eagerly with or without a schema.
#
# In the case of COBOL and fixed-format files, however, a :py:class:`sheet.Row`
# cannot be built eagerly.  It must be a lazy
# object which only builds :py:class:`cell.Cell` as needed.
# See :ref:`cobol` for details.
#
# Get Embedded Schema Use Case
# ===============================
#
# For an :py:class:`sheet.EmbeddedSchemaSheet`, the application (or Workbook) must 
# do a three-step dance to get the schema that is embedded in the sheet.
#
# 1.  Build a :py:class:`sheet.EmbeddedSchemaSheet` with an
#     "embedded schema loader" class.  (For example, :py:class:`schema.loader.HeadingRowSchemaLoader`.)
#     The loader partitions rows into two sets: header and data.
#
# 2.  Load the schema from the sheet.
#     The :py:class:`sheet.Sheet` will build an object of the loader class and use it to 
#     gather the schema information.
#     The schema loading may involve skipping irrelevant rows or
#     combining multi-line headings or anything else required to parse the sheet.
#
# 3.  Get the rows from the sheet.
#     This will, also, invoke the attached loader to filter rows so that the header is not seen as data.
#
# ..  parsed-literal::
#
#     with *open* as wb:
#         sheet = EmbeddedSchemaSheet( workbook, 'Sheet1', HeadingRowSchemaLoader )
#         counts= process_sheet( sheet )
#         pprint.pprint( counts )
#
# Get External Schema Use Case
# ===============================
#
# For an :py:class:`sheet.ExternalSchemaSheet`, the application (or Workbook) 
# must do a four-step dance to get the schema.
#
# 1.  Build a schema loader.
#     This loader will require a source workbook, sheet name and a reader object.
#
# 2.  Get the Schema object from the loader.
#
# 3.  Build a :py:class:`sheet.ExternalSchemaSheet` with the Schema object.
#
# 4.  Get the rows from the sheet.
#
# And yes, the external source, is another
# spreadsheet!  Worse, the external source could be a fixed file or workbook
# for which a meta-schema is required to read the schema.
#
# ..  parsed-literal::
#
#     with *open schema* as swb:
#         esl = ExternalSchemaLoader( swb, sheet_name='Schema' )
#         schema = esl.load()
#     with *open data* as wb:
#         sheet = ExternalSchemaSheet( wb, 'Sheet1', schema )
#         counts= process_sheet( sheet )
#         pprint.pprint( counts )
#
# Get Rows Use Case
# ======================
#
# The essential job of a :py:class:`sheet.Sheet` is to produce :py:class:`sheet.Row` instances.  
# A row is a sequence of :py:class:`cell.Cell` instances.
#
# Note that :py:mod:`csv` is eager about building a row from the source data.
# This isn't universally appropriate.  COBOL files require lazy construction
# of the row's cells.
#
# A :py:class:`schema.Schema` can transform a sequence row into a dictionary row
# or a named tuple row.
# The :py:attr:`schema.Attribute.name` becomes the key for this row-as-dictionary.
#
# We specifically delegate the row-as-dictionary interpretation to the :py:class:`schema.Schema`,
# and avoid doing it in the :py:class:`sheet.Sheet`.  This is because most
# workbook schemata are flat.  However, a COBOL schema can have a very complex
# structure, making the row-as-dictionary too simplistic to be useful.
#
# As noted above, there are two candidate implementations of a Row.
#
# -   **Eager**.  Appropriate for most (but not all) Physical Formats.  The
#     idea is to apply the schema immediately to create the row as a
#     tuple of cells.  :mod:`csv` does this, and it can be applied to
#     other workbook formats.  It can be applied to simple, flat
#     Fixed format files.
#
# -   **Lazy**.  This is more appropriate for Fixed format files and COBOL format
#     files.  Specifically, the data conversion, redefines and repeating group
#     issues force us to wait for cell access rather than immediately create all
#     possible cells.  Indeed, for  COBOL files with REDEFINES definitions,
#     some of the cells cannot be built eagerly; application logic must determine
#     which attributes are valid or invalid.
#    
# Note that the API is the same. The implementation differs.
#
# Here's our prototypical code.
#
# ..  parsed-literal::
#
#     def process_sheet( sheet ):
#         counts= defaultdict( int )
#         for row in sheet.rows():
#             #\ *row is a sequence of* Cell *instances*
#             print( repr(c) for c in row )
#             counts['read'] += 1
#         return counts
#
# Ultimately, the sequence nature of a row is unsatisfying.   We'll have to
# wait until :ref:`schema` to extend this into something useful.
#
# Sheet Identification
# =====================
#
# For CSV and TAB files, as well as COBOL and Flat files, there is one anonymous
# "sheet" that is the entire workbook.
#
# For XLS, XLSX, and ODS formats, however, there are sheets within the workbook.
#
# For Numbers, there are "pages" or "workspaces" that have multiple tables. Each
# Numbers **table** is -- effectively -- a :py:class:`sheet.Sheet`. The
# intermediate organization level, "workspace", is an additional detail.
#
# We handle this in the following way.
#
# -   One anonymous sheet has a name either of ``None`` or the basename of the file.
#
# -   Simple sheets have names which are simple strings.
#
# -   Numbers workspaces with sheets have names which are two-tuples of 
#     workspace ("sheet") and table name.
#
# Model
# =======
#
# ..  code-block:: none
#
#     http://yuml.me/diagram/scruffy;dir:td/class/
#     #sheet,
#     [Workbook]<>-n[Sheet],
#     [Sheet]<>-n[Row],
#     [Row]^[LazyRow],
#     [LazyRow]-gets->[Workbook],
#     [Sheet]^[EmbeddedSchemaSheet],
#     [Sheet]^[ExternalSchemaSheet],
#     [EmbeddedSchemaSheet]->[SchemaLoader].
#
# ..  image:: sheet.png
#
# Overheads
# ==========
#
# Sheet and Row are essentially lazy sequences.
#
# ::

"""stingray.sheet -- Defines Row as  a collection of Cells and Sheet as a collection of Rows.
"""
from collections import Sequence

# There are two "implicit" dependencies, also.
# A row depends on details of an :py:class:`schema.Attribute` and a :py:class:`workbook.Workbook`.  
# However, there's no real need to present a formal import for this.  
# The Attribute and Workbook are simply opaque
# objects passed around as arguments.
#
# Sheet Class
# =============
#
# ..  py:class:: Sheet
#
# A :py:class:`sheet.Sheet` is an iterator over the rows of data in a workbook.
# Subclasses implement different bindings for the sheet's schema information.
#
# ::

class Sheet:
    """An iterator over rows.
        A binding to a workbook.
        A subclass of Sheet will be bound to a schema.
    """
    def __init__( self, workbook, sheet_name ):
        self.workbook, self.name= workbook, sheet_name
    def __repr__( self ):
        return "{0}({1!r},{2!r})".format( self.__class__.__qualname__,
            self.workbook, self.name )
    def rows( self ):
        """Iterate through the rows of this sheet.
        This is a convenient interface for ``self.workbook.rows_of(self)``
        """
        return self.workbook.rows_of( self )

# Row Class
# =============
#
# ..  py:class:: Row
#
# A single row in Sheet; a sequence of :py:class:`cell.Cell` instances.
#
# A Sheet produces this simple row-as-list.  A Schema can transform this
# into row-as-dict or some even more elaborate structure.
#
# A row depends on details of an :py:class:`schema.Attribute` 
# and a :py:class:`workbook.Workbook`.  
# This feels circular, but doesn't present any real problems.
#
# The :py:class:`cell.Cell` conversions are handled by the Workbook.
# Some Workbooks have cell content identified by position.
# Some Workbooks have cell content identified by size, offset and encoding.
# Therefore, we must provide the Attribute details to the Workbook
# to get the Cell's value.
#
# ::

class Row( Sequence ):
    """Eager Row: a tuple of Cell values."""
    def __init__( self, sheet, *data ):
        """Build another Row.

        :param sheet: the containing sheet.
        :param *data: the various Cell values in this row
        """
        self.sheet= sheet
        self.data= data
    def cell( self, attribute ):
        """Get a specific cell, based on a schema Attribute.

        :param attribute: The attribute's value to return.
        """
        return self.sheet.workbook.row_get( self, attribute )
    def __len__( self ):
        return len(self.data)
    def __iter__( self ):
        return iter(self.data)
    def __contains__( self, cell ):
        return any( cell.value == d.value for d in self.data )
    def __getitem__( self, index ):
        return self.data[index]

# To approach the :py:class:`csv.DictReader` API (without the eager processing),
# we need make the ``Row`` API slightly more fluent with a ``by_name()``
# method.
#
# ..  parsed-literal::
#
#         def by_name( self, name ):
#             attr= self.sheet.schema.get_name(name)
#             return self.cell( attr )
#
# Note that the presumption in this interface is that the Attribute is
# sufficiently detailed to specify a single :py:class:`cell.Cell`.
# For non-COBOL workbooks, this is perfectly true.
#
# For COBOL, however, there are groups and occurs clauses, meaning that a single Attribute can
# represent multiple :py:class:`cell.Cell` instances.  
# Which one do we mean?  And how do we specify this selection?
#
# -   The :py:meth:`sheet.Row.cell` method can return a structure with all the values. 
#     Ordinary Python can then pick apart the instances.
#     This requires working up the DDE hierarchy to locate all of the applicable
#     "occurs" by to construct the proper dimensionality of an attribute.
#
#     It also means getting all of the values to create a tuple or nested
#     tuple-of-tuple structure for the various dimensions.
#
# -   The :py:class:`schema.Attribute.index` method
#     selects data from the row in the workbook.  This applies the indices
#     to the Attribute to compute the required offset into the source data.
#
# ..  py:class:: LazyRow
#
# When we can't eagerly build all :py:class:`cell.Cell` instances for a given
# row, this class provides the proper API.
#
# A COBOL REDEFINES clause may make the bytes invalid in all but one of the
# aliases for an attribute.  Also, there's no formal ``NULL`` value in COBOL, so
# optional fields can have invalid data.
#
# Further, we may have Occurs Depending On. This means we can't set size and
# offset until we can access actual data.
#
# For these reasons, we have a :py:class:`sheet.LazyRow`, which conforms to the
# interface for a :py:class:`Row`, but isn't an actual sequence. No data is
# processed until the :py:meth:`LazyRow.__getitem__` method is used.
#
# ::

class LazyRow( Sequence ):
    """Lazy Row: a tuple-like sequence of Cell values."""
    def __init__( self, sheet, **state ):
        """Build another Row.

        :param sheet: the containing sheet.
        :param **state: worksheet-specific state value to save.
        """
        self.sheet= sheet
        self._state= state
        super().__init__()
    def __repr__( self ):
        return "LazyRow(sheet={0!r}, state={1!r})".format( self.sheet, self._state )
    def cell( self, attribute ):
        """Get a specific cell, based on a schema Attribute.

        :param attribute: The attribute's value to return.
        """
        return self.sheet.workbook.row_get( self, attribute )
    def __len__( self ):
        return len(self.sheet.schema)
    def __iter__( self ):
        for attribute in self.sheet.schema:
            try:
                yield self.sheet.workbook.row_get( self, attribute )
            except Exception as e:
                yield None
    def __contains__( self, cell ):
        for attribute in self.sheet.schema:
            try:
                col= self.sheet.workbook.row_get( self, attribute )
            except Exception as e:
                pass
            if col.value == cell.value:
                return True
    def __getitem__( self, index ):
        attribute= self.sheet.schema[index]
        return self.sheet.workbook.row_get( self, attribute )

# To approach the :py:class:`csv.DictReader` API (without the eager processing),
# we can make the ``Row`` API slightly more fluent with a ``by_name()``
# method.
#
# ..  parsed-literal::
#
#         def by_name( self, name ):
#             attr= self.sheet.schema.get_name(name)
#             return self.cell( attr )
#
# ExternalSchemaSheet Class
# ==========================
#
# ..  py:class:: ExternalSchemaSheet
#
# A Sheet with an external schema can be one of two kinds.
#
# -   A Sheet that doesn't have row headers to embed the schema information.
#     In this case, an eager Workbook Row can create a sequence of :py:class:`cell.Cell` instances.  
#     The Schema information can be associated by position.
#
# -   A Sheet that is really a COBOL or Fixed format file.
#     In this case, the Workbook cannot create a sequence of :py:class:`cell.Cell` instances.  
#     Instead, the Sheet (which has schema information) must
#     provide a LazyRow with deferred Cell conversions.
#
# ::

class ExternalSchemaSheet( Sheet ):
    """A Sheet with an external Schema."""
    def __init__( self, workbook, sheet_name, schema ):
        """Initialize a sheet for processing.

        :param workbook: the containing workbook
        :param sheet_name: the specific sheet to locate within the Workbook
        :param schema: the :py:class:`schema.Schema` schema definition.
        """
        super().__init__( workbook, sheet_name )
        self.schema= schema
    def rows( self ):
        """Iterate through the rows of this sheet."""
        return self.workbook.rows_of( self )

# EmbeddedSchemaSheet Class
# ==========================
#
# ..  py:class:: EmbeddedSchemaSheet
#
# A sheet with an embedded schema must have a loader class provided.  The loader
# is invoked to build a :py:class:`schema.Schema` object.
# It's also used to return the rest of the rows; those that weren't used to build the schema.
#
# ::

class EmbeddedSchemaSheet( ExternalSchemaSheet ):
    """A Sheet with a Schema embedded in it."""
    def __init__( self, workbook, sheet_name, loader_class ):
        """Initialize a sheet for processing.

        :param workbook: the containing workbook
        :param sheet_name: the specific sheet to locate within the Workbook
        :param loader_class: the :py:class:`schema.loader.SchemaLoader`
        schema loader to load the schema from the sheet.

        Apply the loader to the given sheet of the workbook to get schema
        and rows.
        """
        s = Sheet( workbook, sheet_name )
        self.loader = loader_class( s )
        schema= self.loader.schema()
        super().__init__( workbook, sheet_name, schema=schema )
    def rows( self ):
        """The parser will skip over the headers."""
        return self.loader.rows()

# Since the rows are already properly encoded as :py:class:`cell.Cell` instances,
# no further processing is required by the Sheet or the Loader.
#
# Rows of a Sheet
# ==================
#
# Note that the :mod:`csv` design pattern for each row involves two subclasses
# with the same method names but different results.  One
# returns a ``dict`` of cells, the other returns a ``list`` of cells.
#
# The dict-based processing has the advantage of clarity.  It has the
# disadvantage of not coping well with duplicate column names or data
# which breaks first normal form.
#
# Also, note that :mod:`csv` does eager creation of each row.  
# The :py:class:`csv.DictReader` does eager creation of a dictionary from each row.
#
# We don't follow the :mod:`csv` design pattern.  Instead we do the following.
#
# -   A :py:class:`sheet.Row` can be a lazy sequence of :py:class:`cell.Cell` instances.
#
# -   A :py:class:`schema.Schema` must be used to fetch :py:class:`cell.Cell` 
#     instances from the :py:class:`sheet.Row`.
#
# -   To create dict-like access to Cell instances, the schema can be turned into a dictionary.  
#     This "schema-as-dict" can then be used with a properly
#     lazy Row to create Cell instances.
#
# This lazy evaluation of a row that fetches data based on :py:class:`schema.Attribute`
# details  allows us to cope with COBOL ``REDEFINES``.  It also allows us to cope
# with the unfortunately common problem of duplicate column names in conventional
# spreadsheets.
#
# We can have application programming which looks like this to process rows in a number of ways.
#
# Row as sequence is the default.
#
# ..  parsed-literal::
#
#     for row in sheet.rows():
#         *Cell:* row[i]
#         *Schema Attribute Name:* sheet.schema[i].name
#
# Row as dict is a common alternative.  If we have unique column names in the schema,
# We can than use application programming that looks like this.
#
# ..  parsed-literal::
#
#     schema_dict = dict( (a.name, a) for a in sheet.schema )
#     for row in sheet.rows():
#         *Cell:* row.cell(schema_dict['name'])
#         row_as_dict= dict(
#             (a.name, row.cell(a)) for a in sheet.schema )
#         *Cell:* row_as_dict['name']
#
# This handles the COBOL case, where rows must be lazy.
# This includes COBOL ``REDEFINES`` and occurs clauses. 
# This assures proper packed decimal conversion of redefined fields.
