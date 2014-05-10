# ########################
# COBOL Integration Tests
# ########################
#
# These tests use the unittest framework, but don't test small 
# fixtures in isolation.   These are "integration" tests of the :py:mod:`cobol`
# and :py:mod:`cobol.loader` modules.
#
# See :ref:`cobol` and :ref:`cobol_loader`.
#
# Overheads
# =================
#
# ::

"""stingray.cobol Integration Tests.
This tests the cobol and cobol loader without mocked objects.
"""
import unittest
import decimal
import stingray.cobol.loader
import logging, sys

# Superclass For Tests
# ======================
#
# This is a handy superclass for all the various tests.  It refactors the 
# :meth:`setUp` method to assure that all of the tests have a common fixture.
#
# ::

class DDE_Test( unittest.TestCase ):
    def setUp( self ):
        self.lexer= stingray.cobol.loader.Lexer()
        self.rf= stingray.cobol.loader.RecordFactory()

# DDE Test copybook 1 with basic features
# ========================================
#
# Some basic COBOL.
#
# ::

copy1= """
      * COPY1.COB
       01  DETAIL-LINE.
           05                              PIC X(7).
           05  QUESTION                    PIC ZZ.
           05                              PIC X(6).
           05  PRINT-YES                   PIC ZZ.
           05                              PIC X(3).
           05  PRINT-NO                    PIC ZZ.
           05                              PIC X(6).
           05  NOT-SURE                    PIC ZZ.
           05                              PIC X(7).
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_1( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde1 = list(self.rf.makeRecord( self.lexer.scan(copy1) ))[0]
        #stingray.cobol.loader.report( self.dde1 )
    def test_should_parse( self ):
        dde1= self.dde1
        self.assertEqual( 7, dde1.get( "QUESTION" ).offset )
        self.assertEqual( 2, dde1.get( "QUESTION" ).size )
        self.assertEqual( "ZZ", dde1.get( "QUESTION" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde1.get( "QUESTION" ).usage.source() )
        self.assertEqual( 15, dde1.get( "PRINT-YES" ).offset )
        self.assertEqual( 2, dde1.get( "PRINT-YES" ).size )
        self.assertEqual( "ZZ", dde1.get( "PRINT-YES" ).sizeScalePrecision.final  )
        self.assertEqual( 20, dde1.get( "PRINT-NO" ).offset )
        self.assertEqual( 2, dde1.get( "PRINT-NO" ).size )
        self.assertEqual( "ZZ", dde1.get( "PRINT-NO" ).sizeScalePrecision.final  )
        self.assertEqual( 28, dde1.get( "NOT-SURE" ).offset )
        self.assertEqual( 2, dde1.get( "NOT-SURE" ).size )
        self.assertEqual( "ZZ", dde1.get( "NOT-SURE" ).sizeScalePrecision.final  )
    def test_should_extract( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde1] )
        #print( schema )
        schema_dict= dict( (a.name, a) for a in schema )
        data= stingray.cobol.Character_File( name="", 
            file_object= ["ABCDEFG01HIJKLM02OPQ03RSTUVW04YZabcde",], 
            schema=schema )

        row= next( data.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        
        self.assertEqual( "1", row.cell(schema_dict['QUESTION']).to_str() )
        self.assertEqual( 2, row.cell(schema_dict['PRINT-YES']).to_int() )
        self.assertEqual( 3, row.cell(schema_dict['PRINT-NO']).to_float() )
        self.assertEqual( decimal.Decimal('4'), row.cell(schema_dict['NOT-SURE']).to_decimal() )


# DDE Test copybook 2 with 88-level item
# ========================================
#
#
# Include 88-level items in the source.
#
# ::

copy2= """
      * COPY2.COB
       01  WORK-AREAS.
           05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE 'YES'.
               88  NO-MORE-RECORDS                     VALUE 'NO '.
           05  ANSWER-SUB                  PIC 99.
           05  QUESTION-SUB                PIC 99.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_2( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde2= list(self.rf.makeRecord( self.lexer.scan(copy2) ))[0]
        #stingray.cobol.loader.report( self.dde2 )
    def test_should_parse( self ):
        dde2= self.dde2
        self.assertEqual( 0, dde2.get("ARE-THERE-MORE-RECORDS").offset )
        self.assertEqual( 3, dde2.get("ARE-THERE-MORE-RECORDS").size )
        self.assertEqual( "XXX", dde2.get("ARE-THERE-MORE-RECORDS").sizeScalePrecision.final  )
        self.assertEqual( 0, dde2.get("NO-MORE-RECORDS").offset )
        self.assertEqual( 3, dde2.get("NO-MORE-RECORDS").size )
        self.assertEqual( 3, dde2.get("ANSWER-SUB").offset )
        self.assertEqual( 5, dde2.get("QUESTION-SUB").offset )
    def test_should_extract( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde2] )
        schema_dict= dict( (a.name, a) for a in schema )
        data= stingray.cobol.Character_File( name="", 
            file_object= ["NO 4567",], 
            schema=schema, )
        
        row= next( data.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        self.assertEqual( "NO", row.cell(schema_dict["ARE-THERE-MORE-RECORDS"]).to_str() )
        self.assertEqual( "NO", row.cell(schema_dict["NO-MORE-RECORDS"]).to_str() )

# DDE Test copybook 3 with nested occurs level
# =============================================
#
# This is a common two-dimensional COBOL structure.
#
# ::

copy3= """
      * COPY3.COB
       01  SURVEY-RESPONSES.
           05  QUESTION-NUMBER         OCCURS 10 TIMES.
               10  RESPONSE-CATEGORY     OCCURS 3 TIMES.
                   15  ANSWER                          PIC 99.
"""

# Be sure that the various access methods (via Attribute and via Python tuple-of-tuples) all work.
#
# ::

class Test_Copybook_3( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde3= list(self.rf.makeRecord( self.lexer.scan(copy3) ))[0]
        #stingray.cobol.loader.report( self.dde3 )
    def test_should_extract( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde3] )
        schema_dict= dict( (a.name, a) for a in schema )
        data = stingray.cobol.Character_File( name="", 
            file_object= ["111213212223313233414243515253616263717273818283919293010203",],
            schema=schema )
        
        row= next( data.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        self.assertEqual( 12, row.cell(schema_dict.get('ANSWER').index(1-1,2-1)).to_int() )
        self.assertEqual( 21, row.cell( schema_dict.get('ANSWER').index(2-1,1-1)).to_int() )
        self.assertEqual( 21, row.cell( schema_dict.get('ANSWER').index(1-1,4-1)).to_int() )
        try:
            self.assertEqual( 21, row.cell( schema_dict.get('ANSWER').index(1))[4].to_int() )
            self.fail()
        except IndexError as e:
            pass

# DDE Test copybook 4 from page 174 with nested occurs level
# ============================================================
#
# From IBM COBOL Language Reference Manual, fourth edition: SC26-9046-03.
#
# ::
    
page174= """
       01 TABLE-RECORD.
          05 EMPLOYEE-TABLE OCCURS 10 TIMES
                ASCENDING KEY IS WAGE-RATE EMPLOYEE-NO
                INDEXED BY A, B.
             10 EMPLOYEE-NAME PIC X(20).
             10 EMPLOYEE-NO PIC 9(6).
             10 WAGE-RATE PIC 9999V99.
             10 WEEK-RECORD OCCURS 52 TIMES
                   ASCENDING KEY IS WEEK-NO INDEXED BY C.
                15 WEEK-NO PIC 99.
                15 AUTHORIZED-ABSENCES PIC 9.
                15 UNAUTHORIZED-ABSENCES PIC 9.
                15 LATE-ARRIVALS PIC 9.
"""

# Be sure it parses.  There's nothing novel in the structure, but the syntax
# has numerous things we need to gracefully skip.
#
# ::

class Test_Copybook_4( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde4= list(self.rf.makeRecord( self.lexer.scan(page174) ))[0]
        #stingray.cobol.loader.report( self.dde4 )
    def test_should_parse( self ):
        dde4= self.dde4
        self.assertEqual( 2920, dde4.size )
        self.assertEqual( 0, dde4.offset )
        self.assertEqual( 10, dde4.get("EMPLOYEE-TABLE" ).occurs.number(None) )
        self.assertEqual( 52, dde4.get("WEEK-RECORD" ).occurs.number(None) )
        self.assertEqual( 5, dde4.get("WEEK-RECORD" ).size )
        self.assertEqual( 52*5+32, dde4.get("EMPLOYEE-TABLE" ).size )
        self.assertEqual( "999999", dde4.get("EMPLOYEE-NO").sizeScalePrecision.final  )

        schema = stingray.cobol.loader.make_schema( [dde4] )
        schema_dict= dict( (a.name, a) for a in schema )
        self.assertEqual( (52*5+32)+32+5+4, schema_dict["LATE-ARRIVALS"].index(1,1).offset )
        self.assertEqual( (52*5+32)+32+5+5+4, schema_dict["LATE-ARRIVALS"].index(1,2).offset )
        
            
# DDE Test copybook 5 from page 195 with simple redefines
# =======================================================
#
# Here is a redefines example.
#
# ::

page195= """
       01  REDEFINES-RECORD.
           05  A PICTURE X(6).
           05  B REDEFINES A.
               10  B-1 PICTURE X(2).
               10  B-2 PICTURE 9(4).
           05  C PICTURE 99V99.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_5( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde5= list(self.rf.makeRecord( self.lexer.scan(page195) ))[0]
        #stingray.cobol.loader.report( self.dde5 )
    def test_should_parse( self ):
        dde5= self.dde5
        self.assertEqual( 10, dde5.size )
        self.assertEqual( 6, dde5.get("A").size )
        self.assertEqual( 0, dde5.get("A").offset )
        self.assertEqual( 6, dde5.get("B").size )
        self.assertEqual( 0, dde5.get("B").offset )
        self.assertEqual( 2, dde5.get("B-1").size )
        self.assertEqual( 0, dde5.get("B-1").offset )
        self.assertEqual( 4, dde5.get("B-2").size )
        self.assertEqual( 2, dde5.get("B-2").offset )
        self.assertEqual( "9999", dde5.get("B-2").sizeScalePrecision.final )
        self.assertEqual( 4, dde5.get("C").size )
        self.assertEqual( 6, dde5.get("C").offset )
        
    def test_should_extract( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde5] )
        schema_dict= dict( (a.name, a) for a in schema )
        data= stingray.cobol.Character_File( name="", 
            file_object= ["AB12345678",],
            schema=schema )
        
        row= next( data.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )

        self.assertEqual( "AB1234", row.cell(schema_dict["A"]).to_str() ) 
        self.assertEqual( "AB1234", row.cell(schema_dict["B"]).to_str() ) 
        self.assertEqual( "AB", row.cell(schema_dict["B-1"]).to_str() ) 
        self.assertEqual( "1234", row.cell(schema_dict["B-2"]).to_str() ) 
        self.assertEqual( "56.78", row.cell(schema_dict["C"]).to_str() ) 


# DDE Test copybook 6 from page 197 with another redefines
# =========================================================
#
# ::

page197= """
       01  REDEFINES-RECORD.
           05 NAME-2.
              10 SALARY PICTURE XXX.
              10 SO-SEC-NO PICTURE X(9).
              10 MONTH PICTURE XX.
           05 NAME-1 REDEFINES NAME-2.
              10 WAGE PICTURE 999V999.
              10 EMP-NO PICTURE X(6).
              10 YEAR PICTURE XX.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_6( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde6= list(self.rf.makeRecord( self.lexer.scan(page197) ))[0]
        #stingray.cobol.loader.report( self.dde6 )
    def test_should_parse( self ):
        dde6= self.dde6
        self.assertEqual( 3, dde6.get("SALARY").size )
        self.assertEqual( 0, dde6.get("SALARY").offset )
        self.assertEqual( 9, dde6.get("SO-SEC-NO").size )
        self.assertEqual( 3, dde6.get("SO-SEC-NO").offset )
        self.assertEqual( 2, dde6.get("MONTH").size )
        self.assertEqual( 12, dde6.get("MONTH").offset )
        self.assertEqual( 6, dde6.get("WAGE").size )
        self.assertEqual( 0, dde6.get("WAGE").offset )
        self.assertEqual( "999999", dde6.get("WAGE").sizeScalePrecision.final )
        self.assertEqual( 3, dde6.get("WAGE").usage.precision )
        self.assertEqual( 6, dde6.get("EMP-NO").size )
        self.assertEqual( 6, dde6.get("EMP-NO").offset )
        self.assertEqual( 2, dde6.get("YEAR").size )
        self.assertEqual( 12, dde6.get("YEAR").offset )

    def test_should_extract_1( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde6] )
        schema_dict= dict( (a.name, a) for a in schema )
        data1= stingray.cobol.Character_File( name="", 
            file_object= ["ABC123456789DE",], 
            schema=schema )
        row= next( data1.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        
        self.assertEqual( "ABC", row.cell(schema_dict["SALARY"]).to_str() )
        self.assertEqual( "123456789", row.cell(schema_dict["SO-SEC-NO"]).to_str() )
        self.assertEqual( "DE", row.cell(schema_dict["MONTH"]).to_str() )
        
    def test_should_extract_2( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde6] )
        schema_dict= dict( (a.name, a) for a in schema )
        data2= stingray.cobol.Character_File( name="", 
            file_object= ["123456ABCDEF78",],
            schema=schema )
        row= next( data2.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        
        self.assertAlmostEquals( 123.456, row.cell(schema_dict["WAGE"]).to_float() )
        self.assertEqual( "ABCDEF", row.cell(schema_dict["EMP-NO"]).to_str() )
        self.assertEqual( "78", row.cell(schema_dict["YEAR"]).to_str() )


# DDE Test copybook 7 from page 198, example "A"
# ==============================================
#
# ::

page198A= """
       01  REDEFINES-RECORD.
           05 REGULAR-EMPLOYEE.
              10 LOCATION PICTURE A(8).
              10 GRADE PICTURE X(4).
              10 SEMI-MONTHLY-PAY PICTURE 9999V99.
              10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY
                  PICTURE 999V999.
           05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
              10 LOCATION PICTURE A(8).
              10 FILLER PICTURE X(6).
              10 HOURLY-PAY PICTURE 99V99.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_7( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde7= list(self.rf.makeRecord( self.lexer.scan(page198A) ))[0]
        #stingray.cobol.loader.report( self.dde7 )
    def test_should_parse( self ):
        dde7= self.dde7
        self.assertEqual( 18, dde7.get("REGULAR-EMPLOYEE").size )
        self.assertEqual( 18, dde7.get("TEMPORARY-EMPLOYEE").size )
        self.assertEqual( 6, dde7.get("SEMI-MONTHLY-PAY").size )
        self.assertEqual( 6, dde7.get("WEEKLY-PAY").size )
        
    def test_should_extract_1( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde7] )
        schema_dict= dict( (a.name, a) for a in schema )
        data1= stingray.cobol.Character_File( name="", 
            file_object= ["ABCDEFGHijkl123456",],
            schema=schema )
        row= next( data1.sheet( "" ).rows() )
        # Can't dump with TEMPORARY-EMPLOYEE
        #stingray.cobol.dump( schema, row )
        
        self.assertEqual( '1234.56', row.cell(schema_dict["SEMI-MONTHLY-PAY"]).to_str() )
        
    def test_should_extract_2( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde7] )
        schema_dict= dict( (a.name, a) for a in schema )
        data2= stingray.cobol.Character_File( name="", 
            file_object= ["ABCDEFGHijklmn1234",],
            schema=schema )
        row= next( data2.sheet( "" ).rows() )
        # Can't dump with REGULAR-EMPLOYEE
        #stingray.cobol.dump( schema, row ) 
        
        self.assertEqual( '12.34', row.cell(schema_dict["HOURLY-PAY"]).to_str() )


# DDE Test copybook 8 from page 198, example "B"
# ==============================================
#
# ::
    
page198B= """
       01  REDEFINES-RECORD.
           05 REGULAR-EMPLOYEE.
               10 LOCATION PICTURE A(8).
               10 GRADE PICTURE X(4).
               10 SEMI-MONTHLY-PAY PICTURE 999V999.
           05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
               10 LOCATION PICTURE A(8).
               10 FILLER PICTURE X(6).
               10 HOURLY-PAY PICTURE 99V99.
               10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_8( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde8= list(self.rf.makeRecord( self.lexer.scan(page198B) ))[0]
    def test_should_parse( self ):
        #stingray.cobol.loader.report( self.dde8 )
        dde8= self.dde8
        self.assertEqual( 18, dde8.get("REGULAR-EMPLOYEE").size )
        self.assertEqual( 18, dde8.get("TEMPORARY-EMPLOYEE").size )
        self.assertEqual( 6, dde8.get("SEMI-MONTHLY-PAY").size )
        self.assertEqual( 4, dde8.get("HOURLY-PAY").size )
        self.assertEqual( 4, dde8.get("CODE-H").size )
        
    def test_should_extract_1( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde8] )
        schema_path_dict= dict( (a.path, a) for a in schema )
        data1= stingray.cobol.Character_File( name="", 
            file_object= ["ABCDEFGHijkl123456",],
            schema=schema )
        
        row= next( data1.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        #print( "SEMI-MONTHLY-PAY", schema_path_dict['REDEFINES-RECORD.REGULAR-EMPLOYEE.SEMI-MONTHLY-PAY'] )
        #print( "row.cell(...)", row.cell(schema_path_dict['REDEFINES-RECORD.REGULAR-EMPLOYEE.SEMI-MONTHLY-PAY']) )
        self.assertAlmostEquals( 123.456, 
            row.cell(schema_path_dict['REDEFINES-RECORD.REGULAR-EMPLOYEE.SEMI-MONTHLY-PAY']).to_float()
        )

    def test_should_extract_2( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde8] )
        schema_path_dict= dict( (a.path, a) for a in schema )
        data2= stingray.cobol.Character_File( name="", 
            file_object= ["ABCDEFGHijklmn1234",],
            schema=schema )
        
        row= next( data2.sheet( "" ).rows() )
        #stingray.cobol.dump( schema, row )
        self.assertEqual( 12.34, 
            row.cell(schema_path_dict['REDEFINES-RECORD.TEMPORARY-EMPLOYEE.HOURLY-PAY']).to_float()
        )
        self.assertEqual( 1234, 
            row.cell(schema_path_dict['REDEFINES-RECORD.TEMPORARY-EMPLOYEE.CODE-H']).to_int()
        )

        schema_name_dict= dict( (a.name, a) for a in schema )
        self.assertEqual( "REDEFINES-RECORD.TEMPORARY-EMPLOYEE.HOURLY-PAY",
            schema_name_dict.get('HOURLY-PAY').path )

# Test Copybook 9, Multiple 01 Levels
# ===================================
#
# Some basic COBOL with two top-level records.
#
# ::

copy9= """
       01  DETAIL-LINE.
           05  QUESTION                    PIC ZZ.
           05  PRINT-YES                   PIC ZZ.
           05  PRINT-NO                    PIC ZZ.
           05  NOT-SURE                    PIC ZZ.
       01  SUMMARY-LINE REDEFINES DETAIL-LINE.
           05  COUNT                       PIC ZZ.
           05  FILLER                      PIC XX.
           05  FILLER                      PIC XX.
           05  FILLER                      PIC XX.
"""

# Be sure it parses.  Be sure we can extract data.
#
# ::

class Test_Copybook_9( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde9a, self.dde9b = self.rf.makeRecord( self.lexer.scan(copy9) )
        #stingray.cobol.loader.report( self.dde9a )
        #stingray.cobol.loader.report( self.dde9b )
    def test_should_parse( self ):
        dde9= self.dde9a
        self.assertEqual( 0, dde9.get( "QUESTION" ).offset )
        self.assertEqual( 2, dde9.get( "QUESTION" ).size )
        self.assertEqual( "ZZ", dde9.get( "QUESTION" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde9.get( "QUESTION" ).usage.source() )
        self.assertEqual( 2, dde9.get( "PRINT-YES" ).offset )
        self.assertEqual( 2, dde9.get( "PRINT-YES" ).size )
        self.assertEqual( "ZZ", dde9.get( "PRINT-YES" ).sizeScalePrecision.final  )
        self.assertEqual( 4, dde9.get( "PRINT-NO" ).offset )
        self.assertEqual( 2, dde9.get( "PRINT-NO" ).size )
        self.assertEqual( "ZZ", dde9.get( "PRINT-NO" ).sizeScalePrecision.final  )
        self.assertEqual( 6, dde9.get( "NOT-SURE" ).offset )
        self.assertEqual( 2, dde9.get( "NOT-SURE" ).size )
        self.assertEqual( "ZZ", dde9.get( "NOT-SURE" ).sizeScalePrecision.final  )
        dde9= self.dde9b
        self.assertEqual( 0, dde9.get( "COUNT" ).offset )
        self.assertEqual( 2, dde9.get( "COUNT" ).size )
        self.assertEqual( "ZZ", dde9.get( "COUNT" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde9.get( "COUNT" ).usage.source() )
    def test_should_extract( self ):
        schema = stingray.cobol.loader.make_schema( [self.dde9a, self.dde9b] )
        #print( schema )
        schema_dict= dict( (a.name, a) for a in schema )
        data= stingray.cobol.Character_File( name="", 
            file_object= ["01020304",], 
            schema=schema )

        row= next( data.sheet( "" ).rows() )
        #stingray..dump( schema, row )
        self.assertEqual( "1", row.cell(schema_dict['QUESTION']).to_str() )
        self.assertEqual( 2, row.cell(schema_dict['PRINT-YES']).to_int() )
        self.assertEqual( 3, row.cell(schema_dict['PRINT-NO']).to_float() )
        self.assertEqual( decimal.Decimal('4'), row.cell(schema_dict['NOT-SURE']).to_decimal() )
        self.assertEqual( "1", row.cell(schema_dict['COUNT']).to_str() )

# Test Copybook 10, Occurs Depending On
# ======================================
#
# The basic ODO situation: size depends on another item in the record.
#
# ::

copy10= """
       01  MAIN-AREA.
           03 REC-1.
             05 FIELD-1                       PIC 9.
             05 FIELD-2 OCCURS 1 TO 5 TIMES
                  DEPENDING ON FIELD-1        PIC X(05).
"""

# Be sure it parses. 
#
# To be sure we can compute the offset, we need to extract data.
# For that, we'll need a mock :py:class:`stingray.cobol.COBOL_File` to provide
# data for setting size and offset.
#
# ::

class Test_Copybook_10( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde10 = list(self.rf.makeRecord( self.lexer.scan(copy10) ))[0]
        #stingray.cobol.loader.report( self.dde10 )
    def test_should_parse( self ):
        dde10= self.dde10

        self.assertEqual( 0, dde10.get( "FIELD-1" ).offset )
        self.assertEqual( 1, dde10.get( "FIELD-1" ).size )
        self.assertEqual( "9", dde10.get( "FIELD-1" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde10.get( "FIELD-1" ).usage.source() )

        self.assertEqual( 0, dde10.get( "FIELD-2" ).offset )
        self.assertEqual( 5, dde10.get( "FIELD-2" ).size )
        self.assertEqual( "XXXXX", dde10.get( "FIELD-2" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde10.get( "FIELD-2" ).usage.source() )

    def test_should_setsizeandoffset( self ):
        dde10= self.dde10
        
        schema= stingray.cobol.loader.make_schema( [dde10] )
        self.data = stingray.cobol.Character_File( name="", 
            file_object= ["3111112222233333",],
            schema=schema )
        row= next( self.data.sheet( "IGNORED" ).rows() )

        self.assertEqual( 0, dde10.get( "FIELD-1" ).offset )
        self.assertEqual( 1, dde10.get( "FIELD-1" ).size )
        self.assertEqual( "9", dde10.get( "FIELD-1" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde10.get( "FIELD-1" ).usage.source() )

        self.assertEqual( 1, dde10.get( "FIELD-2" ).offset )
        self.assertEqual( 5, dde10.get( "FIELD-2" ).size )
        self.assertEqual( "XXXXX", dde10.get( "FIELD-2" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde10.get( "FIELD-2" ).usage.source() )


# Test Copybook 11, Complex Occurs Depending On
# ==============================================
#
# A fairly complex ODO situation: size and offset depends other items
# in the record.
#
# :: 

copy11= """
       01  MAIN-AREA.
           03 REC-1.
              05 FIELD-1                       PIC 9.
              05 FIELD-3                       PIC 9.
              05 FIELD-2 OCCURS 1 TO 5 TIMES
                   DEPENDING ON FIELD-1        PIC X(05).
           03 REC-2.
              05 FIELD-4 OCCURS 1 TO 5 TIMES
                   DEPENDING ON FIELD-3        PIC X(05).
"""

# Be sure it parses.  
#
# To be sure we can compute the offset, we need to extract data.
# For that, we'll need a mock :py:class:`stingray.cobol.COBOL_File` to provide
# data for setting size and offset.
#
# ::

class Test_Copybook_11( DDE_Test ):
    def setUp( self ):
        super().setUp()
        self.dde11 = list(self.rf.makeRecord( self.lexer.scan(copy11) ))[0]
        #stingray.cobol.loader.report( self.dde11 )

    def test_should_parse( self ):
        dde11= self.dde11

        self.assertEqual( 0, dde11.get( "FIELD-1" ).offset )
        self.assertEqual( 1, dde11.get( "FIELD-1" ).size )
        self.assertEqual( "9", dde11.get( "FIELD-1" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-1" ).usage.source() )

        self.assertEqual( 0, dde11.get( "FIELD-2" ).offset )
        self.assertEqual( 5, dde11.get( "FIELD-2" ).size )
        self.assertEqual( "XXXXX", dde11.get( "FIELD-2" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-2" ).usage.source() )

        self.assertEqual( 0, dde11.get( "FIELD-3" ).offset )
        self.assertEqual( 1, dde11.get( "FIELD-3" ).size )
        self.assertEqual( "9", dde11.get( "FIELD-3" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-3" ).usage.source() )
        
        self.assertEqual( 0, dde11.get( "FIELD-4" ).offset )
        self.assertEqual( 5, dde11.get( "FIELD-4" ).size )
        self.assertEqual( "XXXXX", dde11.get( "FIELD-4" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-4" ).usage.source() )

    def test_should_setsizeandoffset( self ):
        dde11= self.dde11
        
        schema= stingray.cobol.loader.make_schema( [dde11] )
        self.data = stingray.cobol.Character_File( name="", 
            file_object= ["321111122222333334444455555",],
            schema=schema )
        row= next( self.data.sheet( "" ).rows() )

        self.assertEqual( 0, dde11.get( "FIELD-1" ).offset )
        self.assertEqual( 1, dde11.get( "FIELD-1" ).size )
        self.assertEqual( "9", dde11.get( "FIELD-1" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-1" ).usage.source() )

        self.assertEqual( 2, dde11.get( "FIELD-2" ).offset )
        self.assertEqual( 5, dde11.get( "FIELD-2" ).size )
        self.assertEqual( "XXXXX", dde11.get( "FIELD-2" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-2" ).usage.source() )

        self.assertEqual( 1, dde11.get( "FIELD-3" ).offset )
        self.assertEqual( 1, dde11.get( "FIELD-3" ).size )
        self.assertEqual( "9", dde11.get( "FIELD-3" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-3" ).usage.source() )
        
        self.assertEqual( 17, dde11.get( "FIELD-4" ).offset )
        self.assertEqual( 5, dde11.get( "FIELD-4" ).size )
        self.assertEqual( "XXXXX", dde11.get( "FIELD-4" ).sizeScalePrecision.final  )
        self.assertEqual( "", dde11.get( "FIELD-4" ).usage.source() )

# Test Suite and Runner
# =====================
#
# In case we want to build up a larger test suite, we avoid doing
# any real work unless this is the main module being executed.
#
# ::

import test
suite= test.suite_maker( globals() )

if __name__ == "__main__":
    with test.Logger( stream=sys.stdout, level=logging.INFO ):
        logging.getLogger( "stingray.cobol.defs" ).setLevel( logging.DEBUG )
        logging.info( __file__ )
        #unittest.TextTestRunner().run(suite())
        unittest.main( Test_Copybook_11() ) # Specific debugging
