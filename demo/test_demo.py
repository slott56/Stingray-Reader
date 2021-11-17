"""
Unit Level Validation for Application and Data

We validate that a given file actually matches the required schema through a three-part valiation process.

1.  Validate application's use of a schema via conventional unit testing.
    This is `Unit Test The Builder Function`_, the first part of our testing.

2.  Validate file conformance to a schema via "live-file testing".
    This is `Live Data Test The Builder Function`_, the second part of our testing.

3.  Validate the three-way application-schema-file binding by including a
    **File Validation** mode in every file processing application.
    We'll look at the part 3 (the 3-way binding of app-schema-file) in :ref:`demo_validate`.

Of course, we must do good unit testing on the application overall.
We'll assume that without further discussion.  Failure to test is simply failure.

Live file testing moves beyond simple unit testing into a realm of establishing evidence
that an application will process a given file correctly.  We're gathering auditable
historical information on file formats and contents.

Our focus here is validation of the application-schema binding as well as the file-schema binding.

We can use the  :py:mod:`unittest` module to write tests that
validate the schema shared by an application and file.
There are three levels to this validation.

-   Unit-level.  This is a test of the builder functions more-or-less in isolation.
    There are two kinds of builder validation.

    -   Subsets of rows.  Developers usually start here.

    -   Whole ("live") files.  Production files are famous for having a few "odd" rows.
        Whole-file tests are essential.

-   Integration-level.  This is a test of a complete application using
    A test database (or collection of files) is prepared and the application
    actually exercised against actual files.

Not *everything* is mocked here: the "unit" is an integrated collection of
components. Not an isolated class.
"""

import pytest
from unittest.mock import Mock
import unittest
from collections import defaultdict
from pathlib import Path
import os
from typing import Any
from stingray.workbook import (
    open_workbook, HeadingRowSchemaLoader, Workbook, Sheet, Row,
    WBUnpacker, JSON, SchemaMaker
)
from stingray.schema_instance import JSON
from jsonschema import Draft202012Validator  # type: ignore [import]


# Builder Functions
# ==================
#
# A "builder function." transforms a source row object into the target object or collection.
#
# Normally, we import the unit under test.
# It would look like this:
#    
# ..  parsed-literal::
#
#     from demo.some_app import some_builder
#
# For this demo, here's a sample builder function.
#
# ..  py:function:: some_builder(aRow)
#
#     Build a Python dict from the row of data.
#
# ::

def some_builder(aRow: Row) -> dict[str, Any]:
    return dict( 
        key = aRow['Column "3" - string'].value(),
        value = aRow['Col 2.0 - float'].value()
    )

# Mock Workbook
# ===============
#
# We'll need to subclass :py:class:`Workbook` for testing purposes.
# The class is abstract, and we need to fill in a few things.
#
# ::


class TestWB(Workbook):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.unpacker = WBUnpacker()

    def sheet(self, name):
        return Sheet(self, "sheet")

    def sheet_iter(self):
        yield self.sheet()

    def instance_iter(self, sheet):
        yield [
            42.0, 3.1415926, 'string', 20708.0, True, None, '#DIV/0!'
        ]



# Unit Test The Builder Function
# ===============================
#            
# Step one is to unit test the :py:func:`some_builder` function with selected row-like objects
# from a the :py:class:`MockWorkbook`.
#
# There are two parts: a :py:class:`schema.Schema` and 
# a :py:class:`sheet.ExternalSchemaSheet` that contains the mock row(s).
#
# ::

def test_some_builder():
    json_schema = {
        "title": "Unit test workbook",
        "type": "object",
        "properties": {
            'Col 1 - int': {"type": "integer"},
            'Col 2.0 - float': {"type": "number"},
            'Column "3" - string': {"type": "string"},
            "Column '4' - date": {"type": "number", "conversion": "excel_date"},
            'Column 5 - boolean': {"type": "boolean"},
            'Column 6 - empty': {"type": "null"},
            'Column 7 - Error': {"type": "string"},
        }
    }
    Draft202012Validator.check_schema(json_schema)
    schema = SchemaMaker().from_json(json_schema)
    wb = TestWB("workbook", Mock(name="Mock File"))
    sheet = wb.sheet("Sheet 1").set_schema(schema)
    row_iter = sheet.row_iter()
    r = next(row_iter)
    assert some_builder(r) == {'key': 'string', 'value': 3.1415926}
    with pytest.raises(StopIteration):
        next(row_iter)


# Live Data Test The Builder Function
# ====================================
#
# Step two is to unit test the :py:func:`some_builder` function with all rows in a given workbook.
# In this demo, we're using :file:`sample/excel97_workbook.xls`. Generally, we want to compute
# some aggregate (like a checksum) of various data items to be sure we've read and 
# converted them properly. 
#
# Pragmatically, it's sometimes hard to get a proper checksum, so we have to resort
# to sums, counts, and perhaps even frequency distribution tables.
#
# ::

class Test_Builder_2_Live( unittest.TestCase ):
    def setUp( self ):
        workbook_path = Path(os.environ.get("SAMPLES", "sample")) / "excel97_workbook.xls"
        self.wb = open_workbook(workbook_path)
        self.sheet = EmbeddedSchemaSheet(self.wb, 'Sheet1', HeadingRowSchemaLoader)
        self.schema = self.sheet.schema
    def test_should_build_all_rows( self ):
        summary= defaultdict( int )
        for row in self.sheet.rows():
            dict_row= dict( (a.name,row.cell(a)) for a in self.schema )
            result= some_builder( dict_row )
            summary[result['key']] += 1
        self.assertEqual( 1, summary['string'] )
        self.assertEqual( 1, summary['data'] )

# Sheet-Level Testing
# ========================
#        
# See :ref:`developer` for background. We're going to need a "sheet process function."
# This transforms the source sheet into the target collection, usually an output file.
#
# We can also use the unit test tools to test the application's
# higher-level :py:func:`process_some_sheet` function.
#
# Normally, we import the unit under test.
# It looks like this:
#    
# ..  parsed-literal::
#
#     from demo.some_app import process_some_sheet
#
# For this demo, here's a sample sheet process function:
#
# ..  py:function:: process_some_sheet(sheet)
#
#     Process all rows of the named sheet.
#
# ::

def process_some_sheet( sheet ):
    counts= defaultdict( int )
    for row in sheet.schema.rows_as_dict_iter(sheet):
        row_dict= some_builder( row )
        counts['key',row_dict['key']] += 1
        counts['read'] += 1
    return counts

# The unit test checks the embedded schema and the overall row counts
# from processing the live file.
#
# In this demo, we're using :file:`sample/excel97_workbook.xls`. 
#
# The test opens the workbook. It selects a sheet from the workbook using the class
# that extracts the schema from the row headers. The test then uses the :py:func:`process_some_sheet`
# function on the given sheet to extract data. In this case, the extraction is 
# a frequency table.
#
# ::

class Test_Sheet_Builder_2_Live( unittest.TestCase ):
    def setUp( self ):
        workbook_path = Path(os.environ.get("SAMPLES", "sample")) / "excel97_workbook.xls"
        self.wb = open_workbook(workbook_path)
        self.sheet = EmbeddedSchemaSheet(self.wb, 'Sheet1', HeadingRowSchemaLoader)
        self.schema = self.sheet.schema
    def test_should_load_schema( self ):
        self.assertEqual( 'Col 1 - int', self.sheet.schema[0].name )
        self.assertEqual( 'Col 2.0 - float', self.sheet.schema[1].name )
        self.assertEqual( 'Column "3" - string', self.sheet.schema[2].name )
        self.assertEqual( "Column '4' - date", self.sheet.schema[3].name )
        self.assertEqual( 'Column 5 - boolean', self.sheet.schema[4].name )
        self.assertEqual( 'Column 6 - empty', self.sheet.schema[5].name )
        self.assertEqual( 'Column 7 - Error', self.sheet.schema[6].name )
    def test_should_build_sample_row( self ):
        counts= process_some_sheet( self.sheet )
        self.assertEqual( 2, counts['read'] )
        self.assertEqual( 1, counts['key','string'] )
        self.assertEqual( 1, counts['key','data'] )

# Main Program Switch
# ====================
#
# This is a common unittest main program.  Ideally, we'd create an actual
# suite object to allow combining tests.
#
# ::

if __name__ == "__main__":
    unittest.main()
