"""
Testing Demonstration.

This is the example code for the demo/data_quality section of the documentation.
"""

import pytest
from unittest.mock import Mock, call, sentinel
from collections import Counter
from pathlib import Path
import os
from typing import Any
from stingray import Row
from stingray import (
    open_workbook, HeadingRowSchemaLoader, Workbook, Sheet,
    WBUnpacker, JSON, SchemaMaker, name_cleaner
)
from jsonschema import Draft202012Validator  # type: ignore [import]


# Unit Test The Builder Function
# ==============================
#
# A "builder function." transforms a source row object into a standard imermediate form.


def some_builder(aRow: Row) -> dict[str, Any]:
    return dict( 
        key = aRow['Column "3" - string'].value(),
        value = aRow['Col 2.0 - float'].value()
    )

@pytest.fixture
def mock_row():
    attr_2 = Mock(
        value=Mock(return_value=3.1415926)
    )
    attr_3 = Mock(
        value=Mock(return_value='string')
    )
    row = {
        'Col 2.0 - float': attr_2,
        'Column "3" - string': attr_3
    }
    return row

def test_builder(mock_row):
    document = some_builder(mock_row)
    assert document == {'key': 'string', 'value': 3.1415926}

# Integration Test The Builder Function version 1
# ===============================================

@pytest.fixture
def mock_schema():
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
    return schema

@pytest.fixture
def mock_sheet(mock_schema):
    workbook = Mock(
        unpacker=WBUnpacker()
    )
    sheet = Mock(
        workbook=Mock(return_value=workbook),
        schema=mock_schema,
    )
    return sheet

@pytest.fixture
def row_instance(mock_sheet):
    row = Row(
        mock_sheet,
        [
            42.0, 3.1415926, 'string', 20708.0, True, None, '#DIV/0!'
        ]
    )
    return row

def test_builder_1(row_instance):
    document = some_builder(row_instance)
    assert document == {'key': 'string', 'value': 3.1415926}

# Integration Test The Builder Function version 2
# ===============================================
#
# We can to subclass :py:class:`Workbook` and :py:class:`WBUnpacker` for testing purposes.
# This allows the Workbook and Sheet to build a :py:class:`Row` object for us.

class MockUnpacker(WBUnpacker):
    def sheet_iter(self):
        yield "sheet"

    def instance_iter(self, sheet):
        yield [
            42.0, 3.1415926, 'string', 20708.0, True, None, '#DIV/0!'
        ]

class MockWB(Workbook):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.unpacker = MockUnpacker()

def test_builder_integration(mock_schema):
    wb = MockWB("workbook")
    sheet = wb.sheet("Sheet 1").set_schema(mock_schema)
    row_iter = sheet.row_iter()
    r = next(row_iter)
    assert some_builder(r) == {'key': 'string', 'value': 3.1415926}
    with pytest.raises(StopIteration):
        next(row_iter)


# Live File Test The Builder Function
# ====================================
#
# Test using using :file:`sample/excel97_workbook.xls`.

@pytest.fixture
def sample_workbook_sheet():
    workbook_path = Path(os.environ.get("SAMPLES", "sample")) / "excel97_workbook.xls"
    wb = open_workbook(workbook_path)
    sheet = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
    # This is essential for keeping the workbook open.
    # Once the `wb` variable goes out of scope, the workbook is closed.
    yield wb, sheet

def test_should_build_all_rows(sample_workbook_sheet):
    wb, sheet = sample_workbook_sheet
    summary = Counter()
    for row in sheet.rows():
        result = some_builder(row)
        summary[result['key']] += 1
    assert summary['string'] == 1
    assert summary['data'] == 1

# Unit Test the Sheet Function
# ============================

def process_some_sheet(sheet: Sheet):
    counts = Counter()
    for row in sheet.rows():
        row_dict = some_builder(row)
        counts['key',row_dict['key']] += 1
        counts['read'] += 1
    return counts

@pytest.fixture
def mock_sheet_2():
    return Mock(
        rows=Mock(return_value=[sentinel.ROW])
    )

@pytest.fixture
def mock_builder():
    mock_builder = Mock(
        return_value={'key': sentinel.KEY}
    )
    return mock_builder

def test_process_some_sheet(mock_sheet_2, mock_builder, monkeypatch):
    monkeypatch.setitem(globals(), 'some_builder', mock_builder)
    counts = process_some_sheet(mock_sheet_2)
    assert some_builder.mock_calls == [call(sentinel.ROW)]
    assert counts == Counter({('key', sentinel.KEY): 1, 'read': 1})

# An alternative with fewer mocks.

def test_process_some_sheet_2(mock_schema):
    wb = MockWB("workbook")
    sheet = wb.sheet("Sheet 1").set_schema(mock_schema)
    counts = process_some_sheet(sheet)
    assert counts[('key', 'string')] == 1
    assert counts['read'] == 1


# Live File Testing the Sheet function
# =======================================
#
# In this demo, we're using :file:`sample/excel97_workbook.xls`.

def test_should_build_sample_row(sample_workbook_sheet):
    wb, sheet = sample_workbook_sheet
    counts = process_some_sheet(sheet)
    assert counts['read'] == 2
    assert counts['key','string'] == 1
    assert counts['key','data'] == 1

    column_names = list(sheet.schema.properties.keys())
    assert column_names == [
        'Col 1 - int',
        'Col 2.0 - float',
        'Column "3" - string',
        "Column '4' - date",
        'Column 5 - boolean',
        'Column 6 - empty',
        'Column 7 - Error',
    ]
