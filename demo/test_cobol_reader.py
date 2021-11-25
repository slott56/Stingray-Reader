"""
Testing demo/cobol_reader.py
"""
import pytest
from unittest.mock import Mock, MagicMock, call, sentinel
from stingray import Workbook, Sheet, Row
import demo.cobol_reader


@pytest.fixture
def mock_workbook():
    mock_nav_name = MagicMock(
        name="Nav()",
        value=Mock(side_effect=['123', '1', '2', '3', '4.1', '4.2'])
    )
    mock_nav = MagicMock(
    )
    mock_nav.name=Mock(return_value=mock_nav_name)
    return Mock(
        name="Workbook",
        unpacker=Mock(
            nav=Mock(return_value=mock_nav)
        )
    )

@pytest.fixture
def mock_schema():
    return Mock(
        name="Schema",
        properties=["properties"]
    )

@pytest.fixture
def mock_sheet_row(mock_schema, mock_workbook):
    mock_sheet = Mock(
        name="Sheet",
        schema=mock_schema,
        workbook=Mock(return_value=mock_workbook),
    )
    mock_row = Row(
        mock_sheet,
        ['raw', 'raw']
    )
    mock_sheet.rows = Mock(return_value=[mock_row])
    return mock_sheet, mock_row

def test_header_builder(mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    header = demo.cobol_reader.header_builder(mock_row)
    assert header == {
        'copyright_symbol': '2',
        'file_version_month': '1',
        'file_version_year': '123',
        'tape_sequence_no': '3'
    }


def test_detail_builder(mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    detail = demo.cobol_reader.detail_builder(mock_row)
    assert detail['county_name'] == '4.1'
    assert detail['county_no'] == '3'
    assert 'high_sector' in detail  # Mocks
    assert 'high_segment' in detail
    assert 'low_sector' in detail
    assert 'low_segment' in detail
    assert detail['state_abbrev'] == '2'
    assert detail['update_key_no'] == '1'
    assert detail['zip_code'] == '123'

