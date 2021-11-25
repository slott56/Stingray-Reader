"""
Testing demo/conversion.py
"""

import logging
from pathlib import Path
import pytest
from unittest.mock import Mock, mock_open, call, sentinel
from stingray import Row
import demo.conversion


@pytest.fixture
def mock_workbook():
    mock_nav_name = Mock(
        name="Nav()",
        value=Mock(side_effect=['123', '1', '2', '3', '4.1', '4.2'])
    )
    mock_nav = Mock(
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

def test_builder(mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    doc = demo.conversion.builder(mock_row)
    assert doc == {'x123': 123.0, 'x4': 4.1, 'y1': 1.0, 'y2': 2.0, 'y3': 3.0, 'y4': 4.2}

@pytest.fixture
def mock_document():
    return {"JSON": "Document", "With": [4.0, "Strings"]}

def test_persistent_processing(tmp_path, mock_document):
    """
    An alternative is to mock the target Path object.
    """
    target = tmp_path/"target.data"

    with demo.conversion.Persistent_Processing(target) as mode:
        mode.save_json(mock_document)
    assert target.read_text() == '{"JSON": "Document", "With": [4.0, "Strings"]}\n'


def test_validation_processing(tmp_path, mock_document):
    """
    An alternative is to mock the target Path object.
    """
    target = tmp_path/"target.data"

    with demo.conversion.Validation_Processing(target) as mode:
        mode.save_json(mock_document)
    assert not target.exists()


@pytest.fixture
def mock_builder(monkeypatch):
    builder = Mock(
        return_value={"x123": sentinel.X123, "y4": sentinel.Y4}
    )
    monkeypatch.setattr(demo.conversion, "builder", builder)
    return builder

@pytest.fixture
def mock_persistence():
    return Mock(
    )

def test_process_sheet(mock_builder, mock_sheet_row, mock_persistence):
    mock_sheet, mock_row = mock_sheet_row
    counts = demo.conversion.process_sheet(
        mock_sheet,
        mock_persistence,
    )
    assert counts == {'input': 1, 'invalid': 1}


def test_parse_args():
    options = demo.conversion.parse_args(["-d", "-v", "file.name"])
    assert options.file == [Path("file.name")]
    assert options.dry_run
    assert options.verbosity == logging.DEBUG
