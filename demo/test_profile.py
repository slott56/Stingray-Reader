"""
Testing demo/profile.py
"""

from collections import Counter
import logging
from pathlib import Path
import pytest
from unittest.mock import Mock, sentinel
from stingray import Row
import demo.profile


@pytest.fixture
def mock_workbook():
    mock_nav_name = Mock(
        name="Nav()", value=Mock(side_effect=["123", "1", "2", "3", "4.1", "4.2"])
    )
    mock_nav = Mock()
    mock_nav.name = Mock(return_value=mock_nav_name)
    return Mock(name="Workbook", unpacker=Mock(nav=Mock(return_value=mock_nav)))


@pytest.fixture
def mock_schema():
    return Mock(name="Schema", properties={"property": {}})


@pytest.fixture
def mock_sheet_row(mock_schema, mock_workbook):
    mock_sheet = Mock(
        name="Sheet",
        schema=mock_schema,
        workbook=Mock(return_value=mock_workbook),
    )
    mock_row = Row(mock_sheet, ["raw", "raw"])
    mock_sheet.rows = Mock(return_value=[mock_row])
    return mock_sheet, mock_row


def test_stats(mock_workbook, mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    stats = demo.profile.Stats(mock_sheet)
    stats.sample("column", "value")
    doc = stats.serialize()
    assert doc.splitlines() == [
        f"{mock_workbook.name} :: {mock_sheet.name}",
        "=======================================================================================",
        "",
        "property",
        "--------",
        "",
        "{}",
        "",
        "..  csv-table::",
        "",
        "",
    ]


@pytest.fixture
def mock_stats():
    return Mock(serialize=Mock(name="serialize"))


def test_persistent_processing(tmp_path, mock_stats, capsys):
    """
    An alternative is to mock the target Path object.
    """
    target = tmp_path / "target.data"

    with demo.profile.Profile_Processing(target) as mode:
        mode.save_stats(mock_stats)
    out, err = capsys.readouterr()
    assert out.splitlines() == [str(mock_stats.serialize())]


@pytest.fixture
def mock_builder(monkeypatch):
    builder = Mock(return_value={"x123": sentinel.X123, "y4": sentinel.Y4})
    monkeypatch.setattr(demo.conversion, "builder", builder)
    return builder


@pytest.fixture
def mock_persistence():
    return Mock()


def test_process_sheet(mock_builder, mock_sheet_row, mock_persistence):
    mock_sheet, mock_row = mock_sheet_row
    counts = demo.profile.process_sheet(
        mock_sheet,
        mock_persistence,
    )
    assert counts == Counter({"read": 1})


def test_parse_args():
    options = demo.profile.parse_args(["-v", "file.name"])
    assert options.file == [Path("file.name")]
    assert not options.fail_fast
    assert options.verbosity == logging.DEBUG
