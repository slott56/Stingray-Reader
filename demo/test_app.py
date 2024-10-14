"""
Testing demo/app.py.
"""

import logging
from pathlib import Path
import pytest
from unittest.mock import Mock, mock_open, call, sentinel
from stingray import Row
import demo.app


def test_this():
    t = demo.app.This("string", 3.1415926)


def test_this_form():
    v = {"key": "string", "value": 3.1415926}
    f_v = demo.app.ThisForm(**v)
    assert f_v.is_valid()
    assert f_v.create() == demo.app.This("string", 3.1415926)

    i = {"key": None, "value": None}
    f_i = demo.app.ThisForm(**i)
    assert not f_i.is_valid()


@pytest.fixture
def mock_this():
    return Mock(serialize=Mock(return_value="serialized"))


def test_persistent_processing(tmp_path, mock_this):
    """
    An alternative is to mock the target Path object.
    """
    target = tmp_path / "target.data"

    with demo.app.Persistent_Processing(target) as mode:
        mode.save_this(mock_this)
    assert target.read_text() == "'serialized'\n"


def test_validation_processing(tmp_path, mock_this):
    """
    An alternative is to mock the target Path object.
    """
    target = tmp_path / "target.data"

    with demo.app.Validate_Only_Processing(target) as mode:
        mode.save_this(mock_this)
    assert not target.exists()


@pytest.fixture
def mock_workbook():
    mock_nav_name = Mock(name="Nav()", value=Mock(return_value=sentinel.VALUE))
    mock_nav = Mock()
    mock_nav.name = Mock(return_value=mock_nav_name)
    return Mock(name="Workbook", unpacker=Mock(nav=Mock(return_value=mock_nav)))


@pytest.fixture
def mock_schema():
    return Mock(name="Schema")


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


def test_builder_1(mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    args = Mock(layout="1")
    b_1 = demo.app.builder_factory(args)
    inter_1 = b_1(mock_row)
    assert inter_1 == {"key": sentinel.VALUE, "value": sentinel.VALUE}
    assert mock_row.nav.name.mock_calls == [
        call('Column "3" - string'),
        call("Col 2.0 - float"),
    ]


def test_builder_2(mock_sheet_row):
    mock_sheet, mock_row = mock_sheet_row
    args = Mock(layout="2")
    b_2 = demo.app.builder_factory(args)
    inter_2 = b_2(mock_row)
    assert inter_2 == {"key": sentinel.VALUE, "value": sentinel.VALUE}
    assert mock_row.nav.name.mock_calls == [call("Column 3"), call("Column 2")]


@pytest.fixture
def mock_builder():
    return Mock(return_value={"key": sentinel.KEY, "value": sentinel.VALUE})


@pytest.fixture
def mock_persistence():
    return Mock()


def test_process_sheet(mock_builder, mock_sheet_row, mock_persistence):
    mock_sheet, mock_row = mock_sheet_row
    counts = demo.app.process_sheet(
        mock_builder,
        mock_sheet,
        mock_persistence,
    )
    assert counts == {"read": 1, "processed": 1}


def test_process_workbook(
    mock_builder, mock_workbook, mock_sheet_row, mock_persistence, caplog
):
    mock_sheet, mock_row = mock_sheet_row
    mock_workbook.sheet_iter = Mock(return_value=[mock_sheet])
    caplog.set_level(logging.INFO)

    demo.app.process_workbook(mock_builder, mock_workbook, mock_persistence)

    assert caplog.record_tuples == [
        ("demo.app", logging.INFO, f"{mock_workbook.name} :: {mock_sheet.name}"),
        ("demo.app", logging.INFO, "{'processed': 1, 'read': 1}"),
    ]


def test_parse_args():
    options = demo.app.parse_args(["-d", "-v", "file.name"])
    assert options.file == [Path("file.name")]
    assert options.dry_run
    assert options.verbosity == logging.DEBUG


@pytest.fixture
def mock_process_workbook(monkeypatch):
    mock_function = Mock(name="Some Workbook")
    monkeypatch.setattr(demo.app, "process_workbook", mock_function)
    return mock_function


@pytest.fixture
def mock_open_workbook(monkeypatch):
    the_workbook = Mock(name="Workbook", return_value=Mock())
    mock_open_workbook = mock_open(mock=the_workbook)
    monkeypatch.setattr(demo.app, "open_workbook", mock_open_workbook)
    return mock_open_workbook


@pytest.fixture
def mock_persistent_processing(monkeypatch):
    the_mode = Mock(name="Peristent_Processing")
    mock_persistent_processing = mock_open(mock=the_mode)
    mock_persistent_processing.__name__ = ("Persistent_Processing class",)
    monkeypatch.setattr(demo.app, "Persistent_Processing", mock_persistent_processing)
    return mock_persistent_processing


def test_main(
    tmp_path, mock_open_workbook, mock_persistent_processing, mock_process_workbook
):
    input_file = tmp_path / "input.csv"
    output_file = tmp_path / "output.csv"

    demo.app.main(["-o", str(output_file), str(input_file)])

    mock_open_workbook.assert_called_once_with(input_file)
    mock_process_workbook.assert_called_once()
