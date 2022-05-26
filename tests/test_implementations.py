"""
Test the implementations module.

These are integration tests. They make sure that *all* of the relevant sample files can be
read.

"""
import datetime
import pytest
from pathlib import Path
import os
from pprint import pprint
from stingray import (
    HeadingRowSchemaLoader,
    CSV_Workbook, JSON_Workbook, COBOL_Text_File,
    XLS_Workbook, XLSX_Workbook, ODS_Workbook, Numbers_Workbook,
    SchemaMaker, CONVERSION, name_cleaner
    )
try:  # pragma: no cover
    from jsonschema import Draft202012Validator as SchemaValidator
except ImportError:
    from jsonschema import Draft7Validator as SchemaValidator  # type: ignore[import]

@pytest.fixture
def csv_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "csv_workbook.csv"

def test_csv_workbook(csv_path, capsys):
    with CSV_Workbook(csv_path) as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['']
        sheet_1 = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "['42', '3.1415926', 'string', '09/10/56', 'TRUE', '', '#DIV/0!']",
            "['9973', '2.7182818', 'data', '01/18/59', 'FALSE', '', '#NAME?']"
        ]


@pytest.fixture
def tab_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "tab_workbook.tab"

def test_tab_workbook(tab_path, capsys):
    with CSV_Workbook(tab_path, delimiter="\t") as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['']
        sheet_1 = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "['42', '3.1415926', 'string', '09/10/56', 'TRUE', '', '#DIV/0!']",
            "['9973', '2.7182818', 'data', '01/18/59', 'FALSE', '', '#NAME?']"
        ]


@pytest.fixture
def xls_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "excel97_workbook.xls"

def test_xls_workbook(xls_path, capsys):
    with XLS_Workbook(xls_path) as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['Sheet1', 'Sheet2', 'Sheet3']
        sheet_1 = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "[42.0, 3.1415926, 'string', 20708.0, 1, '', 7]",
            "[9973.0, 2.7182818280000003, 'data', 21568.0, 0, '', 15]"
        ]


@pytest.fixture
def xlsx_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "excel_workbook.xlsx"

def test_xlsx_workbook(xlsx_path, capsys):
    with XLSX_Workbook(xlsx_path) as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['Sheet1', 'Sheet2', 'Sheet3']
        sheet_1 = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "[42, 3.1415926, 'string', datetime.datetime(1956, 9, 10, 0, 0), True, None, '=2/0']",
            "[9973, 2.7182818280000003, 'data', datetime.datetime(1959, 1, 18, 0, 0), False, None, '=2+\"\\'a\\'\"']"
        ]


@pytest.fixture
def ods_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "ooo_workbook.ods"

def test_ods_workbook(ods_path, capsys):
    with ODS_Workbook(ods_path) as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['Sheet1', 'Sheet2', 'Sheet3']
        sheet_1 = wb.sheet("Sheet1").set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "[42, 3.1415926, 'string', datetime.date(1956, 9, 10), True, '', 0]",
            "[9973, 2.718281828, 'data', datetime.date(1959, 1, 18), False, '', 0]"
        ]


@pytest.fixture
def numbers_path():
    return Path(os.environ.get("SAMPLES", "sample")) / "numbers_workbook_13.numbers"

def test_numbers_workbook(numbers_path, capsys):
    with Numbers_Workbook(numbers_path) as wb:
        assert list(s.name for s in wb.sheet_iter()) == ['Sheet 1::Table 1', 'Sheet 2::Table 1', 'Sheet 3::Table 1']
        sheet_1 = wb.sheet('Sheet 1::Table 1').set_schema_loader(HeadingRowSchemaLoader())
        for row in sheet_1.row_iter():
            print(row)
        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "[42.0, 3.1415926, 'string', datetime.datetime(1956, 9, 10, 0, 0), True, None, None]",
            "[9973.0, 2.7182818, 'data', datetime.datetime(1959, 1, 18, 0, 0), False, None, None]"
        ]

@pytest.fixture
def fixed_text_paths():
    samples = Path(os.environ.get("SAMPLES", "sample"))
    return samples / "workbook.simple", samples / "simple.csv"

def test_fixed_text_file_with_schema(fixed_text_paths, capsys, monkeypatch):
    workbook_path, schema_path = fixed_text_paths
    monkeypatch.setitem(CONVERSION, "datetime", lambda x: datetime.datetime.strptime(x.strip(), '%m/%d/%y'))

    # Step 1. Load a workbook sheet and create a JSON schema.
    type_map = {
        "int": {"type": "integer"},
        "float": {"type": "number"},
        "str": {"type": "string"},
        "datetime": {"type": "string", "conversion": "datetime"},
        "bool": {"type": "boolean"},
    }

    with CSV_Workbook(schema_path) as schema_wb:
        sheet = schema_wb.sheet("")
        sheet.set_schema_loader(HeadingRowSchemaLoader())
        json_schema = {
            "title": "simple",
            "description": "Schema for files with extension of .simple",
            "type": "object",
            "properties": {
                row.name("name").value(): {
                    "$anchor": name_cleaner(row.name("name").value()),
                    "offset": int(row.name("offset").value()),
                    "maxLength": int(row.name("size").value()),
                } | type_map[row.name("type").value()]
                for row in sheet.rows()
            }
        }

    assert SchemaValidator.check_schema(json_schema) is None

    # Step 2. Apply the schema to the data.
    with COBOL_Text_File(workbook_path) as wb:
        sheet = wb.sheet("").set_schema(SchemaMaker.from_json(json_schema))
        rows = sheet.rows()
        header = next(rows)
        assert header.instance == ('Col 1 - intCol 2.0 - fColumn "3" Column \'4\' Column 5 - Column 6 - Column 7 - \n')
        for row in rows:
            print(row)

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "[42, 3.1415926, 'string     ', datetime.datetime(2056, 9, 10, 0, 0), '       TRUE', '           ', '#DIV/0!    ']",
        "[9973, 2.7182818, 'data       ', datetime.datetime(2059, 1, 18, 0, 0), '      FALSE', '           ', '#NAME?     ']"
    ]
