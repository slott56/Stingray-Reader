"""
Test the implementations module.
"""
import pytest
from pathlib import Path
import os
from stingray import HeadingRowSchemaLoader, XLS_Workbook, XLSX_Workbook, ODS_Workbook

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

