"""
Workbook Test Cases
"""
import pytest
from unittest.mock import Mock, MagicMock, call, sentinel
import os
import re
from unittest.mock import Mock, MagicMock
from io import StringIO, BytesIO
from stingray.workbook import *

### Base Workbook/Sheet/Row Features

@pytest.fixture
def mock_unpacker():
    sub_nav = Mock(value=Mock(side_effect=[sentinel.V1, sentinel.V2]))
    nav = Mock()
    nav.name = Mock(return_value=sub_nav)
    unpacker = Mock(
        name="Unpacker",
        nav=Mock(return_value=nav),
        sheet_iter=Mock(return_value=[sentinel.SHEET]),
        instance_iter=Mock(return_value=[sentinel.ROW]),
    )
    return unpacker

@pytest.fixture
def mock_workbook_class(mock_unpacker):
    class WB(Workbook):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.unpacker = mock_unpacker

    return WB

def test_base_workbook(mock_workbook_class):
    with mock_workbook_class(Path("test")) as wb:
        assert wb.sheet("name") == Sheet(wb, "name")
        assert list(wb.sheet_iter()) == [Sheet(wb, sentinel.SHEET)]
        assert wb == wb

def test_external_schema_base_sheet(mock_unpacker, mock_workbook_class):
    schema = Mock(name="Schema", properties=["a", "b"])
    with mock_workbook_class(Path("test")) as wb:
        sheet = Sheet(wb, sentinel.NAME).set_schema(schema)
        assert sheet.name == sentinel.NAME
        assert sheet.schema == schema
        for row in sheet.row_iter():
            assert row.instance == sentinel.ROW
            assert row.schema == schema
            assert row.name("ITEM") == mock_unpacker.nav().name()
            assert row.sheet().workbook().unpacker.nav.mock_calls == [call(schema, sentinel.ROW), call()]
            assert row.get("ITEM") == mock_unpacker.nav().name()
            assert row["ITEM"] == mock_unpacker.nav().name()
            assert row == row
            assert row.values() == [sentinel.V1, sentinel.V2]

def test_repr_external_schema_base_sheet(mock_unpacker, mock_workbook_class):
    schema = Mock(name="Schema", properties=["a", "b"])
    with mock_workbook_class(Path("test")) as wb:
        sheet = Sheet(wb, sentinel.NAME).set_schema(schema)
        for row in sheet.row_iter():
            assert row.sheet() == sheet
            assert row.instance == sentinel.ROW

def test_str_external_schema_base_sheet(mock_unpacker, mock_workbook_class):
    schema = Mock(name="Schema", properties=["a", "b"])
    with mock_workbook_class(Path("test")) as wb:
        sheet = Sheet(wb, sentinel.NAME).set_schema(schema)
        for row in sheet.row_iter():
            assert str(row) == '[sentinel.V1, sentinel.V2]'

### Test CSV Workbook

@pytest.fixture
def anscombe_schema() -> Schema:
    json_schema = {
        "title": "spike/Anscombe_quartet_data.csv",
        "description": "Four series, use (x123, y1), (x123, y2), (x123, y3) or (x4, y4)",
        "type": "object",
        "properties": {
            "x123": {
                "title": "x123",
                "type": "number",
                "description": "X values for series 1, 2, and 3.",
            },
            "y1": {"title": "y1", "type": "number", "description": "Y value for series 1."},
            "y2": {"title": "y2", "type": "number", "description": "Y value for series 2."},
            "y3": {"title": "y3", "type": "number", "description": "Y value for series 3."},
            "x4": {"title": "x4", "type": "number", "description": "X value for series 4."},
            "y4": {"title": "y4", "type": "number", "description": "Y value for series 4."},
        },
    }
    assert SchemaValidator.check_schema(json_schema) is None
    return SchemaMaker.from_json(json_schema)

@pytest.fixture
def csv_test_file_1() -> Path:
    return Path(os.environ.get("SAMPLES", "samples")) / "Anscombe_quartet_data.csv"

@pytest.fixture
def csv_workbook_1(csv_test_file_1) -> CSV_Workbook:
    return CSV_Workbook(csv_test_file_1)

def test_csv_workbook(csv_workbook_1, anscombe_schema, capsys) -> bool:
    assert len(list(csv_workbook_1.sheet_iter())) == 1
    sheet = csv_workbook_1.sheet("").set_schema(anscombe_schema)
    rows = sheet.row_iter()
    # NOTE: The file has a header row, which we're carefully extracting.
    row_0 = next(rows)
    assert row_0.values() == ['x123', 'y1', 'y2', 'y3', 'x4', 'y4']
    assert re.fullmatch(
        r"Row\(Sheet\(\<stingray.workbook.CSV_Workbook object at .+?\>, '', schema=ObjectSchema\(.+?\), loader=\<stingray.workbook.SchemaLoader object at .+?\>\), \['x123', 'y1', 'y2', 'y3', 'x4', 'y4'\]\)",
        repr(row_0)
    )
    # Non-header rows.
    pairs = [(row.name("x123").value(), row.name("y1").value()) for row in rows]
    assert pairs == [
        ('10.0', '8.04'),
        ('8.0', '6.95'),
        ('13.0', '7.58'),
        ('9.0', '8.81'),
        ('11.0', '8.33'),
        ('14.0', '9.96'),
        ('6.0', '7.24'),
        ('4.0', '4.26'),
        ('12.0', '10.84'),
        ('7.0', '4.82'),
        ('5.0', '5.68')
    ]

def test_csv_workbook_row(csv_workbook_1, anscombe_schema, capsys) -> bool:
    sheet = csv_workbook_1.sheet("").set_schema(anscombe_schema)
    rows = sheet.row_iter()
    row_0 = next(rows)
    row_1 = next(rows)
    row_1.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                                 Value',
         'spike/Anscombe_quartet_data.csv',
         "  x123                                                '10.0'",
         "  y1                                                  '8.04'",
         "  y2                                                  '9.14'",
         "  y3                                                  '7.46'",
         "  x4                                                  '8.0'",
         "  y4                                                  '6.58'"
    ]

def test_csv_open_file(csv_test_file_1):
    with csv_test_file_1.open() as open_file:
        wb = CSV_Workbook(csv_test_file_1, open_file)
        assert len(list(wb.sheet_iter())) == 1
        assert wb.name == csv_test_file_1
    assert wb.unpacker.the_file.closed

### open_workbook function tests

def test_bad_csv_open_workbook(tmp_path) -> bool:
    does_not_exist = tmp_path/"must.*fail*"
    with pytest.raises(NotImplementedError):
        open_workbook(does_not_exist)

def test_good_csv_open_workbook(csv_test_file_1) -> bool:
    with open_workbook(csv_test_file_1) as wb:
        assert isinstance(wb, CSV_Workbook)

### Test JSON Workbook

@pytest.fixture
def json_test_file_1() -> Path:
    return Path(os.environ.get("SAMPLES", "samples")) / "Anscombe_quartet_data.ndjson"

@pytest.fixture
def json_workbook_1(json_test_file_1) -> CSV_Workbook:
    return JSON_Workbook(json_test_file_1)


def test_json_workbook(json_workbook_1, anscombe_schema, capsys) -> bool:
    assert len(list(json_workbook_1.sheet_iter())) == 1
    sheet = json_workbook_1.sheet("").set_schema(anscombe_schema)
    pairs = [(row.name("x123").value(), row.name("y1").value()) for row in sheet.row_iter()]
    assert pairs == [
        (10.0, 8.04),
        (8.0, 6.95),
        (13.0, 7.58),
        (9.0, 8.81),
        (11.0, 8.33),
        (14.0, 9.96),
        (6.0, 7.24),
        (4.0, 4.26),
        (12.0, 10.84),
        (7.0, 4.82),
        (5.0, 5.68)
    ]

def test_json_open_file(json_test_file_1):
    with json_test_file_1.open() as open_file:
        wb = JSON_Workbook(json_test_file_1, open_file)
        assert len(list(wb.sheet_iter())) == 1
        assert wb.name == json_test_file_1
    assert wb.unpacker.the_file.closed

def test_good_json_open_workbook(json_test_file_1) -> bool:
    with open_workbook(json_test_file_1) as wb:
        # print(wb)
        assert isinstance(wb, JSON_Workbook)

### Test Schema Loaders

@pytest.fixture
def heading_row_workbook() -> Mock:
    unpacker = Mock(
        name="Mock CSVUnpacker",
        instance_iter=Mock(name="CSVUnpacker.instance_iter", return_value=iter([("a", "b"), ("1", "2")])),
    )
    unpacker.nav = Mock(side_effect=lambda schema, instance: WBNav(unpacker, schema, instance))
    wb = Mock(
        name="mock Workbook",
        unpacker=unpacker,
        kwargs={},
    )
    wb.sheet = Mock(
        return_value=Sheet(wb, "mock"),
    )
    return wb

def test_heading_row_schema_loader(heading_row_workbook):
    sheet = heading_row_workbook.sheet('Sheet1')
    sheet.set_schema_loader(HeadingRowSchemaLoader())
    rows = list(sheet.rows())
    assert rows == [
        Row(sheet, ('1', '2'))
    ]
    assert rows[0].name("a").value() == '1'
    assert rows[0].name("b").value() == '2'


def test_integration_csv_workbook_schema_loader(csv_workbook_1, capsys):
    sheet = csv_workbook_1.sheet('Sheet1')
    sheet.set_schema_loader(HeadingRowSchemaLoader())
    for row in sheet.rows():
        print(row)
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "['10.0', '8.04', '9.14', '7.46', '8.0', '6.58']",
        "['8.0', '6.95', '8.14', '6.77', '8.0', '5.76']",
        "['13.0', '7.58', '8.74', '12.74', '8.0', '7.71']",
        "['9.0', '8.81', '8.77', '7.11', '8.0', '8.84']",
        "['11.0', '8.33', '9.26', '7.81', '8.0', '8.47']",
        "['14.0', '9.96', '8.10', '8.84', '8.0', '7.04']",
        "['6.0', '7.24', '6.13', '6.08', '8.0', '5.25']",
        "['4.0', '4.26', '3.10', '5.39', '19.0', '12.50']",
        "['12.0', '10.84', '9.13', '8.15', '8.0', '5.56']",
        "['7.0', '4.82', '7.26', '6.42', '8.0', '7.91']",
        "['5.0', '5.68', '4.74', '5.73', '8.0', '6.89']"
    ]
    # Last row...
    assert row.name("x123").value() == '5.0'
    assert row.name("y1").value() == '5.68'
    assert row.name("y2").value() == '4.74'
    assert row.name("y3").value() == '5.73'
    assert row.name("x4").value() == '8.0'
    assert row.name("y4").value() == '6.89'


@pytest.fixture
def external_schema_workbook() -> Mock:
    unpacker = Mock(
        name="Mock CSVUnpacker",
        instance_iter = Mock(return_value=iter([("1", "2")])),
    )
    unpacker.nav = Mock(side_effect=lambda schema, instance: WBNav(unpacker, schema, instance))
    wb = Mock(
        name="mock Workbook",
        unpacker=unpacker,
        kwargs={},
    )
    wb.sheet = Mock(
        return_value = Sheet(wb, "mock"),
    )
    return wb

@pytest.fixture
def metaschema_workbook() -> Mock:
    unpacker = Mock(
        name="Mock CSVUnpacker",
        instance_iter=Mock(return_value=iter([("a", "first column", "number"), ("b", "second", "number")])),
    )
    unpacker.nav = Mock(side_effect=lambda schema, instance: WBNav(unpacker, schema, instance))
    schema_wb = Mock(
        name="mock schema workbook",
        unpacker=unpacker,
        kwargs={},
    )
    schema_wb.sheet = Mock(
        return_value = Sheet(schema_wb, "mock"),
    )
    return schema_wb

def test_external_schema_loader(external_schema_workbook, metaschema_workbook):
    schema_sheet = metaschema_workbook.sheet('Sheet1')
    schema_sheet.set_schema(SchemaMaker().from_json(ExternalSchemaLoader.META_SCHEMA))

    json_schema = ExternalSchemaLoader(schema_sheet).load()
    # print(json_schema)
    assert list(json_schema["properties"].keys()) == ["a", "b"]
    schema = SchemaMaker().from_json(json_schema)

    sheet = external_schema_workbook.sheet('Sheet1')
    sheet.set_schema(schema)
    rows = list(sheet.rows())
    assert rows == [
        Row(sheet, ('1', '2'))
    ]
    assert rows[0].name("a").value() == '1'
    assert rows[0].name("b").value() == '2'

def test_cobol_schema_loader():
    source_path = Path(os.environ.get("SAMPLES", "sample")) / "EPSPDATA.cpy"
    schema = COBOLSchemaLoader(source_path).load()
    assert schema["title"] == "EPSPDATA"


### Test COBOL "workbook"


@pytest.fixture
def cobol_case_1(tmp_path) -> tuple[Schema, COBOL_Text_File]:
    """
    COBOL File Access Case 1: Text

        01  FILE-ACCESS-1.
            05  WORD PIC X(3).
            05  NUMBER-1 PIC 999.99.
            05  NUMBER-2 PIC 999V99.

    File content: `b"ASC123.4567890XYZ"`
    """
    text_file_schema = {
        "title": "FILE-ACCESS-1",
        "$anchor": "FILE-ACCESS-1",
        "cobol": "01  FILE-ACCESS-1",
        "type": "object",
        "properties": {
            "WORD": {
                "type": "string",
                "cobol": "05  WORD PIC X(3)",
                "minLength": 3,
                "maxLength": 3,
            },
            "NUMBER-1": {
                "type": "string",
                "cobol": "05  WORD PIC 999.99",
                "minLength": 6,
                "maxLength": 6,
            },
            "NUMBER-2": {
                "type": "string",
                "cobol": "05  WORD PIC 999V99",
                "minLength": 5,
                "maxLength": 5,
            },
        },
    }
    assert SchemaValidator.check_schema(text_file_schema) is None

    file_path_1 = tmp_path/"file_content_1.data"
    file_path_1.write_text("ASC123.4567890XYZ")

    return SchemaMaker.from_json(text_file_schema), file_path_1


def test_cobol_case_1(cobol_case_1, capsys) -> bool:
    text_file_schema, text_cobol_path = cobol_case_1
    with text_cobol_path.open() as file_1:
        text_cobol_file = COBOL_Text_File(text_cobol_path, file_1)
        assert len(list(text_cobol_file.sheet_iter())) == 1
        sheet = text_cobol_file.sheet("").set_schema(text_file_schema)
        for row in sheet.row_iter():
            print(row)

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "['ASC', '123.45', '67890']"
    ]

    with COBOL_Text_File(text_cobol_path) as text_cobol_file:
        assert len(list(text_cobol_file.sheet_iter())) == 1

    with COBOL_Text_File(str(text_cobol_path)) as text_cobol_file:
        assert len(list(text_cobol_file.sheet_iter())) == 1




@pytest.fixture
def cobol_schema_2() -> Schema:
    """
    COBOL File Access Case 2: Simple EBCDIC

        01  FILE-ACCESS-2.
            05  WORD PIC XXX.
            05  NUMBER-1 PIC 999.99.
            05  NUMBER-2 PIC S999V99.
            05  NUMBER-3 PIC S99999999 USAGE COMPUTATIONAL.
            05  NUMBER-4 PIC S999V99 USAGE COMPUTATIONAL-3.
    """

    # Important: minLength and maxLength based on contentEncoding and picture.
    ebcdic_file_schema = {
        "title": "ebcdic file schema",
        "type": "object",
        "cobol": "01  FILE-ACCESS-2",
        "description": "21 byte lrecl with a mix of data types",
        "properties": {
            "WORD": {
                "type": "string",
                "cobol": "05  WORD PIC XXX",
                "maxLength": 3,
                "minLength": 3,
                "contentEncoding": "cp037",
            },
            "NUMBER-1": {
                "type": "string",
                "cobol": "05  WORD PIC 999.99",
                "maxLength": 6,
                "minLength": 6,
                "contentEncoding": "cp037",
            },
            "NUMBER-2": {
                "type": "string",
                "cobol": "05  WORD PIC 999V99",
                "maxLength": 5,
                "minLength": 5,
                "contentEncoding": "cp037",
                "conversion": "decimal",
            },
            "NUMBER-3": {
                "type": "string",
                "cobol": "05  WORD PIC S99999999 USAGE COMPUTATIONAL",
                "maxLength": 4,
                "minLength": 4,
                "contentEncoding": "bigendian-int",
                "conversion": "integer",
            },
            "NUMBER-4": {
                "type": "string",
                "cobol": "05  WORD PIC S999V99 USAGE COMPUTATIONAL-3",
                "maxLength": 4,
                "minLength": 4,
                "contentEncoding": "bigendian-float",
                "conversion": "number",
            },
        },
    }
    assert SchemaValidator.check_schema(ebcdic_file_schema) is None
    return SchemaMaker.from_json(ebcdic_file_schema)


@pytest.fixture
def cobol_schema_2_path_2(tmp_path) -> COBOL_EBCDIC_File:
    """COBOL File Access Case 2: EBCDIC with RECFM=F"""
    file_path_2 = tmp_path / "file_content_2.data"
    file_path_2.write_bytes(
        b"\xe9\xd6\xe2"  # WORD == "ZOS"
        b"\xf1\xf2\xf3K\xf4\xf5"  # NUMBER-1 == "123.45"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2 == Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3 == 4660
        b"\x98\x76\x5d"  # NUMBER-4 == -987.65
    )
    return file_path_2


def test_cobol_case_2_file_2(cobol_schema_2, cobol_schema_2_path_2, capsys) -> bool:
    with COBOL_EBCDIC_File(cobol_schema_2_path_2) as ebcdic_cobol_file:
        assert len(list(ebcdic_cobol_file.sheet_iter())) == 1
        sheet = ebcdic_cobol_file.sheet("").set_schema(cobol_schema_2)

        for row in sheet.row_iter():
            print(row)

        out, err = capsys.readouterr()
        assert out.splitlines() == [
            "['ZOS', '123.45', Decimal('678.90'), 4660, -987.65]"
        ]

    assert sheet.lrecl == 21

    with cobol_schema_2_path_2.open('rb') as file_2:
        COBOL_EBCDIC_File(cobol_schema_2_path_2, file_2)
        assert len(list(ebcdic_cobol_file.sheet_iter())) == 1


@pytest.fixture
def cobol_schema_2_path_3(tmp_path) -> COBOL_EBCDIC_File:
    """
    COBOL File Access Case 3: EBCDIC with RECFM=V
    Sample data has Record Descriptor Word (RDW) added for RECFM=V
    """
    file_path_3 = tmp_path/"file_content_3.data"
    file_path_3.write_bytes(
        b"\x00\x19\x00\x00"  # RDW for 0x19 = 24 bytes, 4 byte header plus 21 bytes of data.
        b"\xe9\xd6\xe2"  # WORD == "ZOS"
        b"\xf1\xf2\xf3K\xf4\xf5"  # NUMBER-1 == "123.45"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2 == Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3 == 4660
        b"\x98\x76\x5d"  # NUMBER-4 == -987.65
    )
    return file_path_3

def test_cobol_case_2_file_3(cobol_schema_2, cobol_schema_2_path_3, capsys) -> bool:
    with COBOL_EBCDIC_File(cobol_schema_2_path_3, recfm_class=estruct.RECFM_V) as ebcdic_cobol_file:
        sheet = ebcdic_cobol_file.sheet("").set_schema(cobol_schema_2)

        lrecl = (
            LocationMaker(EBCDIC(), cobol_schema_2)
            .from_schema()
            .end
        )
        assert lrecl == 21

        for row in sheet.row_iter():
            print(row)

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "['ZOS', '123.45', Decimal('678.90'), 4660, -987.65]"
    ]


@pytest.fixture
def cobol_schema_2_path_4(tmp_path) -> COBOL_EBCDIC_File:
    """
    COBOL File Access Case 4: EBCDIC with RECFM=VB
    Sample data has Block Descriptor Word (BDW) and Record Descriptor Word (RDW) added for RECFM=VB
    """

    file_path_4 = tmp_path / "file_content_4.data"
    file_path_4.write_bytes(
        b"\x00\x36\x00\x00"  # BDW: 54 = 4+sum of RDW's
        b"\x00\x19\x00\x00"  # RDW: 25=4+len(data)
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf5"  # NUMBER-1="123.45"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
        b"\x00\x19\x00\x00"  # RDW
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf6"  # NUMBER-1="123.46"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
        b"\x00\x36\x00\x00"  # BDW
        b"\x00\x19\x00\x00"  # RDW
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf7"  # NUMBER-1="123.47"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
        b"\x00\x19\x00\x00"  # RDW
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf8"  # NUMBER-1="123.48"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
    )

    return file_path_4


def test_cobol_case_2_file_4(cobol_schema_2, cobol_schema_2_path_4, capsys) -> bool:
    cobol_ebcdic_file = COBOL_EBCDIC_File(cobol_schema_2_path_4, recfm_class=estruct.RECFM_VB, lrecl=21)
    sheet = cobol_ebcdic_file.sheet("").set_schema(cobol_schema_2)

    for row in sheet.row_iter():
        print(row)

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "['ZOS', '123.45', Decimal('678.90'), 4660, -987.65]",
        "['ZOS', '123.46', Decimal('678.90'), 4660, -987.65]",
        "['ZOS', '123.47', Decimal('678.90'), 4660, -987.65]",
        "['ZOS', '123.48', Decimal('678.90'), 4660, -987.65]"
    ]


@pytest.fixture
def cobol_schema_2_file_5(tmp_path) -> COBOL_EBCDIC_File:
    """
    COBOL File Access Case 5: EBCDIC without RECFM information.

    This is essentially RECFM=V, but without RDW. The size comes from the ``Location`` and is computed dynamically.
    This is *not* typical of COBOL. But. We can support it.
    """
    file_path_5 = tmp_path/"file_content_5.data"

    file_path_5.write_bytes(
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf5"  # NUMBER-1="123.45"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
        b"\xe9\xd6\xe2"  # WORD="ZOS"
        b"\xf1\xf2\xf3K\xf4\xf5"  # NUMBER-1="123.45"
        b"\xf6\xf7\xf8\xf9\xf0"  # NUMBER-2=Decimal("678.90")
        b"\x00\x00\x12\x34"  # NUMBER-3=4660
        b"\x98\x76\x5d"  # NUMBER-4=-987.65
    )
    return file_path_5


def test_cobol_case_2_file_5(cobol_schema_2, cobol_schema_2_file_5, capsys) -> bool:
    with COBOL_EBCDIC_File(cobol_schema_2_file_5) as ebcdic_file:
        sheet = ebcdic_file.sheet("").set_schema(cobol_schema_2)
        for row in sheet.row_iter():
            print(row)

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "['ZOS', '123.45', Decimal('678.90'), 4660, -987.65]",
        "['ZOS', '123.45', Decimal('678.90'), 4660, -987.65]"
    ]


@pytest.fixture
def cobol_schema_2_file_6(tmp_path) -> COBOL_EBCDIC_File:
    """
    COBOL File Access Case 6: EBCDIC bad COMP-3 data.

    See Case 2 for the schema used here.
    """
    file_path_6 = tmp_path / "file_content_6.data"
    file_path_6.write_bytes(
        b"\xe9\xd6\xe2\xf1\xd6\xf3K\xf4\xd6\xf6\xf7\xf8\xd6\xf0\x00\x00\x12\xd6\x00\x00\x00\x00\x00"
    )
    return file_path_6


def test_cobol_case_2_file_5(cobol_schema_2, cobol_schema_2_file_6, capsys) -> bool:
    with COBOL_EBCDIC_File(cobol_schema_2_file_6) as ebcdic_file:
        assert len(list(ebcdic_file.sheet_iter())) == 1
        sheet = ebcdic_file.sheet("").set_schema(cobol_schema_2)

        assert sheet.lrecl == 21

        row = next(sheet.row_iter())
        assert row.name("WORD").value() == "ZOS"

        assert row.name("NUMBER-1").raw() == b"\xf1\xd6\xf3K\xf4\xd6"
        with pytest.raises(ValueError):
            # Note letter "O" not zero "0" -- Doesn't match pattern of 999.99. Should raise exception...
            v = row.name("NUMBER-1").value()

        assert row.name("NUMBER-2").raw() == b"\xf6\xf7\xf8\xd6\xf0"
        assert row.name("NUMBER-2").value() == Decimal('678.60')

        assert row.name("NUMBER-3").raw() == b"\x00\x00\x12\xd6"
        assert row.name("NUMBER-3").value() == Decimal("4822")

        # What about low values? Should this raise an exception? Or is it valid Z/OS behavior?
        assert row.name("NUMBER-4").raw() == b"\x00\x00\x00"
        assert row.name("NUMBER-4").value() == Decimal("0")
