"""
estruct test cases.
"""

import pytest
from decimal import Decimal
from io import BytesIO
from stingray.estruct import Representation, unpack, calcsize, RECFM_N, RECFM_F, RECFM_V, RECFM_VB

def test_display_text_1():
    rep = Representation.parse("USAGE DISPLAY PICTURE S9(5).99")
    assert rep == Representation(
        usage="DISPLAY",
        picture_elements=[
            {"sign": "S"},
            {"digit": "99999"},
            {"decimal": "."},
            {"digit": "99"},
        ],
        picture_size=9,
    )
    assert rep.pattern == r"[ +-]?\d\d\d\d\d\.\d\d"
    assert rep.digit_groups == ["S", "99999", ".", "99"]

def test_display_text_2():
    rep = Representation.parse("USAGE DISPLAY PICTURE X(7)")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'digit': 'XXXXXXX'}], picture_size=7)
    assert rep.pattern == '.......'
    assert rep.digit_groups == ["", "XXXXXXX", "", ""]

def test_display_text_3():
    rep = Representation.parse("USAGE DISPLAY PICTURE ZZ")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'digit': 'ZZ'}], picture_size=2)
    assert rep.pattern == '\\d\\d'
    assert rep.digit_groups == ["", "ZZ", "", ""]

def test_display_text_4():
    rep = Representation.parse("USAGE DISPLAY PICTURE 999")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'digit': '999'}], picture_size=3)
    assert rep.pattern == '\\d\\d\\d'
    assert rep.digit_groups == ["", "999", "", ""]

def test_display_text_5():
    rep = Representation.parse("USAGE DISPLAY PICTURE 9(5)V99")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'digit': '99999'}, {'decimal': 'V'}, {'digit': '99'}], picture_size=7)
    assert rep.pattern == '\\d\\d\\d\\d\\d\\d\\d'
    assert rep.digit_groups == ["", "99999", "V", "99"]

def test_display_text_6():
    rep = Representation.parse("USAGE DISPLAY PICTURE S9(7)V99")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'digit': '9999999'}, {'decimal': 'V'}, {'digit': '99'}], picture_size=10)
    assert rep.pattern == '[ +-]?\\d\\d\\d\\d\\d\\d\\d\\d\\d'
    assert rep.digit_groups == ["S", "9999999", "V", "99"]

def test_display_text_7():
    rep = Representation.parse("USAGE DISPLAY PICTURE DB9(5).99")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'sign': 'DB'}, {'digit': '99999'}, {'decimal': '.'}, {'digit': '99'}], picture_size=10)
    assert rep.pattern == 'DB\\d\\d\\d\\d\\d\\.\\d\\d'
    assert rep.digit_groups == ["DB", "99999", ".", "99"]

def test_display_text_8():
    rep = Representation.parse("USAGE DISPLAY PICTURE S9(4)V")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'digit': '9999'}, {'decimal': 'V'}], picture_size=5)
    assert rep.pattern == '[ +-]?\\d\\d\\d\\d'
    assert rep.digit_groups == ["S", "9999", "V", ""]
    
def test_display_text_9():
    rep = Representation.parse("USAGE DISPLAY PICTURE 9(6).9(3)")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'digit': '999999'}, {'decimal': '.'}, {'digit': '999'}], picture_size=10)
    assert rep.pattern == '\\d\\d\\d\\d\\d\\d\\.\\d\\d\\d'
    assert rep.digit_groups == ["", "999999", ".", "999"]

def test_display_text_10():
    rep = Representation.parse("USAGE DISPLAY PICTURE SV9(5)")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'decimal': 'V'}, {'digit': '99999'}], picture_size=6)
    assert rep.pattern == '[ +-]?\\d\\d\\d\\d\\d'
    assert rep.digit_groups == ["S", "", "V", "99999"]

def test_display_text_11():
    rep = Representation.parse("USAGE DISPLAY PICTURE SV9(05)")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'sign': 'S'}, {'decimal': 'V'}, {'digit': '99999'}], picture_size=6)
    assert rep.pattern == '[ +-]?\\d\\d\\d\\d\\d'
    assert rep.digit_groups == ["S", "", "V", "99999"]

def test_display_text_12():
    rep = Representation.parse("USAGE DISPLAY PICTURE $***9.99")
    assert rep == Representation(usage='DISPLAY', picture_elements=[{'char': '$'}, {'char': '*'}, {'char': '*'}, {'char': '*'}, {'digit': '9'}, {'decimal': '.'}, {'digit': '99'}], picture_size=8)
    assert rep.pattern == '\$\*\*\*\d\.\d\d'
    assert rep.digit_groups == ["", "9999", ".", "99"]

def test_display_zoned_decimal():
    rep = Representation.parse("05 FIELD-1 USAGE IS DISPLAY PIC S9(5)V99")
    assert rep == Representation(
        usage="DISPLAY",
        picture_elements=[
            {"sign": "S"},
            {"digit": "99999"},
            {"decimal": "V"},
            {"digit": "99"},
        ],
        picture_size=8,
    )
    assert rep.pattern == r"[ +-]?\d\d\d\d\d\d\d"
    assert rep.digit_groups == ["S", "99999", "V", "99"]


def test_calcsize_display():
    assert calcsize("USAGE DISPLAY PICTURE S9(5)V99") == 8


def test_unpack_display():
    assert unpack(
        "USAGE DISPLAY PICTURE S9(5)V99", "1234567".encode("CP037")
    ) == (Decimal("12345.67"),)


def test_calcsize_comp():
    assert calcsize("USAGE COMP PIC 999") == 2
    assert calcsize("USAGE COMPUTATIONAL PIC 9999") == 2
    assert calcsize("USAGE COMP PIC 99999") == 4

def test_calcsize_comp1():
    assert calcsize("USAGE COMP-1 PIC 999") == 4
    assert calcsize("USAGE COMPUTATIONAL-1 PIC 9999") == 4
    assert calcsize("USAGE COMP-1 PIC 99999") == 4

def test_calcsize_comp2():
    assert calcsize("USAGE COMP-2 PIC 999") == 8
    assert calcsize("USAGE COMPUTATIONAL-2 PIC 9999") == 8
    assert calcsize("USAGE COMP-2 PIC 99999") == 8

def test_calcsize_comp3():
    assert calcsize("USAGE COMP-3 PIC S9(7)V99") == 5
    assert calcsize("USAGE COMPUTATIONAL-3 PIC S9(7)V99") == 5
    assert calcsize("USAGE COMP-3 PIC 9999999") == 4

def test_unpack_comp3():
    assert unpack(
        "USAGE COMP-3 PIC S9(7)V99", bytes([0x12, 0x34, 0x56, 0x79, 0x9C])
    ) == (Decimal("1234567.99"),)
    assert unpack(
        "USAGE COMP-3 PIC SV9(5)", bytes([0x12, 0x34, 0x5C])
    ) == (Decimal("0.12345"),)
    assert unpack(
        "USAGE COMP-3 PIC SV9(5)", bytes([0x98, 0x76, 0x5D])
    ) == (Decimal("-0.98765"),)
    assert unpack(
        "USAGE COMPUTATIONAL PIC S99", bytes([0x12, 0x34])
    ) == (0x1234,)
    assert unpack(
        "USAGE COMPUTATIONAL PIC S99999999999", bytes([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0])
    ) == (0x123456789abcdef0,)



def test_should_convert_display():
    assert unpack("PIC 999V99", b"\xf1\xf2\xf3\xf4\xf5") == (Decimal("123.45"),)
    assert unpack("PIC 99999V", b"\xf1\xf2\xf3\xf4\xf5") == (Decimal("12345"),)
    assert unpack("PIC 999.99", b"\xf1\xf2\xf3K\xf4\xf5") == ("123.45",)


def test_should_convert_zoned_decimal():
    assert unpack("USAGE DISPLAY PIC 9", b"\xc4") == (Decimal("4"),)
    assert unpack("USAGE DISPLAY PIC 9", b"\xd4") == (Decimal("-4"),)
    assert unpack("USAGE DISPLAY PIC 99999V", b"\xf1\xf2\xf3\xf4\xf5") == (Decimal("12345"),)


def test_should_convert_comp3():
    assert unpack(
        "USAGE COMP-3 PIC S999V99", b"\x12\x34\x98\x76\x5d"
    ) == (Decimal("-1234987.65"),)
    assert unpack(
        "USAGE COMP-3 PIC S99999V", b"\x12\x34\x98\x76\x5d"
    ) == (Decimal("-123498765"),)

def test_bad_pictures():
    with pytest.raises(ValueError) as exc_info:
        calcsize("USAGE COMP-3 PICTURE S9(5)V99NOPE")
    assert exc_info.type == ValueError
    assert exc_info.value.args[0] == "Invalid characters 'NOPE' in PIC 'S9(5)V99NOPE'"
    with pytest.raises(ValueError) as exc_info:
        calcsize("USAGE COMP-3")
    assert exc_info.type == ValueError
    assert exc_info.value.args[0] == "Problem parsing 'USAGE COMP-3'"

def test_picture_mask():
    r = Representation.parse("USAGE DISPLAY PICTURE 999-99-9999")
    assert r.usage == "DISPLAY"
    assert r.picture_elements == [{'digit': '999'}, {'sign': '-'}, {'digit': '99'}, {'sign': '-'}, {'digit': '9999'}]
    assert r.pattern == '\\d\\d\\d-\\d\\d-\\d\\d\\d\\d'
    assert calcsize("USAGE DISPLAY PICTURE 999-99-9999") == 11

    assert calcsize("USAGE DISPLAY PICTURE $**,***,**9.99") == 14

@pytest.fixture
def recfm_n_file():
    return BytesIO(b'\x01\x02\x03\x04\x11\x12\x13\x14')

@pytest.fixture
def recfm_n_reader(recfm_n_file):
    return RECFM_N(recfm_n_file)

def test_recfm_n(recfm_n_reader):
    records = []
    for rec in recfm_n_reader.record_iter():
        records.append(rec)
        recfm_n_reader.used(4)
    assert records == [
        b'\x01\x02\x03\x04\x11\x12\x13\x14',
        b'\x11\x12\x13\x14',
    ]

def test_faulty_recfm_n(recfm_n_reader):
    records = []
    with pytest.raises(RuntimeError) as exception_info:
        for rec in recfm_n_reader.record_iter():
            records.append(rec)
    assert exception_info.value.args[0] == "no bytes consumed from buffer via the .used() method"
    assert records == [
        b'\x01\x02\x03\x04\x11\x12\x13\x14',
    ]


@pytest.fixture
def recfm_f_file():
    return BytesIO(b'\x01\x02\x03\x04\x11\x12\x13\x14')

@pytest.fixture
def recfm_f_reader(recfm_f_file):
    return RECFM_F(recfm_f_file, 4)

def test_recfm_f(recfm_f_reader):
    records = list(recfm_f_reader.record_iter())
    assert records == [
        b'\x01\x02\x03\x04',
        b'\x11\x12\x13\x14',
    ]

def test_rdw_recfm_f(recfm_f_reader):
    records_rdw = list(recfm_f_reader.rdw_iter())
    assert records_rdw == [
        b'\x00\x08\x00\x00\x01\x02\x03\x04',
        b'\x00\x08\x00\x00\x11\x12\x13\x14'
    ]


def test_faulty_recfm_f_(recfm_f_file):
    with pytest.raises(TypeError) as exception_info:
        reader = RECFM_F(recfm_f_file, 0)
        records = list(reader.record_iter())
    assert exception_info.value.args[0] == "RECFM_F requires lrecl > 0"
    

@pytest.fixture
def recfm_v_file():
    return BytesIO(
        b'\x00\x08\x00\x00\x01\x02\x03\x04'  # RDW + 4 bytes
        b'\x00\x08\x00\x00\x11\x12\x13\x14'  # RDW + 4 bytes
    )


@pytest.fixture
def recfm_v_reader(recfm_v_file):
    return RECFM_V(recfm_v_file)


def test_recfm_v(recfm_v_reader):
    records = list(recfm_v_reader.record_iter())
    assert records == [
        b'\x01\x02\x03\x04',
        b'\x11\x12\x13\x14',
    ]


def test_rdw_recfm_v(recfm_v_reader):
    records_rdw = list(recfm_v_reader.rdw_iter())
    assert records_rdw == [
        b'\x00\x08\x00\x00\x01\x02\x03\x04',
        b'\x00\x08\x00\x00\x11\x12\x13\x14'
    ]


@pytest.fixture
def recfm_vb_file():
    return BytesIO(
        b'\x00\x14\x00\x00'  # BDW: 20 = 4+sum of RDW's
        b'\x00\x08\x00\x00\x01\x02\x03\x04'  # RDW + 4 bytes
        b'\x00\x08\x00\x00\x11\x12\x13\x14'  # RDW + 4 bytes
        b'\x00\x14\x00\x00'  # BDW: 20 = 4+sum of RDW's
        b'\x00\x08\x00\x00\x21\x22\x23\x24'  # RDW + 4 bytes
        b'\x00\x08\x00\x00\x31\x32\x33\x34'  # RDW + 4 bytes
    )

@pytest.fixture
def recfm_vb_reader(recfm_vb_file):
    return RECFM_VB(recfm_vb_file)

def test_recfm_vb(recfm_vb_reader):
    records = list(recfm_vb_reader.record_iter())
    assert records == [
        b'\x01\x02\x03\x04',
        b'\x11\x12\x13\x14',
        b'\x21\x22\x23\x24',
        b'\x31\x32\x33\x34',
    ]

def test_rdw_recfm_vb(recfm_vb_reader):
    records_rdw = list(recfm_vb_reader.rdw_iter())
    assert records_rdw == [
        b'\x00\x08\x00\x00\x01\x02\x03\x04',
        b'\x00\x08\x00\x00\x11\x12\x13\x14',
        b'\x00\x08\x00\x00\x21\x22\x23\x24',
        b'\x00\x08\x00\x00\x31\x32\x33\x34',
    ]

def test_bdw_recfm_vb(recfm_vb_reader):
    records_bdw = list(recfm_vb_reader.bdw_iter())
    assert records_bdw == [
        b'\x00\x14\x00\x00\x00\x08\x00\x00\x01\x02\x03\x04\x00\x08\x00\x00\x11\x12\x13\x14',
        b'\x00\x14\x00\x00\x00\x08\x00\x00\x21\x22\x23\x24\x00\x08\x00\x00\x31\x32\x33\x34',
    ]
