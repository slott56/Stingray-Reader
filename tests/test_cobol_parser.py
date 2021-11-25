"""
COBOL Parser Test Cases.
"""

import pytest
import os
from io import StringIO
from pprint import pprint
from textwrap import dedent
import json
from pathlib import Path
from stingray.cobol_parser import *

try:
    from jsonschema import Draft202012Validator as SchemaValidator  # type: ignore [import]
except ImportError:
    from jsonschema import Draft7Validator as SchemaValidator  # type: ignore [import]


def test_reference_format_good():
    good_source = StringIO(
        dedent(
        """
        123456*
              * COPYT1.COB
               01  'SOME'-RECORD-1.
                   05  'SOME'-COLUMN PIC X(1).
                   05  'SOME'-CONTINUATION
              - PIC X(2).
              D    05  'SOME-'DEBUG PIC X(3).
               EJECT
        """
        )
    )
    assert list(reference_format(good_source, replacing=[("'SOME'", "X"),])) == [
        '01  X-RECORD-1.\n',
        '    05  X-COLUMN PIC X(1).\n',
        '    05  X-CONTINUATION\n PIC X(2).\n'
    ]

def test_reference_format_bad():
    bad_source = StringIO(
        dedent(
        """
        123456*
              * COPYT1.COB
               01  SOME-RECORD-1.
                   05  SOME-COLUMN PIC X(1).
               SKIP1
               COPY 'DETAILS'
               SKIP1
                   05  SOME-COLUMN PIC X(2).
        """
        )
    )
    with pytest.raises(ValueError) as exception_info:
        list(reference_format(bad_source))
    assert exception_info.value.args[0] == "directive not supported: \"COPY 'DETAILS'\\n\""

def test_bad_picture():
    with pytest.raises(ValueError) as exc_info:
        normalize_picture("S9(5)V99NOPE")
    print(dir(exc_info))
    assert exc_info.type == ValueError
    assert exc_info.value.args[0] == "invalid characters 'NOPE' in PIC 'S9(5)V99NOPE'"

@pytest.fixture(
    params=[
        ("01 RECORD. ", [{"name": "01"}, {}, {"name": "RECORD"}, {}]),
        (
                "05 A REDEFINES B. ",
                [{"name": "05"}, {}, {"name": "A"}, {}, {"redefines": "B"}, {}],
        ),
        (
                "05 B BLANK WHEN ZERO. ",
                [{"name": "05"}, {}, {"name": "B"}, {}, {"blank": "ZERO"}, {}],
        ),
        (
                "05 C JUSTIFIED RIGHT. ",
                [{"name": "05"}, {}, {"name": "C"}, {}, {"justified": "RIGHT"}, {}],
        ),
        (
                "05 D PIC XXX OCCURS 10. ",
                [
                    {"name": "05"},
                    {},
                    {"name": "D"},
                    {},
                    {"picture": "XXX"},
                    {},
                    {"occurs_maxitems": "10"},
                    {},
                ],
        ),
        (
                "10 WEEK-RECORD OCCURS 52 TIMES ASCENDING KEY IS WEEK-NO INDEXED BY C. ",
                [
                    {"name": "10"},
                    {},
                    {"name": "WEEK-RECORD"},
                    {},
                    {"occurs_maxitems": "52"},
                    {},
                ],
        ),
        (
                "05 FIELD-2 OCCURS 1 TO 5 TIMES DEPENDING ON FIELD-1 PIC X(05). ",
                [
                    {"name": "05"},
                    {},
                    {"name": "FIELD-2"},
                    {},
                    {"odo_minitems": "1", "odo_maxitems": "5", "depending_on": "FIELD-1"},
                    {},
                    {"picture": "X(05)."},
                    {},
                ],
        ),
        (
                "05 D SIGN LEADING SEPARATE SYNC. ",
                [
                    {"name": "05"},
                    {},
                    {"name": "D"},
                    {},
                    {"sign": "LEADING", "sign_sep": " SEPARATE"},
                    {},
                    {},
                    {},
                ],
        ),
        (
                "05 E PIC S999V99 USAGE PACKED-DECIMAL. ",
                [
                    {"name": "05"},
                    {},
                    {"name": "E"},
                    {},
                    {"picture": "S999V99"},
                    {},
                    {"usage": "PACKED-DECIMAL"},
                    {},
                ],
        ),
        (
                "05 F PIC S999V99 VALUE 42. ",
                [
                    {"name": "05"},
                    {},
                    {"name": "F"},
                    {},
                    {"picture": "S999V99"},
                    {},
                    {"value": "42."},
                    {},
                ],
        ),
    ],
    ids=lambda p: repr(p[0]),
)
def clause_case(request) -> tuple[str, list[dict[str, str]]]:
    return request.param


def test_clause_pattern(clause_case):
    source, clauses = clause_case
    assert (
            list(
                {n: v for n, v in c.groupdict().items() if v}
                for c in clause_pattern.finditer(source)
            )
            == clauses
    )


@pytest.fixture
def sample_cobol() -> Path:
    return Path(os.environ.get("SAMPLES", "samples")) / "EPSPDATA.cpy"


def test_sentences(sample_cobol):
    with sample_cobol.open() as source:
        parsed = (
            (level, clause_dict(body))
            for level, body in dde_sentences(reference_format(source))
        )
        names_parsed = [
            (level, clauses.get("name") or clauses.get("filler") or "FILLER", clauses)
            for level, clauses in parsed
        ]
    assert names_parsed == [
        ("01", "EPSPDATA", {"name": "EPSPDATA"}),
        (
            "03",
            "EPSPDATA-PRINCIPLE-DATA",
            {
                "name": "EPSPDATA-PRINCIPLE-DATA",
                "picture": "S9(9)V99",
                "usage": "COMP",
                "_picture_parsed": [
                    {"sign": "S"},
                    {"digit": "999999999"},
                    {"decimal": "V"},
                    {"digit": "99"},
                ],
            },
        ),
        (
            "03",
            "EPSPDATA-NUMBER-OF-YEARS",
            {
                "name": "EPSPDATA-NUMBER-OF-YEARS",
                "picture": "S9(4)",
                "usage": "COMP",
                "_picture_parsed": [{"sign": "S"}, {"digit": "9999"}],
            },
        ),
        (
            "03",
            "EPSPDATA-NUMBER-OF-MONTHS",
            {
                "name": "EPSPDATA-NUMBER-OF-MONTHS",
                "picture": "S9(4)",
                "usage": "COMP",
                "_picture_parsed": [{"sign": "S"}, {"digit": "9999"}],
            },
        ),
        (
            "03",
            "EPSPDATA-QUOTED-INTEREST-RATE",
            {
                "name": "EPSPDATA-QUOTED-INTEREST-RATE",
                "picture": "S9(2)v9(3)",
                "usage": "COMP",
                "_picture_parsed": [{"sign": "S"}, {"digit": "99"}, {"digit": "999"}],
            },
        ),
        (
            "03",
            "EPSPDATA-YEAR-MONTH-IND",
            {
                "name": "EPSPDATA-YEAR-MONTH-IND",
                "picture": "X",
                "_picture_parsed": [{"digit": "X"}],
            },
        ),
        (
            "03",
            "EPSPDATA-RETURN-MONTH-PAYMENT",
            {
                "name": "EPSPDATA-RETURN-MONTH-PAYMENT",
                "picture": "S9(7)V99",
                "usage": "COMP",
                "_picture_parsed": [
                    {"sign": "S"},
                    {"digit": "9999999"},
                    {"decimal": "V"},
                    {"digit": "99"},
                ],
            },
        ),
        (
            "03",
            "EPSPDATA-RETURN-ERROR",
            {
                "name": "EPSPDATA-RETURN-ERROR",
                "picture": "X(80)",
                "_picture_parsed": [
                    {
                        "digit": "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                    }
                ],
            },
        ),
    ]


def test_dde_structure(sample_cobol, capsys):
    with sample_cobol.open() as source:
        copy_book = structure(dde_sentences(reference_format(source)))
    for record in copy_book:
        DDE.display(record)
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "DDE('01', 'EPSPDATA', clauses={'name': 'EPSPDATA'}): USAGE DISPLAY PIC (Group) None",
        "    DDE('03', 'EPSPDATA-PRINCIPLE-DATA   PIC S9(9)V99 COMP', clauses={'name': 'EPSPDATA-PRINCIPLE-DATA', 'picture': 'S9(9)V99', 'usage': 'COMP', '_picture_parsed': [{'sign': 'S'}, {'digit': '999999999'}, {'decimal': 'V'}, {'digit': '99'}]}): USAGE COMP PIC [{'sign': 'S'}, {'digit': '999999999'}, {'decimal': 'V'}, {'digit': '99'}] None",
        "    DDE('03', 'EPSPDATA-NUMBER-OF-YEARS  PIC S9(4)    COMP', clauses={'name': 'EPSPDATA-NUMBER-OF-YEARS', 'picture': 'S9(4)', 'usage': 'COMP', '_picture_parsed': [{'sign': 'S'}, {'digit': '9999'}]}): USAGE COMP PIC [{'sign': 'S'}, {'digit': '9999'}] None",
        "    DDE('03', 'EPSPDATA-NUMBER-OF-MONTHS PIC S9(4)    COMP', clauses={'name': 'EPSPDATA-NUMBER-OF-MONTHS', 'picture': 'S9(4)', 'usage': 'COMP', '_picture_parsed': [{'sign': 'S'}, {'digit': '9999'}]}): USAGE COMP PIC [{'sign': 'S'}, {'digit': '9999'}] None",
        "    DDE('03', 'EPSPDATA-QUOTED-INTEREST-RATE\\n                                 PIC S9(2)v9(3) COMP', clauses={'name': 'EPSPDATA-QUOTED-INTEREST-RATE', 'picture': 'S9(2)v9(3)', 'usage': 'COMP', '_picture_parsed': [{'sign': 'S'}, {'digit': '99'}, {'digit': '999'}]}): USAGE COMP PIC [{'sign': 'S'}, {'digit': '99'}, {'digit': '999'}] None",
        "    DDE('03', 'EPSPDATA-YEAR-MONTH-IND   PIC X', clauses={'name': 'EPSPDATA-YEAR-MONTH-IND', 'picture': 'X', '_picture_parsed': [{'digit': 'X'}]}): USAGE DISPLAY PIC [{'digit': 'X'}] None",
        "    DDE('03', 'EPSPDATA-RETURN-MONTH-PAYMENT\\n                                 PIC S9(7)V99 COMP', clauses={'name': 'EPSPDATA-RETURN-MONTH-PAYMENT', 'picture': 'S9(7)V99', 'usage': 'COMP', '_picture_parsed': [{'sign': 'S'}, {'digit': '9999999'}, {'decimal': 'V'}, {'digit': '99'}]}): USAGE COMP PIC [{'sign': 'S'}, {'digit': '9999999'}, {'decimal': 'V'}, {'digit': '99'}] None",
        "    DDE('03', 'EPSPDATA-RETURN-ERROR     PIC X(80)', clauses={'name': 'EPSPDATA-RETURN-ERROR', 'picture': 'X(80)', '_picture_parsed': [{'digit': 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'}]}): USAGE DISPLAY PIC [{'digit': 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'}] None",
    ]


def test_dde_syntax_cases():
    """Legacy test cases for COBOL syntax"""
    ref_format_1 = ["01  GROUP. ", ""]
    assert structure(dde_sentences(ref_format_1)) == [DDE('01', 'GROUP', clauses={'name': 'GROUP'})]
    ref_format_2 = ["05  PIC X(10). "]
    assert structure(dde_sentences(ref_format_2)) == [
        DDE('05', 'PIC X(10)', clauses={'picture': 'X(10)', '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_3 = ["05  ELEMENTARY PIC X(10). "]
    assert structure(dde_sentences(ref_format_3)) == [DDE('05', 'ELEMENTARY PIC X(10)',
                                                          clauses={'name': 'ELEMENTARY', 'picture': 'X(10)',
                                                                   '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_4 = " ".join(["05", "ELEMENTARY-OCCURS", "PIC", "S9999", "OCCURS", "5", "TIMES", ". "])
    assert structure(dde_sentences(ref_format_4)) == [DDE('05', 'ELEMENTARY-OCCURS PIC S9999 OCCURS 5 TIMES ',
                                                          clauses={'name': 'ELEMENTARY-OCCURS', 'picture': 'S9999',
                                                                   'occurs_maxitems': '5',
                                                                   '_picture_parsed': [{'sign': 'S'},
                                                                                       {'digit': '9999'}]})]
    ref_format_5 = " ".join(["05", "GROUP-OCCURS", "OCCURS", "7", "TIMES", "INDEXED", "BY", "IRRELEVANT", ". "])
    assert structure(dde_sentences(ref_format_5)) == [DDE('05', 'GROUP-OCCURS OCCURS 7 TIMES INDEXED BY IRRELEVANT ',
                                                          clauses={'name': 'IRRELEVANT', 'occurs_maxitems': '7'})]
    ref_format_6 = " ".join(["05", "ELEMENTARY-PIC", "PIC", "S9999.999", ". "])
    assert structure(dde_sentences(ref_format_6)) == [DDE('05', 'ELEMENTARY-PIC PIC S9999.999 ',
                                                          clauses={'name': 'ELEMENTARY-PIC', 'picture': 'S9999.999',
                                                                   '_picture_parsed': [{'sign': 'S'}, {'digit': '9999'},
                                                                                       {'decimal': '.'},
                                                                                       {'digit': '999'}]})]
    ref_format_7 = " ".join(["05", "ELEMENTARY-REDEF", "PIC", "S9999", "REDEFINES", "SOME-NAME", ". "])
    assert structure(dde_sentences(ref_format_7)) == [DDE('05', 'ELEMENTARY-REDEF PIC S9999 REDEFINES SOME-NAME ',
                                                          clauses={'name': 'ELEMENTARY-REDEF', 'picture': 'S9999',
                                                                   'redefines': 'SOME-NAME',
                                                                   '_picture_parsed': [{'sign': 'S'},
                                                                                       {'digit': '9999'}]})]
    ref_format_8 = " ".join(["05", "GROUP-REDEF", "OCCURS", "7", "TIMES", "REDEFINES", "ANOTHER-NAME", ". "])
    assert structure(dde_sentences(ref_format_8)) == [DDE('05', 'GROUP-REDEF OCCURS 7 TIMES REDEFINES ANOTHER-NAME ',
                                                          clauses={'name': 'GROUP-REDEF', 'occurs_maxitems': '7',
                                                                   'redefines': 'ANOTHER-NAME'})]
    ref_format_9 = " ".join(["05", "USE-DISPLAY", "PIC", "S9999", "USAGE", "DISPLAY", ". "])
    assert structure(dde_sentences(ref_format_9)) == [DDE('05', 'USE-DISPLAY PIC S9999 USAGE DISPLAY ',
                                                          clauses={'name': 'USE-DISPLAY', 'picture': 'S9999',
                                                                   'usage': 'DISPLAY',
                                                                   '_picture_parsed': [{'sign': 'S'},
                                                                                       {'digit': '9999'}]})]
    ref_format_10 = " ".join(["05", "USE-COMP-3", "PIC", "S9(7)V99", "USAGE", "COMP-3", ". "])
    assert structure(dde_sentences(ref_format_10)) == [DDE('05', 'USE-COMP-3 PIC S9(7)V99 USAGE COMP-3 ',
                                                           clauses={'name': 'USE-COMP-3', 'picture': 'S9(7)V99',
                                                                    'usage': 'COMP-3',
                                                                    '_picture_parsed': [{'sign': 'S'},
                                                                                        {'digit': '9999999'},
                                                                                        {'decimal': 'V'},
                                                                                        {'digit': '99'}]})]
    ref_format_11 = " ".join(["05", "USE-COMP-3-ALT", "PIC", "S9(9)V99", "COMP-3", ". "])
    assert structure(dde_sentences(ref_format_11)) == [DDE('05', 'USE-COMP-3-ALT PIC S9(9)V99 COMP-3 ',
                                                           clauses={'name': 'USE-COMP-3-ALT', 'picture': 'S9(9)V99',
                                                                    'usage': 'COMP-3',
                                                                    '_picture_parsed': [{'sign': 'S'},
                                                                                        {'digit': '999999999'},
                                                                                        {'decimal': 'V'},
                                                                                        {'digit': '99'}]})]
    ref_format_12 = " ".join(["05", "DEP-1", "OCCURS", "1", "TO", "5", "TIMES", "DEPENDING", "ON", "ODO-1", ". "])
    assert structure(dde_sentences(ref_format_12)) == [DDE('05', 'DEP-1 OCCURS 1 TO 5 TIMES DEPENDING ON ODO-1 ',
                                                           clauses={'name': 'DEP-1', 'odo_minitems': '1',
                                                                    'odo_maxitems': '5', 'depending_on': 'ODO-1'})]
    ref_format_13 = " ".join(["05", "DEP-2", "OCCURS", "TO", "5", "TIMES", "DEPENDING", "ON", "ODO-2", ". "])
    assert structure(dde_sentences(ref_format_13)) == [
        DDE('05', 'DEP-2 OCCURS TO 5 TIMES DEPENDING ON ODO-2 ', clauses={'name': 'ODO-2'})]
    ref_format_14 = " ".join(["05", "BLANK-ZERO-1", "PIC", "X(10)", "BLANK", "ZERO", ". "])
    assert structure(dde_sentences(ref_format_14)) == [DDE('05', 'BLANK-ZERO-1 PIC X(10) BLANK ZERO ',
                                                           clauses={'name': 'BLANK-ZERO-1', 'picture': 'X(10)',
                                                                    'blank': 'ZERO',
                                                                    '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_15 = " ".join(["05", "BLANK-ZERO-2", "PIC", "X(10)", "BLANK", "WHEN", "ZEROES", ". "])
    assert structure(dde_sentences(ref_format_15)) == [DDE('05', 'BLANK-ZERO-2 PIC X(10) BLANK WHEN ZEROES ',
                                                           clauses={'name': 'ES', 'picture': 'X(10)', 'blank': 'ZERO',
                                                                    '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_16 = " ".join(["05", "JUST-RIGHT-1", "PIC", "X(10)", "JUST", "RIGHT", ". "])
    assert structure(dde_sentences(ref_format_16)) == [DDE('05', 'JUST-RIGHT-1 PIC X(10) JUST RIGHT ',
                                                           clauses={'name': 'JUST-RIGHT-1', 'picture': 'X(10)',
                                                                    'justified': 'RIGHT',
                                                                    '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_17 = " ".join(["05", "JUST-RIGHT-2", "PIC", "X(10)", "JUSTIFIED", "RIGHT", ". "])
    assert structure(dde_sentences(ref_format_17)) == [DDE('05', 'JUST-RIGHT-2 PIC X(10) JUSTIFIED RIGHT ',
                                                           clauses={'name': 'JUST-RIGHT-2', 'picture': 'X(10)',
                                                                    'justified': 'RIGHT',
                                                                    '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_18 = " ".join(["05", "VALUE-1", "PIC", "X(8)", "VALUE", "'10 CHARS'", ". "])
    assert structure(dde_sentences(ref_format_18)) == [DDE('05', "VALUE-1 PIC X(8) VALUE '10 CHARS' ",
                                                           clauses={'name': 'VALUE-1', 'picture': 'X(8)',
                                                                    'value': "'10 CHARS'",
                                                                    '_picture_parsed': [{'digit': 'XXXXXXXX'}]})]


def test_dde_warning_syntax_cases():
    """Featues generally ignored, but accepted silently. A warning might be nice."""
    ref_format_1 = " ".join(("66", "BLANK-ZERO-1", "RENAMES", "SOME-NAME", ". "))
    assert structure(dde_sentences(ref_format_1)) == [
        DDE('66', 'BLANK-ZERO-1 RENAMES SOME-NAME ', clauses={'name': 'SOME-NAME'})]
    ref_format_2 = " ".join(("05", "SIGN-1", "PIC", "X(10)", "LEADING", "SIGN", ". "))
    assert structure(dde_sentences(ref_format_2)) == [DDE('05', 'SIGN-1 PIC X(10) LEADING SIGN ',
                                                          clauses={'name': 'SIGN', 'picture': 'X(10)',
                                                                   '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_3 = " ".join(("05", "SIGN-2", "PIC", "X(10)", "TRAILING", ". "))
    assert structure(dde_sentences(ref_format_3)) == [DDE('05', 'SIGN-2 PIC X(10) TRAILING ',
                                                          clauses={'name': 'TRAILING', 'picture': 'X(10)',
                                                                   '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_4 = " ".join(("05", "SIGN-3", "PIC", "X(10)", "SIGN", "IS", "SEPARATE", ". "))
    assert structure(dde_sentences(ref_format_4)) == [DDE('05', 'SIGN-3 PIC X(10) SIGN IS SEPARATE ',
                                                          clauses={'name': 'SEPARATE', 'picture': 'X(10)',
                                                                   '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]
    ref_format_5 = " ".join(("05", "SYNC-1", "PIC", "X(10)", "SYNCHRONIZED", ". "))
    assert structure(dde_sentences(ref_format_5)) == [DDE('05', 'SYNC-1 PIC X(10) SYNCHRONIZED ',
                                                          clauses={'name': '-1', 'picture': 'X(10)',
                                                                   '_picture_parsed': [{'digit': 'XXXXXXXXXX'}]})]


def test_schemamaker_sample(sample_cobol):
    with sample_cobol.open() as source:
        copy_book_list = structure(dde_sentences(reference_format(source)))
    maker = JSONSchemaMaker(copy_book_list[0])
    s = maker.jsonschema()
    assert s == {
        "title": "EPSPDATA",
        "$anchor": "EPSPDATA",
        "cobol": "01 EPSPDATA",
        "type": "object",
        "properties": {
            "EPSPDATA-PRINCIPLE-DATA": {
                "title": "EPSPDATA-PRINCIPLE-DATA",
                "$anchor": "EPSPDATA-PRINCIPLE-DATA",
                "cobol": "03 EPSPDATA-PRINCIPLE-DATA PIC S9(9)V99 COMP",
                "type": "integer",
                "contentEncoding": "bigendian-int",
            },
            "EPSPDATA-NUMBER-OF-YEARS": {
                "title": "EPSPDATA-NUMBER-OF-YEARS",
                "$anchor": "EPSPDATA-NUMBER-OF-YEARS",
                "cobol": "03 EPSPDATA-NUMBER-OF-YEARS PIC S9(4) COMP",
                "type": "integer",
                "contentEncoding": "bigendian-int",
            },
            "EPSPDATA-NUMBER-OF-MONTHS": {
                "title": "EPSPDATA-NUMBER-OF-MONTHS",
                "$anchor": "EPSPDATA-NUMBER-OF-MONTHS",
                "cobol": "03 EPSPDATA-NUMBER-OF-MONTHS PIC S9(4) COMP",
                "type": "integer",
                "contentEncoding": "bigendian-int",
            },
            "EPSPDATA-QUOTED-INTEREST-RATE": {
                "title": "EPSPDATA-QUOTED-INTEREST-RATE",
                "$anchor": "EPSPDATA-QUOTED-INTEREST-RATE",
                "cobol": "03 EPSPDATA-QUOTED-INTEREST-RATE PIC S9(2)v9(3) COMP",
                "type": "integer",
                "contentEncoding": "bigendian-int",
            },
            "EPSPDATA-YEAR-MONTH-IND": {
                "title": "EPSPDATA-YEAR-MONTH-IND",
                "$anchor": "EPSPDATA-YEAR-MONTH-IND",
                "cobol": "03 EPSPDATA-YEAR-MONTH-IND PIC X",
                "type": "string",
                "contentEncoding": "cp037",
            },
            "EPSPDATA-RETURN-MONTH-PAYMENT": {
                "title": "EPSPDATA-RETURN-MONTH-PAYMENT",
                "$anchor": "EPSPDATA-RETURN-MONTH-PAYMENT",
                "cobol": "03 EPSPDATA-RETURN-MONTH-PAYMENT PIC S9(7)V99 COMP",
                "type": "integer",
                "contentEncoding": "bigendian-int",
            },
            "EPSPDATA-RETURN-ERROR": {
                "title": "EPSPDATA-RETURN-ERROR",
                "$anchor": "EPSPDATA-RETURN-ERROR",
                "cobol": "03 EPSPDATA-RETURN-ERROR PIC X(80)",
                "type": "string",
                "contentEncoding": "cp037",
            },
        },
    }
    assert list(maker.names.keys()) == [
        "EPSPDATA",
        "EPSPDATA-PRINCIPLE-DATA",
        "EPSPDATA-NUMBER-OF-YEARS",
        "EPSPDATA-NUMBER-OF-MONTHS",
        "EPSPDATA-QUOTED-INTEREST-RATE",
        "EPSPDATA-YEAR-MONTH-IND",
        "EPSPDATA-RETURN-MONTH-PAYMENT",
        "EPSPDATA-RETURN-ERROR",
    ]

def test_extended_schemamaker_sample(sample_cobol):
    with sample_cobol.open() as source:
        schema_list = list(schema_iter(source))
    assert schema_list[0] == {
         '$anchor': 'EPSPDATA',
         'cobol': '01 EPSPDATA',
         'properties': {'EPSPDATA-NUMBER-OF-MONTHS': {'$anchor': 'EPSPDATA-NUMBER-OF-MONTHS',
                                                      'cobol': '03 '
                                                               'EPSPDATA-NUMBER-OF-MONTHS '
                                                               'PIC S9(4) COMP',
                                                      'contentEncoding': 'bigendian-int',
                                                      'title': 'EPSPDATA-NUMBER-OF-MONTHS',
                                                      'type': 'integer'},
                        'EPSPDATA-NUMBER-OF-YEARS': {'$anchor': 'EPSPDATA-NUMBER-OF-YEARS',
                                                     'cobol': '03 '
                                                              'EPSPDATA-NUMBER-OF-YEARS '
                                                              'PIC S9(4) COMP',
                                                     'contentEncoding': 'bigendian-int',
                                                     'title': 'EPSPDATA-NUMBER-OF-YEARS',
                                                     'type': 'integer'},
                        'EPSPDATA-PRINCIPLE-DATA': {'$anchor': 'EPSPDATA-PRINCIPLE-DATA',
                                                    'cobol': '03 '
                                                             'EPSPDATA-PRINCIPLE-DATA '
                                                             'PIC S9(9)V99 COMP',
                                                    'contentEncoding': 'bigendian-int',
                                                    'title': 'EPSPDATA-PRINCIPLE-DATA',
                                                    'type': 'integer'},
                        'EPSPDATA-QUOTED-INTEREST-RATE': {'$anchor': 'EPSPDATA-QUOTED-INTEREST-RATE',
                                                          'cobol': '03 '
                                                                   'EPSPDATA-QUOTED-INTEREST-RATE '
                                                                   'PIC S9(2)v9(3) '
                                                                   'COMP',
                                                          'contentEncoding': 'bigendian-int',
                                                          'title': 'EPSPDATA-QUOTED-INTEREST-RATE',
                                                          'type': 'integer'},
                        'EPSPDATA-RETURN-ERROR': {'$anchor': 'EPSPDATA-RETURN-ERROR',
                                                  'cobol': '03 EPSPDATA-RETURN-ERROR '
                                                           'PIC X(80)',
                                                  'contentEncoding': 'cp037',
                                                  'title': 'EPSPDATA-RETURN-ERROR',
                                                  'type': 'string'},
                        'EPSPDATA-RETURN-MONTH-PAYMENT': {'$anchor': 'EPSPDATA-RETURN-MONTH-PAYMENT',
                                                          'cobol': '03 '
                                                                   'EPSPDATA-RETURN-MONTH-PAYMENT '
                                                                   'PIC S9(7)V99 COMP',
                                                          'contentEncoding': 'bigendian-int',
                                                          'title': 'EPSPDATA-RETURN-MONTH-PAYMENT',
                                                          'type': 'integer'},
                        'EPSPDATA-YEAR-MONTH-IND': {'$anchor': 'EPSPDATA-YEAR-MONTH-IND',
                                                    'cobol': '03 '
                                                             'EPSPDATA-YEAR-MONTH-IND '
                                                             'PIC X',
                                                    'contentEncoding': 'cp037',
                                                    'title': 'EPSPDATA-YEAR-MONTH-IND',
                                                    'type': 'string'}},
         'title': 'EPSPDATA',
         'type': 'object'}

@pytest.fixture
def copy_t1_source() -> str:
    return dedent(
        """
        123456*
              * COPYT1.COB
               01  SOME-RECORD-1.
                   05  SOME-COLUMN PIC X(5).
        """
    )


def test_1(copy_t1_source: str, capsys) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_t1_source))))
    for record in copy_book:
        DDE.display(record)
        maker = JSONSchemaMaker(record)
        schema = maker.jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "SOME-RECORD-1",
        "cobol": "01 SOME-RECORD-1",
        "properties": {
            "SOME-COLUMN": {
                "$anchor": "SOME-COLUMN",
                "cobol": "05 SOME-COLUMN PIC X(5)",
                "contentEncoding": "cp037",
                "title": "SOME-COLUMN",
                "type": "string",
            }
        },
        "title": "SOME-RECORD-1",
        "type": "object",
    }
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "DDE('01', 'SOME-RECORD-1', clauses={'name': 'SOME-RECORD-1'}): USAGE DISPLAY PIC (Group) None",
        "    DDE('05', 'SOME-COLUMN PIC X(5)', clauses={'name': 'SOME-COLUMN', 'picture': 'X(5)', '_picture_parsed': [{'digit': 'XXXXX'}]}): USAGE DISPLAY PIC [{'digit': 'XXXXX'}] None",
    ]
    # Extra test result.
    assert repr(maker) == (
        f"JSONSchemaMaker({record}): self.names={{'SOME-RECORD-1': {{'title': 'SOME-RECORD-1', "
        "'$anchor': 'SOME-RECORD-1', 'cobol': '01 SOME-RECORD-1', 'type': 'object', "
        "'properties': {'SOME-COLUMN': {'title': 'SOME-COLUMN', '$anchor': "
        "'SOME-COLUMN', 'cobol': '05 SOME-COLUMN PIC X(5)', 'type': 'string', "
        "'contentEncoding': 'cp037'}}}, 'SOME-COLUMN': {'title': 'SOME-COLUMN', "
        "'$anchor': 'SOME-COLUMN', 'cobol': '05 SOME-COLUMN PIC X(5)', 'type': "
        "'string', 'contentEncoding': 'cp037'}}"
    )

@pytest.fixture
def copy_t2_source() -> str:
    return dedent(
        """
        123456*
              * COPYT2.COB
               01  SOME-RECORD-2.
                   05  FILLER PIC X(5).
                   05  REPEAT OCCURS 4 PIC XXX.
        """
    )


def test_2(copy_t2_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_t2_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "SOME-RECORD-2",
        "cobol": "01 SOME-RECORD-2",
        "properties": {
            "FILLER-1": {
                "$anchor": "FILLER-1",
                "cobol": "05 FILLER PIC X(5)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "REPEAT": {
                "$anchor": "REPEAT",
                "cobol": "05 REPEAT OCCURS 4 PIC XXX",
                "items": {
                    "properties": {
                        "REPEAT": {
                            "cobol": "05 " "REPEAT " "OCCURS " "4 PIC " "XXX",
                            "contentEncoding": "cp037",
                            "type": "string",
                        }
                    },
                    "type": "object",
                },
                "maxItems": 4,
                "title": "REPEAT",
                "type": "array",
            },
        },
        "title": "SOME-RECORD-2",
        "type": "object",
    }


@pytest.fixture
def copy_t3_source() -> str:
    return dedent(
        """
        123456*
              * COPYT3.COB
               01  SOME-RECORD-3.
                   05  FILLER PIC X(5).
                   05  REPEAT OCCURS 5.
                       10  ITEM OCCURS 4 PIC XX.
        """
    )


def test_3(copy_t3_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_t3_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "SOME-RECORD-3",
        "cobol": "01 SOME-RECORD-3",
        "properties": {
            "FILLER-1": {
                "$anchor": "FILLER-1",
                "cobol": "05 FILLER PIC X(5)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "REPEAT": {
                "$anchor": "REPEAT",
                "cobol": "05 REPEAT OCCURS 5",
                "items": {
                    "properties": {
                        "ITEM": {
                            "$anchor": "ITEM",
                            "cobol": "10 ITEM " "OCCURS " "4 PIC " "XX",
                            "items": {
                                "properties": {
                                    "ITEM": {
                                        "cobol": "10 "
                                                 "ITEM "
                                                 "OCCURS "
                                                 "4 "
                                                 "PIC "
                                                 "XX",
                                        "contentEncoding": "cp037",
                                        "type": "string",
                                    }
                                },
                                "type": "object",
                            },
                            "maxItems": 4,
                            "title": "ITEM",
                            "type": "array",
                        }
                    },
                    "type": "object",
                },
                "maxItems": 5,
                "title": "REPEAT",
                "type": "array",
            },
        },
        "title": "SOME-RECORD-3",
        "type": "object",
    }


@pytest.fixture
def copy_1_source() -> str:
    return dedent(
        """
        123456*
              * COPY1.COB
               01  DETAIL-LINE.
                   05                              PIC X(7).
                   05  QUESTION                    PIC ZZ.
                   05                              PIC X(6).
                   05  PRINT-YES                   PIC ZZ.
                   05                              PIC X(3).
                   05  PRINT-NO                    PIC ZZ.
                   05                              PIC X(6).
                 SKIP2
                   05  NOT-SURE                    PIC ZZ.
                   05                              PIC X(7).
        """
    )


def test_4(copy_1_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_1_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "DETAIL-LINE",
        "cobol": "01 DETAIL-LINE",
        "properties": {
            "FILLER-1": {
                "$anchor": "FILLER-1",
                "cobol": "05 PIC X(7)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-2": {
                "$anchor": "FILLER-2",
                "cobol": "05 PIC X(6)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-3": {
                "$anchor": "FILLER-3",
                "cobol": "05 PIC X(3)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-4": {
                "$anchor": "FILLER-4",
                "cobol": "05 PIC X(6)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-5": {
                "$anchor": "FILLER-5",
                "cobol": "05 PIC X(7)",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "NOT-SURE": {
                "$anchor": "NOT-SURE",
                "cobol": "05 NOT-SURE PIC ZZ",
                "contentEncoding": "cp037",
                "title": "NOT-SURE",
                "type": "string",
            },
            "PRINT-NO": {
                "$anchor": "PRINT-NO",
                "cobol": "05 PRINT-NO PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-NO",
                "type": "string",
            },
            "PRINT-YES": {
                "$anchor": "PRINT-YES",
                "cobol": "05 PRINT-YES PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-YES",
                "type": "string",
            },
            "QUESTION": {
                "$anchor": "QUESTION",
                "cobol": "05 QUESTION PIC ZZ",
                "contentEncoding": "cp037",
                "title": "QUESTION",
                "type": "string",
            },
        },
        "title": "DETAIL-LINE",
        "type": "object",
    }


@pytest.fixture
def copy_2_source() -> str:
    return dedent(
        """
        123456*
              * COPY2.COB
               01  WORK-AREAS.
                   05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE 'YES'.
                       88  NO-MORE-RECORDS                     VALUE 'NO '.
                   05  ANSWER-SUB                  PIC 99.
                   05  QUESTION-SUB                PIC 99.
        """
    )


def test_5(copy_2_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_2_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "WORK-AREAS",
        "cobol": "01 WORK-AREAS",
        "properties": {
            "ANSWER-SUB": {
                "$anchor": "ANSWER-SUB",
                "cobol": "05 ANSWER-SUB PIC 99",
                "contentEncoding": "cp037",
                "conversion": "decimal",
                "title": "ANSWER-SUB",
                "type": "string",
            },
            "ARE-THERE-MORE-RECORDS": {
                "$anchor": "ARE-THERE-MORE-RECORDS",
                "cobol": "05 ARE-THERE-MORE-RECORDS " "PIC X(3) VALUE 'YES'",
                "contentEncoding": "cp037",
                "title": "ARE-THERE-MORE-RECORDS",
                "type": "string",
            },
            "QUESTION-SUB": {
                "$anchor": "QUESTION-SUB",
                "cobol": "05 QUESTION-SUB PIC 99",
                "contentEncoding": "cp037",
                "conversion": "decimal",
                "title": "QUESTION-SUB",
                "type": "string",
            },
        },
        "title": "WORK-AREAS",
        "type": "object",
    }


@pytest.fixture
def copy_3_source() -> str:
    return dedent(
        """
        123456*
              * COPY3.COB
               01  SURVEY-RESPONSES.
                   05  QUESTION-NUMBER           OCCURS 10 TIMES.
                       10  RESPONSE-CATEGORY     OCCURS 3 TIMES.
                           15  ANSWER            PIC 99.
        """
    )


def test_6(copy_3_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_3_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "SURVEY-RESPONSES",
        "cobol": "01 SURVEY-RESPONSES",
        "properties": {
            "QUESTION-NUMBER": {
                "$anchor": "QUESTION-NUMBER",
                "cobol": "05 QUESTION-NUMBER OCCURS 10 " "TIMES",
                "items": {
                    "properties": {
                        "RESPONSE-CATEGORY": {
                            "$anchor": "RESPONSE-CATEGORY",
                            "cobol": "10 " "RESPONSE-CATEGORY " "OCCURS " "3 " "TIMES",
                            "items": {
                                "properties": {
                                    "ANSWER": {
                                        "$anchor": "ANSWER",
                                        "cobol": "15 " "ANSWER " "PIC " "99",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "ANSWER",
                                        "type": "string",
                                    }
                                },
                                "type": "object",
                            },
                            "maxItems": 3,
                            "title": "RESPONSE-CATEGORY",
                            "type": "array",
                        }
                    },
                    "type": "object",
                },
                "maxItems": 10,
                "title": "QUESTION-NUMBER",
                "type": "array",
            }
        },
        "title": "SURVEY-RESPONSES",
        "type": "object",
    }


@pytest.fixture
def copy_4_source() -> str:
    """
    DDE Test copybook 4 from page 174 with nested occurs level
    From IBM COBOL Language Reference Manual, fourth edition: SC26-9046-03.
    """
    return dedent(
        """
        123456*
              * COPY4.COB
               01 TABLE-RECORD.
                  05 EMPLOYEE-TABLE OCCURS 10 TIMES
                          ASCENDING KEY IS WAGE-RATE EMPLOYEE-NO
                          INDEXED BY A, B.
                     10 EMPLOYEE-NAME PIC X(20).
                     10 EMPLOYEE-NO PIC 9(6).
                     10 WAGE-RATE PIC 9999V99.
                     10 WEEK-RECORD OCCURS 52 TIMES
                             ASCENDING KEY IS WEEK-NO INDEXED BY C.
                         15 WEEK-NO PIC 99.
                         15 AUTHORIZED-ABSENCES PIC 9.
                         15 UNAUTHORIZED-ABSENCES PIC 9.
                         15 LATE-ARRIVALS PIC 9.

        """
    )


def test_7(copy_4_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_4_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "TABLE-RECORD",
        "cobol": "01 TABLE-RECORD",
        "properties": {
            "B": {
                "$anchor": "B",
                "cobol": "05 EMPLOYEE-TABLE OCCURS 10 TIMES ASCENDING "
                         "KEY IS WAGE-RATE EMPLOYEE-NO INDEXED BY A, B",
                "items": {
                    "properties": {
                        "EMPLOYEE-NAME": {
                            "$anchor": "EMPLOYEE-NAME",
                            "cobol": "10 " "EMPLOYEE-NAME " "PIC " "X(20)",
                            "contentEncoding": "cp037",
                            "title": "EMPLOYEE-NAME",
                            "type": "string",
                        },
                        "EMPLOYEE-NO": {
                            "$anchor": "EMPLOYEE-NO",
                            "cobol": "10 " "EMPLOYEE-NO " "PIC " "9(6)",
                            "contentEncoding": "cp037",
                            "title": "EMPLOYEE-NO",
                            "type": "string",
                        },
                        "WAGE-RATE": {
                            "$anchor": "WAGE-RATE",
                            "cobol": "10 " "WAGE-RATE " "PIC " "9999V99",
                            "contentEncoding": "cp037",
                            "conversion": "decimal",
                            "title": "WAGE-RATE",
                            "type": "string",
                        },
                        "WEEK-RECORD": {
                            "$anchor": "WEEK-RECORD",
                            "cobol": "10 "
                                     "WEEK-RECORD "
                                     "OCCURS "
                                     "52 "
                                     "TIMES "
                                     "ASCENDING "
                                     "KEY "
                                     "IS "
                                     "WEEK-NO "
                                     "INDEXED "
                                     "BY C",
                            "items": {
                                "properties": {
                                    "AUTHORIZED-ABSENCES": {
                                        "$anchor": "AUTHORIZED-ABSENCES",
                                        "cobol": "15 "
                                                 "AUTHORIZED-ABSENCES "
                                                 "PIC "
                                                 "9",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "AUTHORIZED-ABSENCES",
                                        "type": "string",
                                    },
                                    "LATE-ARRIVALS": {
                                        "$anchor": "LATE-ARRIVALS",
                                        "cobol": "15 " "LATE-ARRIVALS " "PIC " "9",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "LATE-ARRIVALS",
                                        "type": "string",
                                    },
                                    "UNAUTHORIZED-ABSENCES": {
                                        "$anchor": "UNAUTHORIZED-ABSENCES",
                                        "cobol": "15 "
                                                 "UNAUTHORIZED-ABSENCES "
                                                 "PIC "
                                                 "9",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "UNAUTHORIZED-ABSENCES",
                                        "type": "string",
                                    },
                                    "WEEK-NO": {
                                        "$anchor": "WEEK-NO",
                                        "cobol": "15 " "WEEK-NO " "PIC " "99",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "WEEK-NO",
                                        "type": "string",
                                    },
                                },
                                "type": "object",
                            },
                            "maxItems": 52,
                            "title": "WEEK-RECORD",
                            "type": "array",
                        },
                    },
                    "type": "object",
                },
                "maxItems": 10,
                "title": "B",
                "type": "array",
            }
        },
        "title": "TABLE-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_5_source() -> str:
    """
    DDE Test copybook 5 from page 195 with simple redefines
    """
    return dedent(
        """
        123456*
              * COPY5.COB
                01  REDEFINES-RECORD.
                     05  A PICTURE X(6).
                     05  B REDEFINES A.
                         10  B-1 PICTURE X(2).
                         10  B-2 PICTURE 9(4).
                     05  C PICTURE 99V99.
        """
    )


def test_8(copy_5_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_5_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "properties": {
            "A": {"$ref": "#A", "cobol": "05 A PICTURE X(6)", "title": "A"},
            "B": {"$ref": "#B", "cobol": "05 B REDEFINES A", "title": "B"},
            "C": {
                "$anchor": "C",
                "cobol": "05 C PICTURE 99V99",
                "contentEncoding": "cp037",
                "conversion": "decimal",
                "title": "C",
                "type": "string",
            },
            "REDEFINES-A": {
                "$anchor": "REDEFINES-A",
                "oneOf": [
                    {
                        "$anchor": "A",
                        "cobol": "05 A PICTURE X(6)",
                        "contentEncoding": "cp037",
                        "title": "A",
                        "type": "string",
                    },
                    {
                        "$anchor": "B",
                        "cobol": "05 B REDEFINES A",
                        "properties": {
                            "B-1": {
                                "$anchor": "B-1",
                                "cobol": "10 " "B-1 " "PICTURE " "X(2)",
                                "contentEncoding": "cp037",
                                "title": "B-1",
                                "type": "string",
                            },
                            "B-2": {
                                "$anchor": "B-2",
                                "cobol": "10 " "B-2 " "PICTURE " "9(4)",
                                "contentEncoding": "cp037",
                                "title": "B-2",
                                "type": "string",
                            },
                        },
                        "title": "B",
                        "type": "object",
                    },
                ],
            },
        },
        "title": "REDEFINES-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_6_source() -> str:
    """
    DDE Test copybook 6 from page 197 with another redefines
    """
    return dedent(
        """
        123456*
              * COPY6.COB
               01  REDEFINES-RECORD.
                   05 NAME-2.
                      10 SALARY PICTURE XXX.
                      10 SO-SEC-NO PICTURE X(9).
                      10 MONTH PICTURE XX.
                   05 NAME-1 REDEFINES NAME-2.
                      10 WAGE PICTURE 999V999.
                      10 EMP-NO PICTURE X(6).
                      10 YEAR PICTURE XX.

        """
    )


def test_9(copy_6_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_6_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "properties": {
            "NAME-1": {
                "$ref": "#NAME-1",
                "cobol": "05 NAME-1 REDEFINES NAME-2",
                "title": "NAME-1",
            },
            "NAME-2": {"$ref": "#NAME-2", "cobol": "05 NAME-2", "title": "NAME-2"},
            "REDEFINES-NAME-2": {
                "$anchor": "REDEFINES-NAME-2",
                "oneOf": [
                    {
                        "$anchor": "NAME-2",
                        "cobol": "05 NAME-2",
                        "properties": {
                            "MONTH": {
                                "$anchor": "MONTH",
                                "cobol": "10 " "MONTH " "PICTURE " "XX",
                                "contentEncoding": "cp037",
                                "title": "MONTH",
                                "type": "string",
                            },
                            "SALARY": {
                                "$anchor": "SALARY",
                                "cobol": "10 " "SALARY " "PICTURE " "XXX",
                                "contentEncoding": "cp037",
                                "title": "SALARY",
                                "type": "string",
                            },
                            "SO-SEC-NO": {
                                "$anchor": "SO-SEC-NO",
                                "cobol": "10 " "SO-SEC-NO " "PICTURE " "X(9)",
                                "contentEncoding": "cp037",
                                "title": "SO-SEC-NO",
                                "type": "string",
                            },
                        },
                        "title": "NAME-2",
                        "type": "object",
                    },
                    {
                        "$anchor": "NAME-1",
                        "cobol": "05 NAME-1 REDEFINES " "NAME-2",
                        "properties": {
                            "EMP-NO": {
                                "$anchor": "EMP-NO",
                                "cobol": "10 " "EMP-NO " "PICTURE " "X(6)",
                                "contentEncoding": "cp037",
                                "title": "EMP-NO",
                                "type": "string",
                            },
                            "WAGE": {
                                "$anchor": "WAGE",
                                "cobol": "10 " "WAGE " "PICTURE " "999V999",
                                "contentEncoding": "cp037",
                                "conversion": "decimal",
                                "title": "WAGE",
                                "type": "string",
                            },
                            "YEAR": {
                                "$anchor": "YEAR",
                                "cobol": "10 " "YEAR " "PICTURE " "XX",
                                "contentEncoding": "cp037",
                                "title": "YEAR",
                                "type": "string",
                            },
                        },
                        "title": "NAME-1",
                        "type": "object",
                    },
                ],
            },
        },
        "title": "REDEFINES-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_7_source() -> str:
    """
    DDE Test copybook 7 from page 198, example "A"
    """
    return dedent(
        """
        123456*
              * COPY7.COB
               01  REDEFINES-RECORD.
                   05 REGULAR-EMPLOYEE.
                      10 LOCATION PICTURE A(8).
                      10 GRADE PICTURE X(4).
                      10 SEMI-MONTHLY-PAY PICTURE 9999V99.
                      10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY
                          PICTURE 999V999.
                   05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
                      10 LOCATION PICTURE A(8).
                      10 FILLER PICTURE X(6).
                      10 HOURLY-PAY PICTURE 99V99.
        """
    )


def test_10(copy_7_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_7_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "properties": {
            "REDEFINES-REGULAR-EMPLOYEE": {
                "$anchor": "REDEFINES-REGULAR-EMPLOYEE",
                "oneOf": [
                    {
                        "$anchor": "REGULAR-EMPLOYEE",
                        "cobol": "05 " "REGULAR-EMPLOYEE",
                        "properties": {
                            "GRADE": {
                                "$anchor": "GRADE",
                                "cobol": "10 " "GRADE " "PICTURE " "X(4)",
                                "contentEncoding": "cp037",
                                "title": "GRADE",
                                "type": "string",
                            },
                            "LOCATION": {
                                "$anchor": "LOCATION",
                                "cobol": "10 " "LOCATION " "PICTURE " "A(8)",
                                "contentEncoding": "cp037",
                                "title": "LOCATION",
                                "type": "string",
                            },
                            "REDEFINES-SEMI-MONTHLY-PAY": {
                                "$anchor": "REDEFINES-SEMI-MONTHLY-PAY",
                                "oneOf": [
                                    {
                                        "$anchor": "SEMI-MONTHLY-PAY",
                                        "cobol": "10 "
                                                 "SEMI-MONTHLY-PAY "
                                                 "PICTURE "
                                                 "9999V99",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "SEMI-MONTHLY-PAY",
                                        "type": "string",
                                    },
                                    {
                                        "$anchor": "WEEKLY-PAY",
                                        "cobol": "10 "
                                                 "WEEKLY-PAY "
                                                 "REDEFINES "
                                                 "SEMI-MONTHLY-PAY "
                                                 "PICTURE "
                                                 "999V999",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "WEEKLY-PAY",
                                        "type": "string",
                                    },
                                ],
                            },
                            "SEMI-MONTHLY-PAY": {
                                "$ref": "#SEMI-MONTHLY-PAY",
                                "cobol": "10 " "SEMI-MONTHLY-PAY " "PICTURE " "9999V99",
                                "title": "SEMI-MONTHLY-PAY",
                            },
                            "WEEKLY-PAY": {
                                "$ref": "#WEEKLY-PAY",
                                "cobol": "10 "
                                         "WEEKLY-PAY "
                                         "REDEFINES "
                                         "SEMI-MONTHLY-PAY "
                                         "PICTURE "
                                         "999V999",
                                "title": "WEEKLY-PAY",
                            },
                        },
                        "title": "REGULAR-EMPLOYEE",
                        "type": "object",
                    },
                    {
                        "$anchor": "TEMPORARY-EMPLOYEE",
                        "cobol": "05 "
                                 "TEMPORARY-EMPLOYEE "
                                 "REDEFINES "
                                 "REGULAR-EMPLOYEE",
                        "properties": {
                            "FILLER-1": {
                                "$anchor": "FILLER-1",
                                "cobol": "10 " "FILLER " "PICTURE " "X(6)",
                                "contentEncoding": "cp037",
                                "title": "FILLER",
                                "type": "string",
                            },
                            "HOURLY-PAY": {
                                "$anchor": "HOURLY-PAY",
                                "cobol": "10 " "HOURLY-PAY " "PICTURE " "99V99",
                                "contentEncoding": "cp037",
                                "conversion": "decimal",
                                "title": "HOURLY-PAY",
                                "type": "string",
                            },
                            "LOCATION": {
                                "$anchor": "LOCATION",
                                "cobol": "10 " "LOCATION " "PICTURE " "A(8)",
                                "contentEncoding": "cp037",
                                "title": "LOCATION",
                                "type": "string",
                            },
                        },
                        "title": "TEMPORARY-EMPLOYEE",
                        "type": "object",
                    },
                ],
            },
            "REGULAR-EMPLOYEE": {
                "$ref": "#REGULAR-EMPLOYEE",
                "cobol": "05 REGULAR-EMPLOYEE",
                "title": "REGULAR-EMPLOYEE",
            },
            "TEMPORARY-EMPLOYEE": {
                "$ref": "#TEMPORARY-EMPLOYEE",
                "cobol": "05 TEMPORARY-EMPLOYEE " "REDEFINES REGULAR-EMPLOYEE",
                "title": "TEMPORARY-EMPLOYEE",
            },
        },
        "title": "REDEFINES-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_8_source() -> str:
    """
    DDE Test copybook 8 from page 198, example "B"
    """
    return dedent(
        """
        123456*
              * COPY8.COB
               01  REDEFINES-RECORD.
                   05 REGULAR-EMPLOYEE.
                       10 LOCATION PICTURE A(8).
                       10 GRADE PICTURE X(4).
                       10 SEMI-MONTHLY-PAY PICTURE 999V999.
                   05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
                       10 LOCATION PICTURE A(8).
                       10 FILLER PICTURE X(6).
                       10 HOURLY-PAY PICTURE 99V99.
                       10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999.
        """
    )


def test_11(copy_8_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_8_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "properties": {
            "REDEFINES-REGULAR-EMPLOYEE": {
                "$anchor": "REDEFINES-REGULAR-EMPLOYEE",
                "oneOf": [
                    {
                        "$anchor": "REGULAR-EMPLOYEE",
                        "cobol": "05 " "REGULAR-EMPLOYEE",
                        "properties": {
                            "GRADE": {
                                "$anchor": "GRADE",
                                "cobol": "10 " "GRADE " "PICTURE " "X(4)",
                                "contentEncoding": "cp037",
                                "title": "GRADE",
                                "type": "string",
                            },
                            "LOCATION": {
                                "$anchor": "LOCATION",
                                "cobol": "10 " "LOCATION " "PICTURE " "A(8)",
                                "contentEncoding": "cp037",
                                "title": "LOCATION",
                                "type": "string",
                            },
                            "SEMI-MONTHLY-PAY": {
                                "$anchor": "SEMI-MONTHLY-PAY",
                                "cobol": "10 " "SEMI-MONTHLY-PAY " "PICTURE " "999V999",
                                "contentEncoding": "cp037",
                                "conversion": "decimal",
                                "title": "SEMI-MONTHLY-PAY",
                                "type": "string",
                            },
                        },
                        "title": "REGULAR-EMPLOYEE",
                        "type": "object",
                    },
                    {
                        "$anchor": "TEMPORARY-EMPLOYEE",
                        "cobol": "05 "
                                 "TEMPORARY-EMPLOYEE "
                                 "REDEFINES "
                                 "REGULAR-EMPLOYEE",
                        "properties": {
                            "CODE-H": {
                                "$ref": "#CODE-H",
                                "cobol": "10 "
                                         "CODE-H "
                                         "REDEFINES "
                                         "HOURLY-PAY "
                                         "PICTURE "
                                         "9999",
                                "title": "CODE-H",
                            },
                            "FILLER-1": {
                                "$anchor": "FILLER-1",
                                "cobol": "10 " "FILLER " "PICTURE " "X(6)",
                                "contentEncoding": "cp037",
                                "title": "FILLER",
                                "type": "string",
                            },
                            "HOURLY-PAY": {
                                "$ref": "#HOURLY-PAY",
                                "cobol": "10 " "HOURLY-PAY " "PICTURE " "99V99",
                                "title": "HOURLY-PAY",
                            },
                            "LOCATION": {
                                "$anchor": "LOCATION",
                                "cobol": "10 " "LOCATION " "PICTURE " "A(8)",
                                "contentEncoding": "cp037",
                                "title": "LOCATION",
                                "type": "string",
                            },
                            "REDEFINES-HOURLY-PAY": {
                                "$anchor": "REDEFINES-HOURLY-PAY",
                                "oneOf": [
                                    {
                                        "$anchor": "HOURLY-PAY",
                                        "cobol": "10 " "HOURLY-PAY " "PICTURE " "99V99",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "HOURLY-PAY",
                                        "type": "string",
                                    },
                                    {
                                        "$anchor": "CODE-H",
                                        "cobol": "10 "
                                                 "CODE-H "
                                                 "REDEFINES "
                                                 "HOURLY-PAY "
                                                 "PICTURE "
                                                 "9999",
                                        "contentEncoding": "cp037",
                                        "conversion": "decimal",
                                        "title": "CODE-H",
                                        "type": "string",
                                    },
                                ],
                            },
                        },
                        "title": "TEMPORARY-EMPLOYEE",
                        "type": "object",
                    },
                ],
            },
            "REGULAR-EMPLOYEE": {
                "$ref": "#REGULAR-EMPLOYEE",
                "cobol": "05 REGULAR-EMPLOYEE",
                "title": "REGULAR-EMPLOYEE",
            },
            "TEMPORARY-EMPLOYEE": {
                "$ref": "#TEMPORARY-EMPLOYEE",
                "cobol": "05 TEMPORARY-EMPLOYEE " "REDEFINES REGULAR-EMPLOYEE",
                "title": "TEMPORARY-EMPLOYEE",
            },
        },
        "title": "REDEFINES-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_9_source() -> str:
    """
    Some basic COBOL with two top-level records that use a
    REDEFINES.
    A REDEFINES on an 01 level is essentially irrelevant, but still valid.
    """
    return dedent(
        """
        123456*
              * COPY9.COB
               01  DETAIL-LINE.
                   05  QUESTION                    PIC ZZ.
                   05  PRINT-YES                   PIC ZZ.
                   05  PRINT-NO                    PIC ZZ.
                   05  NOT-SURE                    PIC ZZ.
               01  SUMMARY-LINE REDEFINES DETAIL-LINE.
                   05  COUNT                       PIC ZZ.
                   05  FILLER                      PIC XX.
                   05  FILLER                      PIC XX.
                   05  FILLER                      PIC XX.
        """
    )


def test_12(copy_9_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_9_source))))
    schema_list = list(JSONSchemaMaker(record).jsonschema() for record in copy_book)
    for schema in schema_list:
        assert SchemaValidator.check_schema(schema) == None
    assert schema_list[0] == {
        "$anchor": "DETAIL-LINE",
        "cobol": "01 DETAIL-LINE",
        "properties": {
            "NOT-SURE": {
                "$anchor": "NOT-SURE",
                "cobol": "05 NOT-SURE PIC ZZ",
                "contentEncoding": "cp037",
                "title": "NOT-SURE",
                "type": "string",
            },
            "PRINT-NO": {
                "$anchor": "PRINT-NO",
                "cobol": "05 PRINT-NO PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-NO",
                "type": "string",
            },
            "PRINT-YES": {
                "$anchor": "PRINT-YES",
                "cobol": "05 PRINT-YES PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-YES",
                "type": "string",
            },
            "QUESTION": {
                "$anchor": "QUESTION",
                "cobol": "05 QUESTION PIC ZZ",
                "contentEncoding": "cp037",
                "title": "QUESTION",
                "type": "string",
            },
        },
        "title": "DETAIL-LINE",
        "type": "object",
    }
    assert schema_list[1] == {
        "$anchor": "SUMMARY-LINE",
        "cobol": "01 SUMMARY-LINE REDEFINES DETAIL-LINE",
        "properties": {
            "COUNT": {
                "$anchor": "COUNT",
                "cobol": "05 COUNT PIC ZZ",
                "contentEncoding": "cp037",
                "title": "COUNT",
                "type": "string",
            },
            "FILLER-1": {
                "$anchor": "FILLER-1",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-2": {
                "$anchor": "FILLER-2",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-3": {
                "$anchor": "FILLER-3",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
        },
        "title": "SUMMARY-LINE",
        "type": "object",
    }


@pytest.fixture
def copy_10_source() -> str:
    return dedent(
        """
        123456*
              * COPY10.COB
               01  MAIN-AREA.
                   03 REC-1.
                      05 FIELD-1                       PIC 9.
                      05 FIELD-2 OCCURS 1 TO 5 TIMES
                         DEPENDING ON FIELD-1        PIC X(05).
        """
    )


def test_13(copy_10_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_10_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "MAIN-AREA",
        "cobol": "01 MAIN-AREA",
        "properties": {
            "REC-1": {
                "$anchor": "REC-1",
                "cobol": "03 REC-1",
                "properties": {
                    "FIELD-1": {
                        "$anchor": "FIELD-1",
                        "cobol": "05 FIELD-1 PIC " "9",
                        "contentEncoding": "cp037",
                        "conversion": "decimal",
                        "title": "FIELD-1",
                        "type": "string",
                    },
                    "FIELD-2": {
                        "$anchor": "FIELD-2",
                        "cobol": "05 FIELD-2 "
                                 "OCCURS 1 TO 5 "
                                 "TIMES DEPENDING "
                                 "ON FIELD-1 PIC "
                                 "X(05)",
                        "items": {
                            "properties": {
                                "FIELD-2": {
                                    "cobol": "05 "
                                             "FIELD-2 "
                                             "OCCURS "
                                             "1 "
                                             "TO "
                                             "5 "
                                             "TIMES "
                                             "DEPENDING "
                                             "ON "
                                             "FIELD-1 "
                                             "PIC "
                                             "X(05)",
                                    "contentEncoding": "cp037",
                                    "type": "string",
                                }
                            },
                            "type": "object",
                        },
                        "maxItemsDependsOn": {"$ref": "#FIELD-1"},
                        "title": "FIELD-2",
                        "type": "array",
                    },
                },
                "title": "REC-1",
                "type": "object",
            }
        },
        "title": "MAIN-AREA",
        "type": "object",
    }


@pytest.fixture
def copy_11_source() -> str:
    return dedent(
        """
        123456*
              * COPY11.COB
               01  MAIN-AREA.
                   03 REC-1.
                      05 FIELD-1                       PIC 9.
                      05 FIELD-3                       PIC 9.
                      05 FIELD-2 OCCURS 1 TO 5 TIMES
                           DEPENDING ON FIELD-1        PIC X(05).
                   03 REC-2.
                      05 FIELD-4 OCCURS 1 TO 5 TIMES
                           DEPENDING ON FIELD-3        PIC X(05).

        """
    )


def test_14(copy_11_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_11_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "MAIN-AREA",
        "cobol": "01 MAIN-AREA",
        "properties": {
            "REC-1": {
                "$anchor": "REC-1",
                "cobol": "03 REC-1",
                "properties": {
                    "FIELD-1": {
                        "$anchor": "FIELD-1",
                        "cobol": "05 FIELD-1 PIC " "9",
                        "contentEncoding": "cp037",
                        "conversion": "decimal",
                        "title": "FIELD-1",
                        "type": "string",
                    },
                    "FIELD-2": {
                        "$anchor": "FIELD-2",
                        "cobol": "05 FIELD-2 "
                                 "OCCURS 1 TO 5 "
                                 "TIMES DEPENDING "
                                 "ON FIELD-1 PIC "
                                 "X(05)",
                        "items": {
                            "properties": {
                                "FIELD-2": {
                                    "cobol": "05 "
                                             "FIELD-2 "
                                             "OCCURS "
                                             "1 "
                                             "TO "
                                             "5 "
                                             "TIMES "
                                             "DEPENDING "
                                             "ON "
                                             "FIELD-1 "
                                             "PIC "
                                             "X(05)",
                                    "contentEncoding": "cp037",
                                    "type": "string",
                                }
                            },
                            "type": "object",
                        },
                        "maxItemsDependsOn": {"$ref": "#FIELD-1"},
                        "title": "FIELD-2",
                        "type": "array",
                    },
                    "FIELD-3": {
                        "$anchor": "FIELD-3",
                        "cobol": "05 FIELD-3 PIC " "9",
                        "contentEncoding": "cp037",
                        "conversion": "decimal",
                        "title": "FIELD-3",
                        "type": "string",
                    },
                },
                "title": "REC-1",
                "type": "object",
            },
            "REC-2": {
                "$anchor": "REC-2",
                "cobol": "03 REC-2",
                "properties": {
                    "FIELD-4": {
                        "$anchor": "FIELD-4",
                        "cobol": "05 FIELD-4 "
                                 "OCCURS 1 TO 5 "
                                 "TIMES DEPENDING "
                                 "ON FIELD-3 PIC "
                                 "X(05)",
                        "items": {
                            "properties": {
                                "FIELD-4": {
                                    "cobol": "05 "
                                             "FIELD-4 "
                                             "OCCURS "
                                             "1 "
                                             "TO "
                                             "5 "
                                             "TIMES "
                                             "DEPENDING "
                                             "ON "
                                             "FIELD-3 "
                                             "PIC "
                                             "X(05)",
                                    "contentEncoding": "cp037",
                                    "type": "string",
                                }
                            },
                            "type": "object",
                        },
                        "maxItemsDependsOn": {"$ref": "#FIELD-3"},
                        "title": "FIELD-4",
                        "type": "array",
                    }
                },
                "title": "REC-2",
                "type": "object",
            },
        },
        "title": "MAIN-AREA",
        "type": "object",
    }


@pytest.fixture
def copy_12_source() -> str:
    """See copy_9_source for a similar test."""
    return dedent(
        """
        123456*
              * COPY12.COB
               01  DETAIL-LINE.
                   05  QUESTION                    PIC ZZ.
                   05  PRINT-YES                   PIC ZZ.
                   05  PRINT-NO                    PIC ZZ.
                   05  NOT-SURE                    PIC ZZ.
               01  SUMMARY-LINE.
                   05  COUNT                       PIC ZZ.
                   05  FILLER                      PIC XX.
                   05  FILLER                      PIC XX.
                   05  FILLER                      PIC XX.
        """
    )


def test_15(copy_12_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_12_source))))
    schema_list = list(JSONSchemaMaker(record).jsonschema() for record in copy_book)
    for schema in schema_list:
        assert SchemaValidator.check_schema(schema) == None
    assert schema_list[0] == {
        "$anchor": "DETAIL-LINE",
        "cobol": "01 DETAIL-LINE",
        "properties": {
            "NOT-SURE": {
                "$anchor": "NOT-SURE",
                "cobol": "05 NOT-SURE PIC ZZ",
                "contentEncoding": "cp037",
                "title": "NOT-SURE",
                "type": "string",
            },
            "PRINT-NO": {
                "$anchor": "PRINT-NO",
                "cobol": "05 PRINT-NO PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-NO",
                "type": "string",
            },
            "PRINT-YES": {
                "$anchor": "PRINT-YES",
                "cobol": "05 PRINT-YES PIC ZZ",
                "contentEncoding": "cp037",
                "title": "PRINT-YES",
                "type": "string",
            },
            "QUESTION": {
                "$anchor": "QUESTION",
                "cobol": "05 QUESTION PIC ZZ",
                "contentEncoding": "cp037",
                "title": "QUESTION",
                "type": "string",
            },
        },
        "title": "DETAIL-LINE",
        "type": "object",
    }
    assert schema_list[1] == {
        "$anchor": "SUMMARY-LINE",
        "cobol": "01 SUMMARY-LINE",
        "properties": {
            "COUNT": {
                "$anchor": "COUNT",
                "cobol": "05 COUNT PIC ZZ",
                "contentEncoding": "cp037",
                "title": "COUNT",
                "type": "string",
            },
            "FILLER-1": {
                "$anchor": "FILLER-1",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-2": {
                "$anchor": "FILLER-2",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
            "FILLER-3": {
                "$anchor": "FILLER-3",
                "cobol": "05 FILLER PIC XX",
                "contentEncoding": "cp037",
                "title": "FILLER",
                "type": "string",
            },
        },
        "title": "SUMMARY-LINE",
        "type": "object",
    }


@pytest.fixture
def copy_13_source() -> str:
    """Note that two records only overlap with the GENERIC-FIELD of the GENERIC-RECORD."""
    return dedent(
        """
        123456*
              * COPY13.COB
               01  GENERIC-RECORD.
                   05 HEADER PIC X(3).
                   05 GENERIC-FIELD PIC X(17).

               01 ABC-SPECIFIC-RECORD.
                   05 ITEM-1 PIC X(10).
                   05 ITEM-2 PIC X(7).

               01 DEF-ANOTHER-RECORD.
                   05 ITEM-3 PIC X(7).
                   05 ITEM-4 PIC X(10).
        """
    )


def test_16(copy_13_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_13_source))))
    schema_list = list(JSONSchemaMaker(record).jsonschema() for record in copy_book)
    for schema in schema_list:
        assert SchemaValidator.check_schema(schema) == None
    assert schema_list[0] == {
        "$anchor": "GENERIC-RECORD",
        "cobol": "01 GENERIC-RECORD",
        "properties": {
            "GENERIC-FIELD": {
                "$anchor": "GENERIC-FIELD",
                "cobol": "05 GENERIC-FIELD PIC X(17)",
                "contentEncoding": "cp037",
                "title": "GENERIC-FIELD",
                "type": "string",
            },
            "HEADER": {
                "$anchor": "HEADER",
                "cobol": "05 HEADER PIC X(3)",
                "contentEncoding": "cp037",
                "title": "HEADER",
                "type": "string",
            },
        },
        "title": "GENERIC-RECORD",
        "type": "object",
    }
    assert schema_list[1] == {
        "$anchor": "ABC-SPECIFIC-RECORD",
        "cobol": "01 ABC-SPECIFIC-RECORD",
        "properties": {
            "ITEM-1": {
                "$anchor": "ITEM-1",
                "cobol": "05 ITEM-1 PIC X(10)",
                "contentEncoding": "cp037",
                "title": "ITEM-1",
                "type": "string",
            },
            "ITEM-2": {
                "$anchor": "ITEM-2",
                "cobol": "05 ITEM-2 PIC X(7)",
                "contentEncoding": "cp037",
                "title": "ITEM-2",
                "type": "string",
            },
        },
        "title": "ABC-SPECIFIC-RECORD",
        "type": "object",
    }
    assert schema_list[2] == {
        "$anchor": "DEF-ANOTHER-RECORD",
        "cobol": "01 DEF-ANOTHER-RECORD",
        "properties": {
            "ITEM-3": {
                "$anchor": "ITEM-3",
                "cobol": "05 ITEM-3 PIC X(7)",
                "contentEncoding": "cp037",
                "title": "ITEM-3",
                "type": "string",
            },
            "ITEM-4": {
                "$anchor": "ITEM-4",
                "cobol": "05 ITEM-4 PIC X(10)",
                "contentEncoding": "cp037",
                "title": "ITEM-4",
                "type": "string",
            },
        },
        "title": "DEF-ANOTHER-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_14_source() -> str:
    """
    This is from a bug report for fields which couldn't be parsed
    properly. The ``SV9(n)`` is rarely used. The bug was that it created something that
    baffled the COMP-3 conversion.
    """
    return dedent(
        """
        123456*
              * COPY14.COB
               01  GENERIC-RECORD.
                      04 NUMBER-1     PIC SV9(5)   COMP-3.
                      04 NUMBER-2     PIC SV9(5)   COMP-3.
                      04 NUMBER-3     PIC SV9(05)  COMP-3.
                      04 NUMBER-4     PIC SV9(5)   COMP-3.
        """
    )


def test_17(copy_14_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_14_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "GENERIC-RECORD",
        "cobol": "01 GENERIC-RECORD",
        "properties": {
            "NUMBER-1": {
                "$anchor": "NUMBER-1",
                "cobol": "04 NUMBER-1 PIC SV9(5) COMP-3",
                "contentEncoding": "packed-decimal",
                "conversion": "decimal",
                "title": "NUMBER-1",
                "type": "string",
            },
            "NUMBER-2": {
                "$anchor": "NUMBER-2",
                "cobol": "04 NUMBER-2 PIC SV9(5) COMP-3",
                "contentEncoding": "packed-decimal",
                "conversion": "decimal",
                "title": "NUMBER-2",
                "type": "string",
            },
            "NUMBER-3": {
                "$anchor": "NUMBER-3",
                "cobol": "04 NUMBER-3 PIC SV9(05) COMP-3",
                "contentEncoding": "packed-decimal",
                "conversion": "decimal",
                "title": "NUMBER-3",
                "type": "string",
            },
            "NUMBER-4": {
                "$anchor": "NUMBER-4",
                "cobol": "04 NUMBER-4 PIC SV9(5) COMP-3",
                "contentEncoding": "packed-decimal",
                "conversion": "decimal",
                "title": "NUMBER-4",
                "type": "string",
            },
        },
        "title": "GENERIC-RECORD",
        "type": "object",
    }


@pytest.fixture
def copy_l2_source() -> str:
    return dedent(
        """
        123456*
              * COPY2.COB
               01  DETAIL-LINE.
                   05  FANCY-FORMAT                PIC 9(7).99.
                   05  SSN-FORMAT                  PIC 999-99-9999.
        """
    )


def test_18(copy_l2_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_l2_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        '$anchor': 'DETAIL-LINE',
        'cobol': '01 DETAIL-LINE',
        'properties': {'FANCY-FORMAT': {'$anchor': 'FANCY-FORMAT',
                                        'cobol': '05 FANCY-FORMAT PIC 9(7).99',
                                        'contentEncoding': 'cp037',
                                        'title': 'FANCY-FORMAT',
                                        'type': 'string'},
                       'SSN-FORMAT': {'$anchor': 'SSN-FORMAT',
                                      'cobol': '05 SSN-FORMAT PIC 999-99-9999',
                                      'contentEncoding': 'cp037',
                                      'title': 'SSN-FORMAT',
                                      'type': 'string'}},
        'title': 'DETAIL-LINE',
        'type': 'object'}


@pytest.fixture
def copy_l3_source() -> str:
    return dedent(
        """
        123456*
              * COPY3.COB
               01  SIMPLE-LINE.
                   05  VALUE-1                PIC X(10) VALUE 'STRING    '.
                   05  VALUE-2                PIC X(10) VALUE "STRING    ".
        """
    )


def test_19(copy_l3_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(copy_l3_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {'$anchor': 'SIMPLE-LINE',
                      'cobol': '01 SIMPLE-LINE',
                      'properties': {'VALUE-1': {'$anchor': 'VALUE-1',
                                                 'cobol': "05 VALUE-1 PIC X(10) VALUE 'STRING '",
                                                 'contentEncoding': 'cp037',
                                                 'title': 'VALUE-1',
                                                 'type': 'string'},
                                     'VALUE-2': {'$anchor': 'VALUE-2',
                                                 'cobol': '05 VALUE-2 PIC X(10) VALUE "STRING "',
                                                 'contentEncoding': 'cp037',
                                                 'title': 'VALUE-2',
                                                 'type': 'string'}},
                      'title': 'SIMPLE-LINE',
                      'type': 'object'}


@pytest.fixture
def copy_l4_source() -> str:
    return dedent(
        """
        123456*
              * COPY4.COB
               01  'XY'-SIMPLE-LINE.
                   05  'XY'-VALUE-1                PIC X(10) VALUE 'STRING    '.
                   05  'XY'-VALUE-2                PIC X(10) VALUE "STRING    ".
        """
    )


def test_20(copy_l4_source: str) -> None:
    source = StringIO(copy_l4_source)
    copy_book = structure(dde_sentences(reference_format(source, replacing=[("'XY'", 'AB')])))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {'$anchor': 'AB-SIMPLE-LINE',
                      'cobol': '01 AB-SIMPLE-LINE',
                      'properties': {'AB-VALUE-1': {'$anchor': 'AB-VALUE-1',
                                                    'cobol': "05 AB-VALUE-1 PIC X(10) VALUE 'STRING "
                                                             "'",
                                                    'contentEncoding': 'cp037',
                                                    'title': 'AB-VALUE-1',
                                                    'type': 'string'}},
                      'title': 'AB-SIMPLE-LINE',
                      'type': 'object'}

@pytest.fixture
def all_types_source() -> str:
    return dedent(
        """
        123456*
              * COPYALLTYPES.COB
               01  ALL-TYPES.
                   05  FLOAT-A              PIC S9999 COMPUTATIONAL-1.
                   05  FLOAT-B              PIC S9999 COMP-1.
                   05  DOUBLE-A             PIC S9999 COMPUTATIONAL-2.
                   05  DOUBLE-B             PIC S9999 COMP-2.
                   05  DECIMAL-A            PIC S9999 COMPUTATIONAL-3.
                   05  DECIMAL-B            PIC S9999 COMP-3.
                   05  DECIMAL-C            PIC S9999 PACKED-DECIMAL.
                   05  BINARY-A             PIC S9999 COMPUTATIONAL.
                   05  BINARY-B             PIC S9999 COMPUTATIONAL-4.
                   05  BINARY-C             PIC S9999 COMP-4.
                   05  BINARY-D             PIC S9999 BINARY.
                   05  TEXT-A               PIC  9999 DISPLAY.
                   05  TEXT-B               PIC  9999.
                   05  TEXT-C               PIC  XXXX.
        """
    )

def test_schemamaker_all_types(all_types_source: str) -> None:
    source = StringIO(all_types_source)
    copy_book = structure(dde_sentences(reference_format(source)))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        '$anchor': 'ALL-TYPES',
         'cobol': '01 ALL-TYPES',
         'properties': {'BINARY-A': {'$anchor': 'BINARY-A',
                                     'cobol': '05 BINARY-A PIC S9999 COMPUTATIONAL',
                                     'contentEncoding': 'bigendian-int',
                                     'title': 'BINARY-A',
                                     'type': 'integer'},
                        'BINARY-B': {'$anchor': 'BINARY-B',
                                     'cobol': '05 BINARY-B PIC S9999 COMPUTATIONAL-4',
                                     'contentEncoding': 'bigendian-int',
                                     'title': 'BINARY-B',
                                     'type': 'integer'},
                        'BINARY-C': {'$anchor': 'BINARY-C',
                                     'cobol': '05 BINARY-C PIC S9999 COMP-4',
                                     'contentEncoding': 'bigendian-int',
                                     'title': 'BINARY-C',
                                     'type': 'integer'},
                        'BINARY-D': {'$anchor': 'BINARY-D',
                                     'cobol': '05 BINARY-D PIC S9999 BINARY',
                                     'contentEncoding': 'bigendian-int',
                                     'title': 'BINARY-D',
                                     'type': 'integer'},
                        'DECIMAL-A': {'$anchor': 'DECIMAL-A',
                                      'cobol': '05 DECIMAL-A PIC S9999 COMPUTATIONAL-3',
                                      'contentEncoding': 'packed-decimal',
                                      'conversion': 'decimal',
                                      'title': 'DECIMAL-A',
                                      'type': 'string'},
                        'DECIMAL-B': {'$anchor': 'DECIMAL-B',
                                      'cobol': '05 DECIMAL-B PIC S9999 COMP-3',
                                      'contentEncoding': 'packed-decimal',
                                      'conversion': 'decimal',
                                      'title': 'DECIMAL-B',
                                      'type': 'string'},
                        'DECIMAL-C': {'$anchor': 'DECIMAL-C',
                                      'cobol': '05 DECIMAL-C PIC S9999 PACKED-DECIMAL',
                                      'contentEncoding': 'packed-decimal',
                                      'conversion': 'decimal',
                                      'title': 'DECIMAL-C',
                                      'type': 'string'},
                        'DOUBLE-A': {'$anchor': 'DOUBLE-A',
                                     'cobol': '05 DOUBLE-A PIC S9999 COMPUTATIONAL-2',
                                     'contentEncoding': 'bigendian-double',
                                     'title': 'DOUBLE-A',
                                     'type': 'number'},
                        'DOUBLE-B': {'$anchor': 'DOUBLE-B',
                                     'cobol': '05 DOUBLE-B PIC S9999 COMP-2',
                                     'contentEncoding': 'bigendian-double',
                                     'title': 'DOUBLE-B',
                                     'type': 'number'},
                        'FLOAT-A': {'$anchor': 'FLOAT-A',
                                    'cobol': '05 FLOAT-A PIC S9999 COMPUTATIONAL-1',
                                    'contentEncoding': 'bigendian-float',
                                    'title': 'FLOAT-A',
                                    'type': 'number'},
                        'FLOAT-B': {'$anchor': 'FLOAT-B',
                                    'cobol': '05 FLOAT-B PIC S9999 COMP-1',
                                    'contentEncoding': 'bigendian-float',
                                    'title': 'FLOAT-B',
                                    'type': 'number'},
                        'TEXT-A': {'$anchor': 'TEXT-A',
                                   'cobol': '05 TEXT-A PIC 9999 DISPLAY',
                                   'contentEncoding': 'cp037',
                                   'conversion': 'decimal',
                                   'title': 'TEXT-A',
                                   'type': 'string'},
                        'TEXT-B': {'$anchor': 'TEXT-B',
                                   'cobol': '05 TEXT-B PIC 9999',
                                   'contentEncoding': 'cp037',
                                   'conversion': 'decimal',
                                   'title': 'TEXT-B',
                                   'type': 'string'},
                        'TEXT-C': {'$anchor': 'TEXT-C',
                                   'cobol': '05 TEXT-C PIC XXXX',
                                   'contentEncoding': 'cp037',
                                   'title': 'TEXT-C',
                                   'type': 'string'}

                        },
         'title': 'ALL-TYPES',
         'type': 'object'}

def test_extended_schemamaker_all_types(all_types_source: str) -> None:
    source = StringIO(all_types_source)
    copy_book = structure(dde_sentences(reference_format(source)))
    for record in copy_book:
        schema = JSONSchemaMakerExtendedVocabulary(record).jsonschema()
    # There's only one DDE to examine.
    # TODO: Test with an extended vocabulary in JSONSchema Validator
    # assert SchemaValidator.check_schema(schema) == None
    assert schema == {
         '$anchor': 'ALL-TYPES',
         'cobol': '01 ALL-TYPES',
         'properties': {'BINARY-A': {'$anchor': 'BINARY-A',
                                     'cobol': '05 BINARY-A PIC S9999 COMPUTATIONAL',
                                     'title': 'BINARY-A',
                                     'type': 'integer'},
                        'BINARY-B': {'$anchor': 'BINARY-B',
                                     'cobol': '05 BINARY-B PIC S9999 COMPUTATIONAL-4',
                                     'title': 'BINARY-B',
                                     'type': 'integer'},
                        'BINARY-C': {'$anchor': 'BINARY-C',
                                     'cobol': '05 BINARY-C PIC S9999 COMP-4',
                                     'title': 'BINARY-C',
                                     'type': 'integer'},
                        'BINARY-D': {'$anchor': 'BINARY-D',
                                     'cobol': '05 BINARY-D PIC S9999 BINARY',
                                     'title': 'BINARY-D',
                                     'type': 'integer'},
                        'DECIMAL-A': {'$anchor': 'DECIMAL-A',
                                      'cobol': '05 DECIMAL-A PIC S9999 COMPUTATIONAL-3',
                                      'title': 'DECIMAL-A',
                                      'type': 'decimal'},
                        'DECIMAL-B': {'$anchor': 'DECIMAL-B',
                                      'cobol': '05 DECIMAL-B PIC S9999 COMP-3',
                                      'title': 'DECIMAL-B',
                                      'type': 'decimal'},
                        'DECIMAL-C': {'$anchor': 'DECIMAL-C',
                                      'cobol': '05 DECIMAL-C PIC S9999 PACKED-DECIMAL',
                                      'title': 'DECIMAL-C',
                                      'type': 'decimal'},
                        'DOUBLE-A': {'$anchor': 'DOUBLE-A',
                                     'cobol': '05 DOUBLE-A PIC S9999 COMPUTATIONAL-2',
                                     'title': 'DOUBLE-A',
                                     'type': 'number'},
                        'DOUBLE-B': {'$anchor': 'DOUBLE-B',
                                     'cobol': '05 DOUBLE-B PIC S9999 COMP-2',
                                     'title': 'DOUBLE-B',
                                     'type': 'number'},
                        'FLOAT-A': {'$anchor': 'FLOAT-A',
                                    'cobol': '05 FLOAT-A PIC S9999 COMPUTATIONAL-1',
                                    'title': 'FLOAT-A',
                                    'type': 'number'},
                        'FLOAT-B': {'$anchor': 'FLOAT-B',
                                    'cobol': '05 FLOAT-B PIC S9999 COMP-1',
                                    'title': 'FLOAT-B',
                                    'type': 'number'},
                        'TEXT-A': {'$anchor': 'TEXT-A',
                                   'cobol': '05 TEXT-A PIC 9999 DISPLAY',
                                   'title': 'TEXT-A',
                                   'type': 'decimal'},
                        'TEXT-B': {'$anchor': 'TEXT-B',
                                   'cobol': '05 TEXT-B PIC 9999',
                                   'title': 'TEXT-B',
                                   'type': 'decimal'},
                        'TEXT-C': {'$anchor': 'TEXT-C',
                                   'cobol': '05 TEXT-C PIC XXXX',
                                   'title': 'TEXT-C',
                                   'type': 'string'}
                        },
         'title': 'ALL-TYPES',
         'type': 'object'}

@pytest.fixture
def issue_1_source() -> str:
    """
    See https://github.com/slott56/Stingray-Reader/issues/1.

    The reported but was on the :meth:`cobol.model.DDE.__repr__`.
    """
    return dedent(
        """
        123456*
              * ISSUE1.COB
                   02 GROUP-LABL.
                      03 LABL-STDY-GP-AR-CT           PIC 9(00004).
                      03 LABL-STDY-GP-AR
                         OCCURS 0 TO 40 TIMES DEPENDING ON LABL-STDY-GP-AR-CT.
                         05 LABL-PRSN-ID              PIC 9(00009).
                         05 LABL-GNPR-ID              PIC 9(00009).
                         05 LABL-GNRC-ID              PIC 9(00009).
                         05 LABL-GNRC-AT              PIC -9(00012).9(6).
                         05 LABL-GNRC-QY              PIC -9(00015).
                         05 LABL-GNRC-CD              PIC X(00006).
                         05 LABL-GNRC-PT              PIC -9(00003).9(4).
                         05 LABL-GNRC-TX              PIC X(00030).
                         05 LABL-QRY-FILL-50-TX       PIC X(00050).

        """
    )


def test_issue_1(issue_1_source: str) -> None:
    copy_book = structure(dde_sentences(reference_format(StringIO(issue_1_source))))
    for record in copy_book:
        schema = JSONSchemaMaker(record).jsonschema()
    # There's only one DDE to examine.
    assert SchemaValidator.check_schema(schema) == None
    assert schema == {
        "$anchor": "GROUP-LABL",
        "cobol": "02 GROUP-LABL",
        "properties": {
            "LABL-STDY-GP-AR": {
                "$anchor": "LABL-STDY-GP-AR",
                "cobol": "03 LABL-STDY-GP-AR OCCURS 0 TO "
                         "40 TIMES DEPENDING ON "
                         "LABL-STDY-GP-AR-CT",
                "items": {
                    "properties": {
                        "LABL-GNPR-ID": {
                            "$anchor": "LABL-GNPR-ID",
                            "cobol": "05 " "LABL-GNPR-ID " "PIC " "9(00009)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNPR-ID",
                            "type": "string",
                        },
                        "LABL-GNRC-AT": {
                            "$anchor": "LABL-GNRC-AT",
                            "cobol": "05 " "LABL-GNRC-AT " "PIC " "-9(00012).9(6)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-AT",
                            "type": "string",
                        },
                        "LABL-GNRC-CD": {
                            "$anchor": "LABL-GNRC-CD",
                            "cobol": "05 " "LABL-GNRC-CD " "PIC " "X(00006)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-CD",
                            "type": "string",
                        },
                        "LABL-GNRC-ID": {
                            "$anchor": "LABL-GNRC-ID",
                            "cobol": "05 " "LABL-GNRC-ID " "PIC " "9(00009)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-ID",
                            "type": "string",
                        },
                        "LABL-GNRC-PT": {
                            "$anchor": "LABL-GNRC-PT",
                            "cobol": "05 " "LABL-GNRC-PT " "PIC " "-9(00003).9(4)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-PT",
                            "type": "string",
                        },
                        "LABL-GNRC-QY": {
                            "$anchor": "LABL-GNRC-QY",
                            "cobol": "05 " "LABL-GNRC-QY " "PIC " "-9(00015)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-QY",
                            "type": "string",
                        },
                        "LABL-GNRC-TX": {
                            "$anchor": "LABL-GNRC-TX",
                            "cobol": "05 " "LABL-GNRC-TX " "PIC " "X(00030)",
                            "contentEncoding": "cp037",
                            "title": "LABL-GNRC-TX",
                            "type": "string",
                        },
                        "LABL-PRSN-ID": {
                            "$anchor": "LABL-PRSN-ID",
                            "cobol": "05 " "LABL-PRSN-ID " "PIC " "9(00009)",
                            "contentEncoding": "cp037",
                            "title": "LABL-PRSN-ID",
                            "type": "string",
                        },
                        "LABL-QRY-FILL-50-TX": {
                            "$anchor": "LABL-QRY-FILL-50-TX",
                            "cobol": "05 " "LABL-QRY-FILL-50-TX " "PIC " "X(00050)",
                            "contentEncoding": "cp037",
                            "title": "LABL-QRY-FILL-50-TX",
                            "type": "string",
                        },
                    },
                    "type": "object",
                },
                "maxItemsDependsOn": {"$ref": "#LABL-STDY-GP-AR-CT"},
                "title": "LABL-STDY-GP-AR",
                "type": "array",
            },
            "LABL-STDY-GP-AR-CT": {
                "$anchor": "LABL-STDY-GP-AR-CT",
                "cobol": "03 LABL-STDY-GP-AR-CT PIC " "9(00004)",
                "contentEncoding": "cp037",
                "title": "LABL-STDY-GP-AR-CT",
                "type": "string",
            },
        },
        "title": "GROUP-LABL",
        "type": "object",
    }

