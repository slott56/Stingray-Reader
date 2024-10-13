"""
Reading COBOL Files

For sample data, we're using data found here:
http://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp

The data files are in two zip archives: http://wonder.cdc.gov/wonder/sci_data/datasets/zipctyA.zip
and http://wonder.cdc.gov/wonder/sci_data/datasets/zipctyB.zip

Each of these archives contains five large files, with 2,310,000 rows of data, plus a header.
The 10th file has 2,037,944 rows of data plus a header.

The member names of the ZIP archive are zipcty1 to zipcty5
and zipcty6 to zipcty10.

We'll work with two small subsets in the sample directory.

Here are the two record layouts.

..    parsed-literal::

    COUNTY CROSS-REFERENCE FILE - COBOL EXAMPLE


            BLOCK CONTAINS 0 RECORDS
            LABEL RECORDS ARE STANDARD
            RECORD CONTAINS 53 CHARACTERS
            RECORDING MODE IS F
            DATA RECORDS ARE
                   COUNTY-CROSS-REFERENCE-RECORD.

        01  COUNTY-CROSS-REFERENCE-RECORD.
            05   ZIP-CODE                                 PIC X(05).
            05   UPDATE-KEY-NO                            PIC X(10).
            05   ZIP-ADD-ON-RANGE.
                 10  ZIP-ADD-ON-LOW-NO.
                      15  ZIP-SECTOR-NO                   PIC X(02).
                      15  ZIP-SEGMENT-NO                  PIC X(02).
                 10  ZIP-ADD-ON-HIGH-NO.
                      15  ZIP-SECTOR-NO                   PIC X(02).
                      15  ZIP-SEGMENT-NO                  PIC X(02).
            05   STATE-ABBREV                             PIC X(02).
            05   COUNTY-NO                                PIC X(03).
            05   COUNTY-NAME                              PIC X(25).

..    parsed-literal::

    COPYRIGHT HEADER RECORD - COBOL EXAMPLE


               BLOCK CONTAINS 0 RECORDS
               LABEL RECORDS ARE STANDARD
               RECORD CONTAINS 53 CHARACTERS
               RECORDING MODE IS F
               DATA RECORDS ARE
                   COPYRIGHT-HEADER RECORD.

          01  COPYRIGHT-HEADER-RECORD.
              05  FILLER                                     PIC  X(05).
              05  FILE-VERSION-YEAR                          PIC  X(02).
              05  FILE-VERSION-MONTH                         PIC  X(02).
              05  COPYRIGHT-SYMBOL                           PIC  X(11).
              05  TAPE-SEQUENCE-NO                           PIC  X(03).
              05  FILLER                                     PIC  X(30).
"""

import argparse
from collections import Counter
import logging
from pathlib import Path
from pprint import pformat
import sys
from typing import Any

from stingray.workbook import (
    COBOL_Text_File,
    schema_iter,
    SchemaMaker,
    Sheet,
    Row,
    Schema,
)

# When working with unknown files, we sometimes need to preview the data.


def raw_dump(sheet: Sheet) -> None:
    for row in sheet.rows():
        row.dump()


def header_builder(row: Row) -> dict[str, Any]:
    return {
        "file_version_year": row["FILE-VERSION-YEAR"].value(),
        "file_version_month": row["FILE-VERSION-MONTH"].value(),
        "copyright_symbol": row["COPYRIGHT-SYMBOL"].value(),
        "tape_sequence_no": row["TAPE-SEQUENCE-NO"].value(),
    }


# Because the names within the COBOL layout are not unique at the bottom-most
# element level, we must use path names. The default path names include all
# levels of the DDE. More clever path name components might be useful here.
#
# COBOL uses an "of" to work up the hierarchy looking for a unique name.


def detail_builder(row: Row) -> dict[str, Any]:
    return {
        "zip_code": row["ZIP-CODE"].value(),
        "update_key_no": row["UPDATE-KEY-NO"].value(),
        "low_sector": row["ZIP-ADD-ON-RANGE"]["ZIP-ADD-ON-LOW-NO"][
            "ZIP-SECTOR-NO"
        ].value(),
        "low_segment": row["ZIP-ADD-ON-RANGE"]["ZIP-ADD-ON-LOW-NO"][
            "ZIP-SEGMENT-NO"
        ].value(),
        "high_sector": row["ZIP-ADD-ON-RANGE"]["ZIP-ADD-ON-HIGH-NO"][
            "ZIP-SECTOR-NO"
        ].value(),
        "high_segment": row["ZIP-ADD-ON-RANGE"]["ZIP-ADD-ON-HIGH-NO"][
            "ZIP-SEGMENT-NO"
        ].value(),
        "state_abbrev": row["STATE-ABBREV"].value(),
        "county_no": row["COUNTY-NO"].value(),
        "county_name": row["COUNTY-NAME"].value(),
    }


def process_sheet(sheet: Sheet, schema_1: Schema, schema_2: Schema) -> Counter:
    counts = Counter()
    row_iter = sheet.rows()
    sheet.set_schema(schema_2)
    row = next(row_iter)
    try:
        header = header_builder(row)
    except KeyError as e:
        print(repr(e))
        row.dump()
        raise
    print(header)

    sheet.set_schema(schema_1)
    for row in row_iter:
        data = detail_builder(row)
        print(data)
        counts["read"] += 1
    return counts


# The main() function.


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=Path, nargs="+")
    parser.add_argument("-s", "--schema", type=Path, required=True)
    parser.add_argument("-d", "--dry-run", default=False, action="store_true")
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbosity",
        default=logging.INFO,
        action="store_const",
        const=logging.DEBUG,
    )
    return parser.parse_args(argv)


def main(argv: list[str] = sys.argv[1:]) -> None:
    logger = logging.getLogger(__name__)
    args = parse_args(argv)
    logger.setLevel(args.verbosity)

    schema = args.schema
    with schema.open() as cobol:
        parser = schema_iter(cobol)
        json_schema_1 = next(parser)
        json_schema_2 = next(parser)
        logger.debug(pformat(json_schema_1, sort_dicts=False))
        logger.debug(pformat(json_schema_2, sort_dicts=False))
        schema_1 = SchemaMaker().from_json(json_schema_1)
        schema_2 = SchemaMaker().from_json(json_schema_2)

    for filename in args.file:
        with COBOL_Text_File(filename) as wb:
            sheet = wb.sheet("")
            # raw_dump(sheet.set_schema(schema_1))
            counts = process_sheet(sheet, schema_1, schema_2)
            logger.info(pformat(counts))


if __name__ == "__main__":
    logging.basicConfig(stream=sys.stderr)
    main()
    logging.shutdown()
