"""
Quick *and dirty* conversion of data from one format to another.

In this case, from CSV to NDJSON.

The idea is that we have a schema for the source.
In the case of many spreadsheet files, the schema is in the heading row.

Really, this needs to be decomposed like the app.py into two subclasses:

-   Read and Validate.

-   Read, Validate, and Persist.

The idea is to make persistence an optional feature so we can validate without making
any code changes.
"""
import argparse
from collections import Counter
import json
import logging
import os
from pathlib import Path
from pprint import pprint
import sys
from stingray.workbook import open_workbook, HeadingRowSchemaLoader, Sheet, Row
from stingray.schema_instance import JSON
from typing import Iterator, NamedTuple, TextIO, Any
from jsonschema import Draft202012Validator  # type: ignore [import]


def builder(row: Row) -> dict[str, Any]:
    doc = {
        col: float(row.name(col).value())
        for col in ("x123", "y1", "y2", "y3", "x4", "y4")
    }
    return doc


class Persistent_Processing:
    stop_on_exception = True

    def __init__(self, target_path: Path) -> None:
        self.target_path = target_path
        self.target_file: TextIO

    def __enter__(self) -> "Persistent_Processing":
        self.target_file = self.target_path.open("w")
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> bool:
        self.target_file.close()
        return False

    def save_json(self, this_instance: JSON) -> None:
        self.target_file.write(json.dumps(this_instance) + "\n")


class Validation_Processing(Persistent_Processing):
    stop_on_exception = False

    def __enter__(self) -> "Validation_Processing":
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> bool:
        return False

    def save_json(self, this_instance: JSON) -> None:
        print(json.dumps(this_instance))


TARGET_SCHEMA = {
    "title": "Anscombe's Quartet",
    "type": "object",
    "properties": {
        "x123": {"type": "number"},
        "y1": {"type": "number"},
        "y2": {"type": "number"},
        "y3": {"type": "number"},
        "x4": {"type": "number"},
        "y4": {"type": "number"},
    },
}


def process_sheet(sheet: Sheet, persistence: Persistent_Processing) -> Counter:
    counts = Counter()
    for row in sheet.rows():
        counts["input"] += 1
        # Convert to an intermediate form
        doc = builder(row)
        # Vaidate against the target JSONSchema
        if Draft202012Validator(TARGET_SCHEMA).is_valid(doc):
            # Persist the valid data.
            persistence.save_json(doc)
            counts["output"] += 1
        else:
            # Report on the invalid data
            counts["invalid"] += 1
            print(f"error, {row} produced invalid {doc}")
            for error in Draft202012Validator(TARGET_SCHEMA).iter_errors(doc):
                print(" ", error)
    return counts


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=Path, nargs="+")
    parser.add_argument("-o", "--output", type=Path)
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
    args = parse_args(argv)
    mode_class = Validation_Processing if args.dry_run else Persistent_Processing
    with mode_class(args.output) as persistence:
        for source in args.file:
            with open_workbook(source) as workbook:
                sheet = workbook.sheet("Sheet1")
                sheet.set_schema_loader(HeadingRowSchemaLoader())
                counts = process_sheet(sheet, persistence)
            pprint(counts)


if __name__ == "__main__":
    sample_dir = Path(os.environ.get("SAMPLES", "sample"))
    source = sample_dir / "Anscombe_quartet_data.csv"
    target = sample_dir / "Anscombe_quartet_data.ndjson"
    main(["-d", "-o", str(target), str(source)])
