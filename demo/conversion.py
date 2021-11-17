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
from collections import Counter
import json
import os
from pathlib import Path
from pprint import pprint
from stingray.workbook import open_workbook, HeadingRowSchemaLoader, Sheet, Row
from stingray.schema_instance import JSON
from typing import Iterator, NamedTuple, TextIO
from jsonschema import Draft202012Validator  # type: ignore [import]

class Context(NamedTuple):
    target_path: Path

class Persistent_Processing:
    stop_on_exception = True
    def __init__(self, context: Context) -> None:
        self.context = context
        self.target_file: TextIO
    def __enter__(self) -> "Persistent_Processing":
        self.target_file = self.context.target_path.open('w')
        return self
    def __exit__(self, exc_type, exc_val, exc_tb) -> bool:
        self.target_file.close()
        return False
    def save_json(self, this_instance: JSON) -> None:
        self.target_file.write(json.dumps(this_instance) + "\n")

class Validation_Processing(Persistent_Processing):
    stop_on_exception = False
    def __enter__(self) -> "Persistent_Processing":
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
    }
}


def process_sheet(sheet: Sheet, persistence: Persistent_Processing) -> Counter:
    counts = Counter()
    for row in sheet.rows():
        counts["input"] += 1
        # A "Form" to validate the input
        doc = {
            col: float(row.name(col).value())
            for col in ("x123", "y1", "y2", "y3", "x4", "y4")
        }
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

def main(persistence: Persistent_Processing, source: Path) -> None:
    with open_workbook(source) as workbook:
        sheet = workbook.sheet('Sheet1')
        sheet.set_schema_loader(HeadingRowSchemaLoader(sheet))
        counts = process_sheet(sheet, persistence)
    pprint(counts)

if __name__ == "__main__":
    sample_dir = Path(os.environ.get("SAMPLES", "sample"))
    source = sample_dir / "Anscombe_quartet_data.csv"
    target = sample_dir / "Anscombe_quartet_data.ndjson"
    operation_class = Validation_Processing
    # operation_class = Persistent_Processing
    with operation_class(Context(target)) as persistence:
        main(persistence, source)
