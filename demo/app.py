"""
Application Level Data Validation Demonstration.

This is the example code for the demo/validation section
of the documentation.
"""

import logging
import sys
import argparse
import pprint
from pathlib import Path
from collections import Counter
from typing import TextIO, Any, Callable
from types import TracebackType

from stingray import Row, Sheet, Workbook, HeadingRowSchemaLoader, open_workbook

logger = logging.getLogger(__name__)

# ORM Layer

from dataclasses import dataclass, asdict


@dataclass
class This:
    key: str
    value: float

    def serialize(self, file: TextIO) -> None:
        return asdict(self)

    def other_app_method(self) -> None:
        pass


class ThisForm:
    def __init__(self, **kw: Any) -> None:
        self.clean = kw

    def is_valid(self) -> bool:
        return all(
            [
                self.clean["key"] is not None,
                self.clean["value"] is not None,
            ]
        )

    def create(self) -> This:
        return This(**self.clean)


# Persistence Context Manager


class Persistent_Processing:
    stop_on_exception = True

    def __init__(self, target: TextIO) -> None:
        self.target = target
        self.file_or_database: TextIO

    def save_this(self, this: This) -> None:
        self.file_or_database.write(f"{this.serialize()!r}\n")

    def __enter__(self) -> "Persistent_Processing":
        self.file_or_database = self.target.open("a")
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType,
    ) -> None:
        if self.file_or_database:
            self.file_or_database.close()


class Validate_Only_Processing(Persistent_Processing):
    stop_on_exception = False

    def save_this(self, this: This) -> None:
        pass

    def __enter__(self) -> "Persistent_Processing":
        self.file_or_database = None
        return self


# Builder Functions


def builder_1(row: Row) -> dict[str, Any]:
    return dict(
        key=row['Column "3" - string'].value(), value=row["Col 2.0 - float"].value()
    )


def builder_2(row: Row) -> dict[str, Any]:
    return dict(key=row["Column 3"].value(), value=row["Column 2"].value())


Builder_Func = Callable[[Row], dict[str, Any]]


def builder_factory(args: argparse.Namespace) -> Builder_Func:
    return {"1": builder_1, "2": builder_2}[args.layout]


# Sheet Processing


def process_sheet(
    builder: Builder_Func, sheet: Sheet, persistence: Persistent_Processing
) -> Counter:
    counts = Counter()
    for source_row in sheet.rows():
        try:
            counts["read"] += 1
            row_dict = builder(source_row)
            row_form = ThisForm(**row_dict)
            if row_form.is_valid():
                counts["processed"] += 1
                this = row_form.create()
                persistence.save_this(this)
            else:
                counts["invalid"] += 1
        except Exception as e:
            summary = f"{e.__class__.__name__} {e.args!r}"
            logger.error(summary)
            counts["error", summary] += 1
            if persistence.stop_on_exception:
                raise
    return counts


# Workbook Processing


def process_workbook(
    builder: Builder_Func, workbook: Workbook, mode: Persistent_Processing
) -> None:
    for sheet in workbook.sheet_iter():
        logger.info(f"{workbook.name} :: {sheet.name}")
        sheet.set_schema_loader(HeadingRowSchemaLoader())
        counts = process_sheet(builder, sheet, mode)
        logger.info(pprint.pformat(dict(counts)))


# Command-Line Interface
#
# While we'd like to use "-v" for validate mode, this gets confused with setting the verbosity level.


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=Path, nargs="+")
    parser.add_argument("-o", "--output", type=Path)
    parser.add_argument("-d", "--dry-run", default=False, action="store_true")
    parser.add_argument("-l", "--layout", default="1", choices=("1", "2"))
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbosity",
        default=logging.INFO,
        action="store_const",
        const=logging.DEBUG,
    )
    return parser.parse_args(argv)


# The overall main program


def main(argv: list[str] = sys.argv[1:]) -> None:
    args = parse_args(argv)
    logger.setLevel(args.verbosity)
    builder_func = builder_factory(args)
    mode_class = Validate_Only_Processing if args.dry_run else Persistent_Processing
    logger.info("Mode: {0}".format(mode_class.__name__))
    try:
        with mode_class(args.output) as mode:
            for input in args.file:
                with open_workbook(input) as source:
                    process_workbook(builder_func, source, mode)
    except Exception as e:
        logger.exception(e)
        raise


if __name__ == "__main__":
    logging.basicConfig(stream=sys.stderr)
    main()
    logging.shutdown()
