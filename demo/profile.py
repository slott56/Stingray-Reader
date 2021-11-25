# ##########################################################
# Data Profiling Demonstration
# ##########################################################
#
# This produces simple RST-format output on stdout.
#
# A common use case is the following:
#
# ..  code-block:: bash
#
#     python3 demo/profile.py sample/\*.csv >profile_csv.rst
#     rst2html.py profile_csv.rst profile_csv.html
#
# This gives us an HTML-formatted report.

import argparse
from collections import defaultdict, Counter
import contextlib
from io import StringIO
import logging
from pathlib import Path
import pprint
import sys
from typing import TextIO, Any, Optional, Callable
from types import TracebackType

from stingray.workbook import (
    open_workbook,
    HeadingRowSchemaLoader,
    Workbook,
    Sheet,
    Row,
)


logger = logging.getLogger(__name__)

# Core Model


class Stats:
    def __init__(self, sheet: Sheet) -> None:
        self.sheet = sheet
        self.stats = defaultdict(lambda: Counter())

    def sample(self, column: str, value: Any) -> None:
        self.stats[column][value] += 1

    def report(self) -> None:
        title = f"{self.sheet.workbook().name} :: {self.sheet.name}"
        print(title)
        print("=" * len(title))
        print()
        for name, attr in self.sheet.schema.properties.items():
            print(name)
            print("-" * len(name))
            print()
            print(attr)
            print()
            print("..  csv-table::")
            print()
            for k in self.stats[name]:
                print('    "{0}","{1}"'.format(k, self.stats[name][k]))
            print()

    def serialize(self) -> str:
        buffer = StringIO()
        with contextlib.redirect_stdout(buffer):
            self.report()
        return buffer.getvalue()


# Processing Context


class Profile_Processing:
    """A subclass might avoid printing the results??"""

    def __init__(self, fail_fast: bool = False) -> None:
        self.stop_on_exception = fail_fast

    def save_stats(self, stats: Stats) -> None:
        print(stats.serialize())

    def __enter__(self) -> "Profile_Processing":
        return self

    def __exit__(
        self,
        exc_type: Optional[type[BaseException]],
        exc_val: Optional[BaseException],
        exc_tb: TracebackType,
    ) -> None:
        pass


# Processing


def process_sheet(sheet: Sheet, mode: Profile_Processing) -> Counter:
    audit_counts = Counter()
    statistics = Stats(sheet)
    for source_row in sheet.rows():
        try:
            audit_counts["read"] += 1
            for name in sheet.schema.properties:
                statistics.sample(name, source_row[name].value())
        except Exception as e:
            audit_counts["invalid"] += 1
            if mode.stop_on_exception:
                raise
            logger.error(f"{e}")  #: {source_row.dump()}")
            audit_counts[("error ", type(e))] += 1

    mode.save_stats(statistics)
    return audit_counts


# Note that the following :py:func:`process_workbook` function makes some specific claims about the given
# file.  In particular:
#
# -   :py:class:`sheet.EmbeddedSchemaSheet`.  The schema is embedded within each sheet.
#
# -   :py:class:`schema.loader.HeadingRowSchemaLoader`.  The schema is the heading row.
#
# If these assumptions are not universally true, then different application
# programs or different :py:func:`process_workbook` functions must be written to handle other kinds of workbooks.
#
# ::


def process_workbook(input: Workbook, mode: Profile_Processing) -> None:
    for sheet in input.sheet_iter():
        logger.info("{0} :: {1}".format(input.name, sheet.name))
        sheet.set_schema_loader(HeadingRowSchemaLoader())
        counts = process_sheet(sheet, mode)
        logger.info(pprint.pformat(dict(counts)))


# Command-Line Interface


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("file", type=Path, nargs="+")
    parser.add_argument("-f", "--fail-fast", default=False, action="store_true")
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
    logging.getLogger().setLevel(args.verbosity)
    with Profile_Processing(args.fail_fast) as mode:
        try:
            for input in args.file:
                with open_workbook(input) as source:
                    process_workbook(source, mode)
        except Exception as e:
            logging.error(e)
            raise


if __name__ == "__main__":
    logging.basicConfig(stream=sys.stderr)
    main()
    logging.shutdown()
