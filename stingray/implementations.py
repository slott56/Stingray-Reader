"""
Workbook Implementations

See http://www.python-excel.org/ for a complete list of implementations.
Of these, we choose a few to handle XLSX, ODS, and Numbers.

.XLS files via ``xlrd``
=======================

Note that this is *only* for historical .XLS files.

See https://xlrd.readthedocs.io/en/latest/

.XLSX files via ``Openpyxl``
============================

See https://openpyxl.readthedocs.io/en/stable/

.ODS files via Pyexcel
==========================

See http://docs.pyexcel.org/en/latest/

See https://github.com/pyexcel/pyexcel-ods3

Generally, Pyexcel seems to cover the widest variety of formats.
It might be helpful to delegate more of the implementations to PyExcel.

.NUMBERS 13 files via ??
========================

See https://pypi.org/project/numbers-parser/


"""
from pathlib import Path
from stingray.workbook import file_registry, Workbook, Sheet
from stingray.schema_instance import WBInstance, WBUnpacker
from typing import Optional, Iterator, cast, Union, Any
from types import TracebackType

try:
    import xlrd  # type: ignore [import]

    # We _could_ define an XLRD_Unpacker to work with the ``list[Cell]``
    # definitions for each row. This _could_ allow some more flexibility in
    # extracting the ``Cell.value`` and ``Cell.type`` information.
    # It, however, doesn't seem to be required.

    @file_registry.file_suffix(".xls")
    class XLS_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.xlrd_args = kwargs
            self.unpacker = WBUnpacker()
            self.xlrd_file = xlrd.open_workbook(name, **self.xlrd_args)

        def __exit__(self, exc_type: Optional[type[BaseException]], exc_val: Optional[BaseException],
                     exc_tb: TracebackType) -> None:
            if hasattr(self, "xlrd_file") and self.xlrd_file:
                del self.xlrd_file

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return XLS_Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            return (XLS_Sheet(self, name) for name in self.xlrd_file.sheet_names())

        def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
            sheet = cast(XLS_Sheet, sheet)
            for row in sheet.xlrd_sheet.get_rows():
                sheet.raw_row = row
                yield cast(WBInstance, [cell.value for cell in row])

    class XLS_Sheet(Sheet[WBInstance]):
        def __init__(self, wb: XLS_Workbook, name: str) -> None:
            super().__init__(wb, name)
            self.xlrd_sheet = wb.xlrd_file.sheet_by_name(name)
            self.raw_row: list[xlrd.CELL]

except ImportError:  #pragma: no cover
    pass


try:
    from openpyxl import load_workbook  # type: ignore [import]
    import openpyxl.cell.cell  # type: ignore [import]

    @file_registry.file_suffix(".xlsx")
    class XLSX_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.pyxl_args = kwargs
            self.unpacker = WBUnpacker()
            self.pyxl_file = load_workbook(filename=name, **self.pyxl_args)

        def __exit__(self, exc_type: Optional[type[BaseException]], exc_val: Optional[BaseException],
                     exc_tb: TracebackType) -> None:
            if hasattr(self, "pyxl_file") and self.pyxl_file:
                self.pyxl_file.close()
                del self.pyxl_file

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return XLSX_Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            return (XLSX_Sheet(self, name) for name in self.pyxl_file.sheetnames)

        def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
            sheet = cast(XLSX_Sheet, sheet)
            for row in sheet.pyxl_sheet.iter_rows():
                sheet.raw_row = row
                yield cast(WBInstance, [cell.value for cell in row])

    class XLSX_Sheet(Sheet[WBInstance]):
        def __init__(self, wb: XLSX_Workbook, name: str) -> None:
            super().__init__(wb, name)
            self.pyxl_sheet = wb.pyxl_file[name]
            self.raw_row: list[openpyxl.cell.cell.Cell]

except ImportError:  #pragma: no cover
    pass


try:
    import pyexcel_ods3  # type: ignore [import]

    @file_registry.file_suffix(".ods")
    class ODS_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.pyexcel_args = kwargs
            self.unpacker = WBUnpacker()
            self.pyexcel_book = pyexcel_ods3.get_data(str(name), **self.pyexcel_args)

        def __exit__(self, exc_type: Optional[type[BaseException]], exc_val: Optional[BaseException],
                     exc_tb: TracebackType) -> None:
            if hasattr(self, "pyexcel_book") and self.pyexcel_book:
                del self.pyexcel_book

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return ODS_Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            return (ODS_Sheet(self, name) for name in self.pyexcel_book.keys())

        def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
            sheet = cast(ODS_Sheet, sheet)
            for row in sheet.pyexcel_sheet:
                sheet.raw_row = row
                yield cast(WBInstance, row)

    class ODS_Sheet(Sheet[WBInstance]):
        def __init__(self, wb: ODS_Workbook, name: str) -> None:
            super().__init__(wb, name)
            self.pyexcel_sheet = wb.pyexcel_book[name]
            self.raw_row: list[Any]

except ImportError:  #pragma: no cover
    pass


try:
    import numbers_parser  # type: ignore [import]

except ImportError:  #pragma: no cover
    pass

