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
from typing import Optional, Iterator, cast, Union, Any, IO, AnyStr
from types import TracebackType

try:
    import xlrd  # type: ignore [import]

    # We _could_ define an XLRD_Unpacker to work with the ``list[Cell]``
    # definitions for each row. This _could_ allow some more flexibility in
    # extracting the ``Cell.value`` and ``Cell.type`` information.
    # It, however, doesn't seem to be required.

    class XLSUnpacker(WBUnpacker):
        def open(self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None, **kwargs: Any) -> None:
            self.the_file = xlrd.open_workbook(name, **kwargs)
        def close(self) -> None:
            if hasattr(self, "the_file") and self.the_file:
                del self.the_file
        def sheet_iter(self) -> Iterator[str]:
            yield from (sheet.name for sheet in self.the_file.sheets())
        def instance_iter(self, name: str, **kwargs: Any) -> Iterator[WBInstance]:
            xlrd_sheet = self.the_file.sheet_by_name(name)
            for row in xlrd_sheet.get_rows():
                yield cast(WBInstance, [cell.value for cell in row])

    @file_registry.file_suffix(".xls")
    class XLS_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.xlrd_args = kwargs
            self.unpacker = XLSUnpacker()
            self.unpacker.open(self.name, None, **self.xlrd_args)

        def close(self) -> None:
            # if hasattr(self, "xlrd_file") and self.xlrd_file:
            #     del self.xlrd_file
            self.unpacker.close()

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            return (Sheet(self, name) for name in self.unpacker.sheet_iter())

except ImportError:  #pragma: no cover
    pass


try:
    from openpyxl import load_workbook  # type: ignore [import]
    import openpyxl.cell.cell  # type: ignore [import]

    class XLSXUnpacker(WBUnpacker):
        def open(self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None, **kwargs: Any) -> None:
            self.the_file = load_workbook(filename=name, **kwargs)
        def close(self) -> None:
            if hasattr(self, "the_file") and self.the_file:
                self.the_file.close()
                del self.the_file
        def sheet_iter(self) -> Iterator[str]:
            return (cast(str, name) for name in self.the_file.sheetnames)
        def instance_iter(self, name: str, **kwargs: Any) -> Iterator[WBInstance]:
            pyxl_sheet = self.the_file[name]
            for row in pyxl_sheet.iter_rows():
                yield cast(WBInstance, [cell.value for cell in row])

    @file_registry.file_suffix(".xlsx")
    class XLSX_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.kwargs = kwargs
            self.unpacker = XLSXUnpacker()
            self.unpacker.open(self.name, None, **self.kwargs)

        def close(self) -> None:
            # if hasattr(self, "pyxl_file") and self.pyxl_file:
            #     self.pyxl_file.close()
            #     del self.pyxl_file
            self.unpacker.close()

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            # return (Sheet(self, name) for name in self.pyxl_file.sheetnames)
            return (Sheet(self, name) for name in self.unpacker.sheet_iter())

    #     def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
    #         sheet = cast(XLSX_Sheet, sheet)
    #         for row in sheet.pyxl_sheet.iter_rows():
    #             sheet.raw_row = row
    #             yield cast(WBInstance, [cell.value for cell in row])
    #
    # class XLSX_Sheet(Sheet[WBInstance]):
    #     def __init__(self, wb: XLSX_Workbook, name: str) -> None:
    #         super().__init__(wb, name)
    #         self.pyxl_sheet = wb.pyxl_file[name]
    #         self.raw_row: list[openpyxl.cell.cell.Cell]

except ImportError:  #pragma: no cover
    pass


try:
    import pyexcel_ods3  # type: ignore [import]

    class ODSUnpacker(WBUnpacker):
        def open(self, name: Path, file_object: Optional[Union[IO[str], IO[bytes]]] = None, **kwargs: Any) -> None:
            self.the_file = pyexcel_ods3.get_data(str(name), **kwargs)
        def close(self) -> None:
            if hasattr(self, "the_file") and self.the_file:
                del self.the_file
        def sheet_iter(self) -> Iterator[str]:
            return iter(self.the_file.keys())
        def instance_iter(self, name: str, **kwargs: Any) -> Iterator[WBInstance]:
            pyexcel_sheet = self.the_file[name]
            for row in pyexcel_sheet:
                yield cast(WBInstance, row)

    @file_registry.file_suffix(".ods")
    class ODS_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.kwargs = kwargs
            self.unpacker = ODSUnpacker()
            self.unpacker.open(self.name, None, **self.kwargs)

        def close(self) -> None:
            # if hasattr(self, "pyexcel_book") and self.pyexcel_book:
            #     del self.pyexcel_book
            self.unpacker.close()

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            # return (Sheet(self, name) for name in self.pyexcel_book.keys())
            return (Sheet(self, name) for name in self.unpacker.sheet_iter())

    #     def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
    #         sheet = cast(ODS_Sheet, sheet)
    #         for row in sheet.pyexcel_sheet:
    #             sheet.raw_row = row
    #             yield cast(WBInstance, row)
    #
    # class ODS_Sheet(Sheet[WBInstance]):
    #     def __init__(self, wb: ODS_Workbook, name: str) -> None:
    #         super().__init__(wb, name)
    #         self.pyexcel_sheet = wb.pyexcel_book[name]
    #         self.raw_row: list[Any]

except ImportError:  #pragma: no cover
    pass


try:
    import numbers_parser  # type: ignore [import]

    @file_registry.file_suffix(".ods")
    class Numbers_Workbook(Workbook[WBInstance]):
        def __init__(
                self, name: Union[str, Path], **kwargs: Any
        ) -> None:
            super().__init__(name)
            self.numbers_args = kwargs
            self.unpacker = WBUnpacker()
            self.open()

        def open(self, file_object: Optional[IO[AnyStr]] = None) -> None:
            self.numbers_book = numbers_parser.Document(self.name, **self.numbers_args)

        def close(self) -> None:
            if hasattr(self, "numbers_book") and self.numbers_book:
                del self.numbers_book

        def sheet(self, name: str) -> Sheet[WBInstance]:
            return Numbers_Sheet(self, name)

        def sheet_iter(self) -> Iterator[Sheet[WBInstance]]:
            return (
                Numbers_Sheet(self, f"{sheet}::{table}")
                    for sheet in self.numbers_book.sheets().keys()
                        for table in self.numbers_book.sheets(sheet).tables().keys()
            )

        def instance_iter(self, sheet: Sheet[WBInstance]) -> Iterator[WBInstance]:
            sheet = cast(Numbers_Sheet, sheet)
            for row in sheet.numbers_sheet.iter_rows():
                sheet.raw_row = row
                yield cast(WBInstance, row)

    class Numbers_Sheet(Sheet[WBInstance]):
        def __init__(self, wb: Numbers_Workbook, name: str) -> None:
            super().__init__(wb, name)
            sheet, _, table = name.partition("::")
            self.numbers_sheet = wb.numbers_book.sheets()[sheet].tables()[table]
            self.raw_row: list[Any]

except ImportError:  #pragma: no cover
    pass

