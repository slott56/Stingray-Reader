stingray.implementations
========================

.. automodule:: stingray.implementations

XLS
---

.XLS files are unpacked via ``xlrd``.

Note that this is *only* for historical .XLS files.

See https://xlrd.readthedocs.io/en/latest/

..  autoclass::      XLSUnpacker
    :members:
    :undoc-members:

..  autoclass::      XLS_Workbook
    :members:
    :undoc-members:

XLSX
----

.XLSX files are unpacked with ``Openpyxl``.

See https://openpyxl.readthedocs.io/en/stable/

..  autoclass::      XLSXUnpacker
    :members:
    :undoc-members:

..  autoclass::      XLSX_Workbook
    :members:
    :undoc-members:

ODS
----


.ODS files are unpacked with ``Pyexcel``.

See http://docs.pyexcel.org/en/latest/

See https://github.com/pyexcel/pyexcel-ods3

Pyexcel seems to cover a wide variety of formats.
It might be helpful to delegate more of the implementations to PyExcel.

..  autoclass::      ODSUnpacker
    :members:
    :undoc-members:

..  autoclass::      ODS_Workbook
    :members:
    :undoc-members:

Numbers
-------


.NUMBERS 13 files are unpacked by ``numbers-parser``.

See https://pypi.org/project/numbers-parser/


..  autoclass::      NumbersUnpacker
    :members:
    :undoc-members:

..  autoclass::      Numbers_Workbook
    :members:
    :undoc-members:

   

   
   
   



