##################
Main Test Script
##################

This module imports the other test modules and builds a complete suite
from the individual module suites.

::

    """stingray test script."""
    import unittest
    import sys
    import logging
    import test.cell
    import test.sheet
    import test.schema
    import test.schema_loader
    import test.workbook
    import test.cobol
    import test.cobol_loader
    import test.cobol_2
    import test.snappy_protobuf
            
Construction of an overall suite depends on each module providing
and easy-to-use :py:func:`suite` function that returns the module's suite.

::

    def suite():
        s= unittest.TestSuite()
        s.addTests( test.cell.suite() )
        s.addTests( test.sheet.suite() )
        s.addTests( test.schema.suite() )
        s.addTests( test.schema_loader.suite() )
        s.addTests( test.workbook.suite() )
        s.addTests( test.cobol.suite() )
        s.addTests( test.cobol_loader.suite() )
        s.addTests( test.cobol_2.suite() )
        s.addTests( test.snappy_protobuf.suite() )
        return s
        
    if __name__ == "__main__":
        with test.Logger( stream=sys.stderr, level=logging.INFO ):
            unittest.TextTestRunner().run(suite())
