stingray API Reference
======================

The overall structure of Stingray Reader is a
collection of modules.

..  uml::

    @startuml
    package stingray-reader {
        package schema_instance
        package estruct
        package cobol_parser
        package workbook
        package implementations

        implementations ..> workbook
        workbook ..> cobol_parser
        workbook ..> estruct
        workbook ..> schema_instance
        schema_instance ..> estruct
    }
    @enduml

We'll look at each module, separately, working
up from the bottom of the diagram.

..  toctree::

    modules/stingray.estruct
    modules/stingray.schema_instance
    modules/stingray.cobol_parser
    modules/stingray.workbook
    modules/stingray.implementations
