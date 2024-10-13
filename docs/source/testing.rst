###########
Development
###########

The ``tox.toml`` and ``Makefile``  provide an overview of the CI processing.

Makefile targets:

-   ``quick``: Uses the ``quick`` environment in **tox**.

-   ``test``: the full test suite, including static analysis.

-   ``docs``: Creates the documentation.

-   ``docs-coverage``: a report of how many modules, classes, methods, and functions have documentation.

-   ``apidoc_gen``: re-generates an ``apidoc.rst``.
    Useful after significant revisions or extensions.

Tox environments.

-   default, used by ``tox run`` is a full test suite, and static analysis.

-   ``quick`` runs the tests in the ``tests`` directory.

-   ``demo`` runs the tests in the ``demo`` directory.

-   ``docs`` runs doctest on the examples in the documentation.

Testing
=======

There are three "tiers" of testing for Stingray Reader.

-   Unit testing.

    -   Some tests use ``pytest``. This should have 100% code coverage.

    -   Additional tests use ``doctest``. These are examples from code and documentation that are also validated.

-   Integration testing.
    All of the sample files are processed through a test case to be sure they can be read.

-   Acceptance testing.
    The ``demo`` directory contains applications which serve as acceptance test cases.

Static Analysis
================

Additionally, ``ruff`` and ``pyright`` are used to be sure the code has no obvious flaws and the type hints are correct and consistent.

Documentation
================

Sphinx is used.

Note that diagrams are created with **plantUML**.
This relies on Java.
Both the ``Makefile`` and the ``config.py`` references to the required JAR file.

Makefile::

    	curl -LSf https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar -o .plantuml/plantuml-1.2024.7.jar


docs/source/config.py::

    _plantuml_path = Path.cwd().parent.parent / ".plantuml" / "plantuml-1.2024.7.jar"

Yes. Both of these pin a specific version number.
It changes rarely.
