##########
Testing
##########

There are three "tiers" of testing for Stingray Reader.

-   Unit testing in isolation.

    -   Some tests use ``pytest``. This has 100% code coverage.

    -   Additional tests use ``doctest``. These are additional examples that are also validated.

-   Integration testing.

    -   All of the sample files are processed through a test case.

-   Acceptance testing.

    -   The demo directory contains applications which serve as acceptance test cases.

Additionally, ``mypy`` is used in strict mode to be sure the type hints are correct and consistent.
