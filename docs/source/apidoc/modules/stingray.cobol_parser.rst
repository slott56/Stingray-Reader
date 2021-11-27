stingray.cobol\_parser
======================

.. automodule:: stingray.cobol_parser


COBOL Language
--------------

A COBOL "Copybook" is a group-level DDE.
See https://www.ibm.com/docs/en/cobol-zos/4.2?topic=division-data-data-description-entry

There are three formats for DDE's. We only really care about one of them.

- Format 1 is the useful DDE level numbers 01 to 49 and 77.

- Format 2 is a RENAMES clause, level 66. We don't support this.

. Format 3 is a CONDITION, level 88. This is a kind of enumeration of values; we tolerate it, but don't do anything with it.

Here's the railroad diagramm for each sentence. Clauses after level-number and data-name-1 can appear in any order.

::

    >>-level-number--+-------------+--+------------------+---------->
                     +-data-name-1-+  '-redefines-clause-'
                     '-FILLER------'

    >--+------------------------+--+-----------------+-------------->
       '-blank-when-zero-clause-'  '-external-clause-'

    >--+---------------+--+--------------------+-------------------->
       '-global-clause-'  '-group-usage-clause-'

    >--+------------------+--+---------------+---------------------->
       '-justified-clause-'  '-occurs-clause-'

    >--+----------------+--+-------------+-------------------------->
       '-picture-clause-'  '-sign-clause-'

    >--+---------------------+--+--------------+-------------------->
       '-synchronized-clause-'  '-usage-clause-'

    >--+--------------+--+--------------------+--------------------><
       '-value-clause-'  '-date-format-clause-'

A separator period occurs at the end of the sentence. (It's described elsewhere in the COBOL language reference.)

For simple examples, see https://github.com/rradclif/mortgagesample/tree/master/MortgageApplication/copybook

For comprehensive, complex examples, see
https://github.com/royopa/cb2xml/tree/ec83af657b781afd0dad9cc263623faa2549f738/source/cb2xml_tests/src/common/cobolCopybook

These cover a large number of COBOL-to-XML cases.

reference_format function
-------------------------
..  autofunction::       reference_format

Parsing
-------
..  autofunction::       dde_sentences

..  autofunction::       expand_repeat

..  autofunction::       pass_non_empty

..  autofunction::       normalize_picture

..  autofunction::       clause_dict

High Level Parsing
-------------------

..  autofunction::       structure

..  autofunction::       schema_iter

   

   
   
DDE Class
---------
   
..  autoclass:: DDE
    :members:
    :undoc-members:

JSONSchemaMaker Class
----------------------

..  autoclass:: JSONSchemaMaker
    :members:
    :undoc-members:

..  autoclass:: JSONSchemaMakerExtendedVocabulary
    :members:
    :undoc-members:

   
Exceptions
-----------
   
..  autoexception:: DesignError
   
   



