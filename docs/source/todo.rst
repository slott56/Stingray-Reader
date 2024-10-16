
The TODO List
===============

Upgrade deprecated Type Annotations
------------------------------------

Replace deprecated aliases (https://docs.python.org/3/library/typing.html#deprecated-aliases).
This seems to be:

[x] ``typing.Union``
[x] ``typing.Optional``
[x] ``typing.Match``
[x] ``typing.Sequence``  -- not actually used
[ ] ``typing.Type``
[x] Replace ``typing.AnyStr``. See :pep:`695`: may need to use use ``AnyStr: (str, bytes)`` as an argument to a generic type definition.

There's also the ``Instance`` ``TypeVar`` that needs to be replaced with a simpler Union, if possible.
Otherwise ``Instance: (NDInstance, DInstance, WBInstance)`` where used.
See ``schema_instance.py``, line 701.

JSON Schema Changes
-------------------

Look at https://github.com/slott56/DataSynthTool.

The **Data Synth Tool** uses schema information to define ranges of values, and distributions.

These schema extensions should be unified with schema extensions described here.

Further, the **Data Synth Tool** emphasizes Pydantic's style of embedded data validation into the class definition.
While this adds a dependency, it seems like a useful direction.

From the Code
=============

These are items marked with ``..  todo::`` in the code.

..  todolist::
