stingray.schema\_instance
=========================

.. automodule:: stingray.schema_instance


A schema is used to unpack (or decode) the data in a file.
Even a simple CSV file offers headings in the first row as a simplistic schema.

JSON Schema
---------------------

A JSON Schema permits definitions used to navigate spreadsheet files, like CSV.
It is also used to unpack more sophisticated generic structures in JSON, YAML, and TOML format, as well as XML.

A JSON Schema -- with some extensions -- can be used to unpack COBOL files, in Unicode or ASCII text as well as EBCDIC.
Most of the features of a COBOL DDE definition parallel JSON Schema constructs.

- COBOL has Atomic fields of type text (with vaious format details), and a variety of "Computational" variants. The
  most important is ``COMP-3``, which is a decimal representation with digits packed two per byte. The JSON Schema
  presumes types "null", "boolean", "number", or "string" types have text representations that fit well with COBOL.

- The hierarchy of COBOL DDE's is the JSON Schema "object" type.

- The COBOL ``OCCURS`` clause is the JSON Schema "array" type. The simple case, with a single, literal ``TIMES`` option
  is expressed with ``maxItems`` and ``minItems``.

While COBOL is more sophisticated than CSV, it's generally comprarable to JSON/YAML/TOML/XML.
There are some unique specializations related to COBOL processing.

COBOL Processing
----------------

The parallels between COBOL and JSON Schema permit translating COBOL Data Definition Entries (DDE's) to
JSON Schema constructs.
The JSON Schema (with extensions) is used to decode bytes from COBOL representation to
create native Python objects.

There are three areas of unique complex that require extensions.
The COBOL ``REDEFINES`` and ``OCCURS DEPENDING ON`` structures.

Additionally, EBCDIC unpacking is handled by :py:mod:`stingray.estruct`.

Redefines
~~~~~~~~~

A COBOL ``REDEFINES`` clause defines a free union of types for a given sequence of bytes. Within the application code,
there are generally fields that imply a more useful tagged union. The tags used for discrimination is not part of the
COBOL definition.

To make this work, each field within the JSON schema has an implied starting offset and length.
A COBOL ``REDEFINES`` clause can be described with a JSON Schema extension that includes a JSON Pointer to name a field with which a given field is co-located.

The COBOL language requires a strict backwards reference to a previously-defined field, and the names must have the
name indentation level, making them peers within the same parent, reducing the need for complex pointers.

Occurs Depending On
~~~~~~~~~~~~~~~~~~~

The complexity of ``OCCURS DEPENDING ON`` constructs arises because the size (``maxItems``) of the array is the value of
another field in the COBOL record definition.

Ideally, a JSON Reference names the referenced field as the ``maxItems`` attribute for an array. This, however,
is not supported, so an extension vocabulary is required.

Notes
~~~~~

See https://json-schema.org/draft/2020-12/relative-json-pointer.html#RFC6901 for information on JSON Pointers.

Terminology
~~~~~~~~~~~

The JSON Schema specification talks about the data described by the schema as a "instance" of the schema. The schema
is essentially a class of object, the data is an instance of that class.

It's awkward to distinguish the more general use of "instance" from the specific use of "Instance of a JSON Schema".
We'll try to use :py:class:`Instance`, and :py:class:`NDInstance` to talk about the object described by a JSON Schema.


Physical File Formats
---------------------

There are several unique considerations
for the various kinds of file formats.
These are implemented via the :py:class:`Unpacker`
class hierarchy.

Delimited Files
~~~~~~~~~~~~~~~

Delimited files have text representations with syntax defined by a module like :py:mod:`json`. Because of the presence of delimiters, individual character and byte counting isn't relevant.

Pythonic navigation through instances of delimited structures leverages the physical format's parser output. Since most formats provide a mixture of dictionaries and lists, `object["field"]` and `object[index]` work nicely.

The JSON Schema structure will parallel the instance structure.

Workbook Files
~~~~~~~~~~~~~~

Files may also be complex binary objects described by workbook file for XLSX, ODS, Numbers, or CSV files. To an extent, these are somewhat like delimited files, since individual character and byte counting isn't relevant.

Pythonic navigation through instances of workbook row structures leverages the workbook format's parser output. Most workbooks are lists of cells; a schema with a flat list of properties will work nicely.

The :py:mod:`csv` fornmat is built-in. It's more like a workbook than it is like JSON or TPOML. For example, with simple CSV files, the JSON Schema must be a flat list of properties corresponding to the columns.

Non-Delimited Files (COBOL)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's essential to provide Pythonic navigation through a COBOL structure. Because of ``REDEFINES`` clauses, the COBOL structure may not map directly to simple Pythonic dict and list types. Instead, the evaluation of each field must be strictly lazy.

This suggests several target constructs.

- ``object.name("field").value()`` should catapult down through the instance to the named field. Syntactic sugar might include ``object["field"]`` or ``object.field``. Note that COBOL raises a compile-time error for a reference to an amiguous name; names may be duplicated, but the duplicates must be disambiguated with ``OF`` clauses.

- ``object.name("field").index(x).value()`` works when the field is a member of an array somewhere above it in the structure. Syntactic sugar might include ``object["field"][x]`` or ``object.field[x]``.

These constructs are abbreviations for explicit field-by-field navigation. The field-by-field navigation involves explicitly naming all parent fields. Here are some constructs.

- ``object.name("parent").name("child").name("field").value()`` is the full navigation path to a nested field. This can be ``object["parent"]["child"]["field"]``. A more sophisticated parser for URL path syntax might also be supported. ``object.name["parent/child/field"]``.

- ``object.name("parent").name("child").index(x).name("field").value()`` is the full navigation path to a nesteed field with a parent occurs-depending-on clause. This can be ``object["parent"]["child"][x]["field"]``. A more sophisticated parser for URL path syntax might also be supported. ``object.name["parent/child/0/field"]``.

The COBOL ``OF`` construct provides parentage in reverse order. This means ``object.name("field").of("child").of("parent").value()`` is requred to parallel COBOL syntax. While unpleasant, it's helpful to support this.

The ``value()`` method can be helpful to be explicit about locating a value. This avoids eager evaluation of ``REDEFINES`` alternatives that happen to be invalid.

An alternative to the ``value()`` method is to use built-in special names ``__int__()``, ``__float__()``, ``__str__()``, ``__bool__()`` to do conversion to a primitive type; i.e., ``int(object.name["parent/child/field"])``. Additional functions like ``asdict()``, ``aslist()``, and ``asdecimal()`` can be provided to handle conversion edge cases.

We show these as multi-step operations with a fluent interface. This works out well when a nagivation context object is associated with each sequence of ``object.name()...``, ``object.index()``, and ``object.of()`` operations. The first call in the sequence emits a navigation object; all subsequent steps in the fluent interface return navigation objects. The final ``value()`` or other special method refers back to the original instance container for type conversion of an atomic field.

Each COBOL navigation step involves two parallel operations:

- Finding the definition of the named subschema within a JSON schema.

- Locating the sub-instance for the item. This is a slice of the instance.

The instance is a buffer of bytes (or characters for non-COBOL file processing.) The overall COBOL record has a starting offset of zero. Each DDE has an starting offset and length. For object property navigation this is the offset to the named property that describes the DDE. For array navigation, this is the index to a subinstance within an array of subinstances.

It's common practice in COBOL to use a non-atomic field as if it was atomic. A date field, for example, may have year, month, and day subfields that are rarely used independently. This means that JSON Schema array and object definitions are implicitly `type: "string"` to parallel the way COBOL treats non-atomic fields as `USAGE IS DISPLAY`.

   

   
   
decimal_places function
-----------------------

..  autofunction:: decimal_places

digit_string function
---------------------

..  autofunction:: digit_string
   
   
Schema
------

Here is the extended JSON Schema definition.
This is a translation of the various JSON Schema constructs into Python class definitions.
These objects must be considered immutable. (Pragmatically, RefTo objects can be updated
to resolve forward references.)

A :py:class:`Schema` is used to describe an :py:class:`Instance`.
For non-delimited instances, the schema requires additional :py:class:`Location` information;
this must be computed lazily to permits ``OCCURS DEPENDING ON`` to work.
For delimited instances, no additional data is required, since the parser located all object
boundaries and did conversions to Python types. Similarly, for workbook instances, the
underlying workbook parser can create a row of Python objects.

The :py:class:`DependsOnArraySchema` is an extension to handle references to another
field's value to provide ``minItems`` and ``maxItems`` for an array.
This handles the COBOL ``OCCURS DEPENDING ON``. This requires a reference to another
field which is a reference to another field instead of a simple value.

The COBOL ``REDEFINES`` clause is handled created a OneOf suite of alternatives
and using some JSON Schema "$ref" references to preserve the original, relatively flat
naming for an elements and the element(s) which redefine it.

..  uml::

    @startuml
        abstract class Schema {
            type: string
        }

        class AtomicSchema

        class ArraySchema

        class DependsOnArraySchema

        class ObjectSchema

        class OneOfSchema

        class RefToSchema

        Schema <|-- AtomicSchema
        Schema <|-- ArraySchema
        Schema <|-- ObjectSchema
        AtomicSchema <|-- OneOfSchema
        AtomicSchema <|-- RefToSchema
        ArraySchema <|-- DependsOnArraySchema
        RefToSchema --> Schema
        'OneOfSchema --> "*" Schema : Redefines
        'ObjectSchema --> "*" Schema : Properties
        'ArraySchema --> Schema : Items
        'DependsOnArraySchema --> Schema : maxItems
    @enduml

.. autoclass::       Schema
   :members:

.. autoclass::       ArraySchema
   :members:

.. autoclass::       AtomicSchema
   :members:

.. autoclass::       DependsOnArraySchema
   :members:

.. autoclass::       ObjectSchema
   :members:

.. autoclass::       OneOfSchema
   :members:

.. autoclass::       RefToSchema
   :members:

.. autoclass::       SchemaMaker
   :members:

.. autoclass::       Reference
   :members:

Instance
---------


For bytes and strings, we provide wrapper :py:class:`Instance` definitions.
These :py:class:`BytesInstance` and :py:class:`TextInstance`
are used by :py:class:`NDNav` and :py:class:`Location` objects.

For :py:class:`DInstance` and :py:class:`WBInstance`, however, we don't
really need any additional features. We can use native ``JSON`` or ``list[Any]``
objects.


.. autoclass::       WBInstance
   :members:

.. autoclass::       DInstance
   :members:

.. autoclass::       NDInstance
   :members:

.. autoclass::       BytesInstance
   :members:

.. autoclass::       TextInstance
   :members:

Unpacker
---------


An :py:class:`Unpacker` is a strategy class that handles details of physical unpacking of bytes or text. We call it an :py:class:`Unpacker`, because it's similar to `struct.unpack`.

The JSON Schema's intent is to depend on delimited files, using a separate parser. For this application, however, the schema is used to provide information to the parser.

To work with the variety of instance data, we have several subclasses of :py:class:`Instance` and related :py:class:`Unpacker` classes:

- **Non-Delimited**. These cases use :py:class:`Location` objects. We define an :py:class:`NDInstance` as a common protocol
  wrapped around ``AnyStr`` types. There are three sub-cases depending on the underlying object.

    - **COBOL Bytes**. An :py:class:`NDInstance` type union includes ``bytes``. The :py:mod:`estruct` module is a COBOL replacement for the :py:mod:`struct` module. The JSON Schema requires extensions to handle COBOL complexities.

    - **STRUCT Bytes**. An :py:class:`NDInstance` type union includes ``bytes``. The :py:mod:`struct` module :py:func:`unpack` and :py:func:`calcsize` functions are used directly. This means the field codes must match the :py:mod:`struct` module's definitions. This can leverage some of the same extensions as COBOL requires.

    - **Text**. An :py:class:`NDInstance` type union includes ``str``. This is the case with non-delimited text that has plain text encodings for data. The DISPLAY data will be ASCII or UTF-8, and any COMP/BINARY numbers are represented as text.

- **Delimited**. These cases do not use :py:class:`Location` objects. There are two sub-cases:

    - **JSON Objects**. This is a Union of ``dict[str, Any] | Any | list[Any]``.
      The instance is created by some external unpacker, and is already in a Python native structure.
      Unpackers include ``json``, ``toml``, and ``yaml``. A wrapper around an ``xml`` parser
      can be used, also.
      We'll use a ``JSON`` type hint for objects this unpacker works with.

    - **Workbook Rows**. These include CSV, ODS, XLSX, and Numbers documents.
      The instance is a structure created by the workbook module as an unpacker.
      The :py:mod:`csv` unpacker is built-in.
      These all use ``list[Any]`` for objects this unpacker works with.

Unpacking is a plug-in strategy.
For non-delimited data, it combines some essential location information with a :py:meth:`value` method that's unique to the instance source data.
For delimited data, it provides a uniforma interface for the various kinds of spreadsheets.

The JSON Schema extensions to drive unpacking include the `"cobol"` keyword. The value for this has the original COBOL DDE. This definition can have  USAGE and  PICTURE clauses that define how bytes will encode the value.

..  uml::

    @startuml
        abstract class Unpacker {
            calcsize(schema): int
            value(schema, instance): Any
            nav(schema, instance): Nav
        }

        class Location

        abstract class Nav

        abstract class Schema

        Unpacker "n" -- Schema
        Unpacker --> Location : creates
        Unpacker --> Nav : creates

        abstract class NonDelimited
        Unpacker <|-- NonDelimited

        class EBCDIC
        NonDelimited <|-- EBCDIC

        class Struct
        NonDelimited <|-- Struct

        class Text
        NonDelimited <|-- Text

        class Delimited
        Unpacker <|-- Delimited

        class Sequence
        Delimited <|-- Sequence

        class Mapping
        Delimited <|-- Mapping

    @enduml

Implementation Notes
~~~~~~~~~~~~~~~~~~~~

We need three separate kinds of :py:class:`Unpacker` subclasses to manage the kinds of :py:class:`Instance` subclasses:

- The :py:class:`NonDelimited` subclass of :py:class:`Unpacker` handles an :py:class:`NDInstance`
  which is either a string or bytes with non-delimited data.
  The :py:class:`Location` reflects an offset into the :py:class:`NDInstance`.

- The ``Delimited`` subclass of :py:class:`Unpacker` handles delimited data, generally using ``JSON``
  as a type hint. This will have a `dict[str, Any] | list[Any] | Any` structure.

- A ``Workbook`` subclass of :py:class:`Unpacker` wraps a workbook parser creating a :py:class:`WBInstance`.
  Generally workbook rows are `list[Any]` structures.

An :py:class:`Unpacker` instance is a factory for :py:class:`Nav` objects. When we need to navigate around
an instance, we'll leverage ``unpacker.nav(schema, instance)``. Since the
schema binding doesn't change very often, ``nav = partial(unpacker.nav, (schema,))`` is
a helpful simplification. With this partial, ``nav(instance).name(n)`` or ``nav(instance).index(n)`` are all that's needed
to locate a named field or apply array indices.


Unpacker Size Computations
~~~~~~~~~~~~~~~~~~~~~~~~~~

The sizes are *highly* dependent on format information that comes from COBOL DDE (or other schema details.) A ``cobol`` extension to JSON Schema provides the COBOL-syntax ``USAGE`` and ``PICTURE`` clauses required to parse bytes. There are four overall cases, only two of which require careful size computations.

**Non-Delimited COBOL**.  See https://www.ibm.com/docs/en/cobol-zos/4.2?topic=clause-computational-items and https://www.ibm.com/docs/en/cobol-zos/4.2?topic=entry-usage-clause and https://www.ibm.com/docs/en/cobol-zos/4.2?topic=entry-picture-clause.

-  ``USAGE DISPLAY``. ``PIC X...`` or ``PIC A...``. Data is text. Size given by the picture. Value is ``str``.

-  ``USAGE DISPLAY``. ``PIC 9...``. Data is "Zoned Decimal" text. Size given by the picture. Value is ``decimal``.

-  ``USAGE COMP`` or ``USAGE BINARY`` or ``USAGE COMP-4``. ``PIC 9...``. Data is bytes. Size based on the picture: 1-4 digits is two bytes. 5-9 digits is 4 bytes. 10-18 is 8 bytes. Value is a ``int``.


-  ``USAGE COMP-1``. ``PIC 9...``. Data is 32-bit float. Size is 4. Value is ``float``.

-  ``USAGE COMP-2``. ``PIC 9...``. Data is 64-bit float. Size is 8. Value is ``float``.

-  ``USAGE COMP-3`` or ``USAGE PACKED-DECIMAL``. ``PIC 9...``. Data is two-digits-per-byte packed decimal. Value is a ``decimal``.


**Non-Delimited Native**. Follows the Python :py:mod:`struct` module definitions. The :py:func:`struct.calcsize` function computes the structure's size. The :py:func:`struct.unpack` function unpacks the values using the format specification. Or ``maxLength`` can be used to define sizes.

**Delimited**. The underlying parser (JSON, YAML, TOML, XML) decomposed the data and performed conversions. The schema conversions *should* match the data that's present

**Workbook**. Generally, an underlying workbook unpacker is required. For CSV, the data is all strings, conversions are defined only in the schema.

Conversions
~~~~~~~~~~~

The ``CONVERSION`` mapping has values for the "conversion" keyword.
Some of these are extensions that could also be part of a vocabulary for COBOL and Workbooks.

Date, Time, Timestamp, Duration, and Error may need to be part of these conversions.
The problem with non-ISO standard dates means that a package like ``dateutil`` is required
to guess at the format.

For US ZIP codes, a ``digit_string(size, value)`` function turns an integer to a string padded with zeroes.
The partial function ``digits_5 = partial(digit_string, 5)`` is used to transforms spreadsheet zip
codes from integers back into useful strings.

For currency in many countries, a ``decimal_places()`` function will transform a float value back to Decimal
with an appropriate number of decimal places. The partial function ``decimal_2 = partial(decimal_places, 2)``
will transform float dollars into a decimal value rounded to the nearest penny



.. autoclass::       Mode
   :members:

.. autoclass::       Unpacker
   :members:

.. autoclass::       Delimited
   :members:

.. autoclass::       EBCDIC
   :members:

.. autoclass::       Struct
   :members:

.. autoclass::       TextUnpacker
   :members:

.. autoclass::       WBUnpacker
   :members:

Schema and Instance Navigation
------------------------------


This is the core abstraction for a :py:class:`Row` of a :py:class:`Sheet`. Or a document in an JSON-Newline file. Or a row in a CSV file or other workbook. It's one document in an interable YAML file. (While there's no trivial mapping to TOML files, a subclass can locate sections or objects within a section that are treated as rows.)

A :py:class:`Row` is a collection of named values. A :py:class:`Schema` provides name and type information for unpacking the values. In the case of non-delimited file formats, navigation becomes a complex problem and :py:class:`Location` objects are created. With COBOL ``REDEFINES`` and `OCCURS DEPENDING ON` clauses, fields are found in positions unique to each :py:class:`NDInstance`.

The names for attributes should be provided as ``"$anchor"`` values to make them visible. In the case of simple workbook files, the rows are flat and property names are a useful surrogate for anchors.

A :py:class:`Row` has a plug-in strategy for navigation among cells in the workbook or fields in a JSON object, or the more complex structures present in a Non-Delimited File.

The abstract :py:class:`Nav` class provides unifieid navigation for delimited as well as non-delimited rows. The :py:class:`NDNav` subclass handles non-delimited files where :py:class:`Location` objects are required. The :py:class:`DNav` handles JSON and other delimited structures. The :py:class:`WBNav` subclass wraps workbook modules.

An :py:class:`NDNav` instance provides a context that can help to move through an :py:class:`NDInstance` of non-delimited data using a :py:class:`Schema`.  These are created by a :py:class:`LocationMaker` instance because this navigation so intimately involved in creating :py:class:`Location` objects to find structures.

A separate :py:class:`DNav` subclass is a context that navigates through delimited objects where the object structure matches the schema structure. In the case of JSON/YAML/TOML, the operations are trivially delegated to the underlying native Python object; it's already been unpacked.

A third :py:class:`WBNav` subclass handles CSV, XML and Workbook files. These rely on an underlying unpacker to handle the details of navigation, which are specific to a parser. The :py:class:`WBNav` is a **Facade** over these various kinds of parsers.

All of these are plug-in strategies used by a :py:class:`Row` that provides a uniform wrapper.


..  uml::

    @startuml
        abstract class Unpacker

        abstract class Row {
            name(string): Nav
            index(int): Nav
        }

        Row ..> Unpacker
        abstract class NDInstance {
            name(string): NDNav
            index(int): NDNav
        }

        Unpacker --> Nav : creates

        abstract class Nav

        class NDNav {
            name(string): NDNav
            index(int): NDNav
            value(schema): Any
        }

        Nav <|-- NDNav
        Row --> NDNav
        NDNav --> NDInstance

        class Location

        NDNav --> Location
        class DNav {
            name(string): DNav
            value(schema): Any
        }

        Nav <|-- DNav
        Row --> DNav
        DNav --> JSON

        class WBNav {
            name(string): DNav
            value(schema): Any
        }

        Row --> WBNav
        Nav <|-- WBNav
    @enduml

We could try to create a subclass of ``dict`` that added methods to support :py:class:`Nav` and :py:class:`DNav` behaviors.
This seems a bit complicated, since we're generally dealing with a :py:class:`Row`.
This class creates an appropriate :py:class:`NDInstance` or :py:class:`WBInstance` based in the
Workbook's :py:class:`Unpacker` subclass.

A separate plug-in **Strategy** acts as an **Adapter** over the distinct implementation details.


.. autoclass::       Nav
   :members:

.. autoclass::       DNav
   :members:

.. autoclass::       NDNav
   :members:

.. autoclass::       WBNav
   :members:

.. autoclass::       CSVNav
   :members:

Locations
----------


A :py:class:`Location` is required to unpack bytes from non-delimited instances. This is a feature of the :py:class:`NonDelimited` subclass of :py:class:`Unpacker` and the associated :py:class:`NDNav` class.

It's common to consider the :py:class:`Location` details as "decoration" applied to a :py:class:`Schema`. An implementation that decorates the schema requires a stateful schema and cant process more than one :py:class:`Instance` at a time.

We prefer to have :py:class:`Location` objects as "wrappers" on :py:class:`Schema` objects; the :py:class:`Schema` remains stateless and we process multiple :py:class:`NDInstance` objects with distinct :py:class:`Location` objects.

Each :py:class:`Location` object contains a :py:class:`Schema` object and additional start and end offsets. This may be based on the values of dependencies like `OCCURS DEPENDING ON` and ``REDEFINES``.

The abstract :py:class:`Location` class is built by a :py:class:`LocationMaker` object to provide specific offsets and sizes for non-delimited files with `OCCURS DEPENDING ON`. The :py:class:`LocationMaker` seems to be part of the :py:class:`Unpacker` class definition.

..  uml::

    @startuml
        abstract class Unpacker {
            value(schema, NDInstance): Any
            size(schema): int
            location(schema, NDInstance): Location
        }
        abstract class Schema {
            type: string
        }
        abstract class NDInstance

        class Location {
            schema: Schema
            unpacker: Unpacker
            start: int
            end: int
            value(NDInstance): Any
        }

        Location ...> NDInstance
        Unpacker -> Location
        Location -> Schema
        Unpacker .> Schema : "Uses"

        class AtomicLocation
        Location <|-- AtomicLocation
        class ArrayLocation
        Location <|-- ArrayLocation
        class ObjectLocation
        Location <|-- ObjectLocation
        class OneOfLocation
        Location <|-- OneOfLocation
        class RefToLocation
        Location <|-- RefToLocation
    @enduml


.. autoclass::       Location
   :members:

.. autoclass::       ArrayLocation
   :members:

.. autoclass::       AtomicLocation
   :members:

.. autoclass::       ObjectLocation
   :members:

.. autoclass::       OneOfLocation
   :members:

.. autoclass::       RefToLocation
   :members:

.. autoclass::       LocationMaker
   :members:

   
Exceptions
----------

.. autoclass:: DesignError
   :members:
