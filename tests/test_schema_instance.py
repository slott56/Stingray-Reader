"""
Test the Schema and Instance models.
"""
import pytest
from unittest.mock import Mock, MagicMock, call, sentinel
import re
try:  # pragma: no cover
    from jsonschema import Draft202012Validator as SchemaValidator
except ImportError:
    from jsonschema import Draft7Validator as SchemaValidator  # type: ignore[import]

@pytest.fixture
def atomic_schema():
    json_schema = {"type": "number"}
    SchemaValidator.check_schema(json_schema)
    return AtomicSchema(json_schema)

@pytest.fixture
def object_schema():
    json_schema = {"type": "object", "properties": {"field": {"type": "number"}}}
    SchemaValidator.check_schema(json_schema)
    return ObjectSchema(json_schema, {"field": AtomicSchema(json_schema["properties"]["field"])})

@pytest.fixture
def array_schema():
    json_schema = {"type": "array", "items": {"type": "string"}, "maxItems": 42}
    SchemaValidator.check_schema(json_schema)
    return ArraySchema(json_schema, AtomicSchema(json_schema["items"]))

@pytest.fixture
def depends_on_array_schema():
    json_schema = {"type": "array", "items": {"type": "string"}, "maxItemsDependsOn": {"$ref": "#otherField"}}
    SchemaValidator.check_schema(json_schema)
    return DependsOnArraySchema(json_schema, AtomicSchema(json_schema["items"]), ref_to="#otherField")

@pytest.fixture
def one_of_schema():
    json_schema = {"oneOf": [{"type": "number"}, {"type": "string"}]}
    SchemaValidator.check_schema(json_schema)
    return OneOfSchema(json_schema, [AtomicSchema(json_schema["oneOf"][0]), AtomicSchema(json_schema["oneOf"][1])])

@pytest.fixture
def ref_to_schema():
    json_schema = {"type": "object", "properties": {"a": {"$anchor": "a", "type": "string"}, "b": {"$ref": "#a"}}}
    SchemaValidator.check_schema(json_schema)
    a_field = AtomicSchema(json_schema["properties"]["a"])
    return ObjectSchema(json_schema, {"a": a_field, "b": RefToSchema(json_schema["properties"]["b"], a_field)})

@pytest.fixture
def bad_schema():
    json_schema = {"description": "no type"}
    SchemaValidator.check_schema(json_schema)
    return AtomicSchema(json_schema)

@pytest.fixture
def bad_ref_to_schema():
    json_schema = {"type": "object", "properties": {"a": {"$anchor": "a", "type": "string"}, "b": {"$ref": "#a"}}}
    SchemaValidator.check_schema(json_schema)
    a_field = AtomicSchema(json_schema["properties"]["a"])
    return ObjectSchema(json_schema, {"a": a_field, "b": RefToSchema(json_schema["properties"]["b"], None)})

def test_schema_class(atomic_schema, object_schema, array_schema, depends_on_array_schema, one_of_schema, ref_to_schema):
    atomic_nav = Mock(name="atomic_nav", value=Mock(return_value=42))
    assert atomic_schema.type == "number"
    assert atomic_schema.json() == {"type": "number"}
    assert repr(atomic_schema) == "AtomicSchema({'type': 'number'})"
    assert list(atomic_schema.dump_iter(atomic_nav)) == [(0, AtomicSchema({'type': 'number'}), (), 42)]

    object_nav = Mock()
    object_nav.name=Mock(return_value=atomic_nav)  # Two-step dance required to avoid special-case use of "name"
    assert object_schema.type == "object"
    assert repr(object_schema) == (
        "ObjectSchema({'type': 'object', 'properties': {'field': {'type': 'number'}}}, {'field': AtomicSchema({'type': 'number'})})"
    )
    assert list(object_schema.dump_iter(object_nav)) == [
        (0, ObjectSchema({'type': 'object', 'properties': {'field': {'type': 'number'}}}, {'field': AtomicSchema({'type': 'number'})}),  (),  None),
        (1, AtomicSchema({'type': 'number'}), (), 42)
    ]

    array_nav = Mock(name="array_nav", index=Mock(return_value=atomic_nav))
    assert array_schema.type == "array"
    assert array_schema.maxItems == 42
    assert repr(array_schema) == (
        "ArraySchema({'type': 'array', 'items': {'type': 'string'}, 'maxItems': 42}, "
        "AtomicSchema({'type': 'string'}))"
    )
    assert list(array_schema.dump_iter(array_nav)) == [
        (0, ArraySchema({'type': 'array', 'items': {'type': 'string'}, 'maxItems': 42}, AtomicSchema({'type': 'string'})), (), None),
        (1, AtomicSchema({'type': 'string'}), (), 42)
    ]

    assert depends_on_array_schema.type == "array"
    with pytest.raises(TypeError):
        depends_on_array_schema.maxItems
    assert repr(depends_on_array_schema) == (
        "DependsOnArraySchema({'type': 'array', 'items': {'type': 'string'}, "
        "'maxItemsDependsOn': {'$ref': '#otherField'}}, AtomicSchema({'type': "
        "'string'}))"
    )

    assert one_of_schema.type == "oneOf"
    assert one_of_schema.alternatives[0].type == "number"
    assert one_of_schema.alternatives[1].type == "string"
    assert repr(one_of_schema) == (
        "OneOfSchema({'oneOf': [{'type': 'number'}, {'type': 'string'}]}, "
        "[AtomicSchema({'type': 'number'}), AtomicSchema({'type': 'string'})])"
    )
    assert list(one_of_schema.dump_iter(atomic_nav)) == [
        (0, OneOfSchema({'oneOf':[{'type': 'number'}, {'type': 'string'}]}, [AtomicSchema({'type': 'number'}), AtomicSchema({'type': 'string'})]), (), None),
        (1, AtomicSchema({'type': 'number'}), (), 42),
        (1, AtomicSchema({'type': 'string'}), (), 42)
    ]

    assert ref_to_schema.properties["b"].type == "string"
    assert ref_to_schema.properties["b"].attributes == {'$anchor': 'a', 'type': 'string'}
    with pytest.raises(AttributeError):
        assert ref_to_schema.properties["b"].properties
    with pytest.raises(AttributeError):
        assert ref_to_schema.properties["b"].items
    assert repr(ref_to_schema.properties["b"]) == "RefToSchema({'$ref': '#a'}, #a)"
    assert repr(ref_to_schema.properties["a"]) == "AtomicSchema({'$anchor': 'a', 'type': 'string'})"
    nav = Mock()
    nav.name = Mock(side_effect=lambda name: {"a": Mock(value=Mock(return_value="42")), "b": Mock()}.get(name))
    assert list(ref_to_schema.dump_iter(nav)) == [
        (0, ObjectSchema({'type': 'object', 'properties': {'a': {'$anchor': 'a', 'type': 'string'}, 'b': {'$ref': '#a'}}}, {'a': AtomicSchema({'$anchor': 'a', 'type': 'string'}), 'b': RefToSchema({'$ref': '#a'}, "#a")}), (), None),
        (1, AtomicSchema({'$anchor': 'a', 'type': 'string'}), (), '42'),
        (1, RefToSchema({'$ref': '#a'}, '#a'), (), None)
    ]

def test_bad_schema_class(bad_schema, bad_ref_to_schema):
    with pytest.raises(ValueError):
        assert bad_ref_to_schema.properties["b"].type is None
    with pytest.raises(ValueError):
        assert bad_ref_to_schema.properties["b"].attributes is None
    with pytest.raises(ValueError):
        assert bad_ref_to_schema.properties["b"].properties is None
    with pytest.raises(ValueError):
        assert bad_ref_to_schema.properties["b"].items is None

    with pytest.raises(KeyError):
        assert bad_schema.type is None

### Schema Maker

def test_schema_maker_1():
    schema_json_1 = {"title": "integer example", "type": "integer"}
    SchemaValidator.check_schema(schema_json_1)

    s = SchemaMaker.from_json(schema_json_1)
    assert s == AtomicSchema({'title': 'integer example', 'type': 'integer'})


def test_schema_maker_2(capsys):
    schema_json_2 = {"title": "array example", "type": "array", "items": {"type": "string"}}
    SchemaValidator.check_schema(schema_json_2)

    s = SchemaMaker.from_json(schema_json_2)
    s.print()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ArraySchema({'title': 'array example', 'type': 'array'})",
        "  AtomicSchema({'type': 'string'})",
    ]

def test_schema_maker_3(capsys):
    schema_json_3 = {"title": "object example", "type": "object", "properties": {"num": {"type": "number"}, "den": {"type": "number"}}}
    SchemaValidator.check_schema(schema_json_3)

    s = SchemaMaker.from_json(schema_json_3)
    s.print()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ObjectSchema({'title': 'object example', 'type': 'object'})",
        "  AtomicSchema({'type': 'number'})",
        "  AtomicSchema({'type': 'number'})",
    ]


def test_schema_maker_5(capsys):
    """
    To provide COBOL-style names to OneOf/REDEFINES schema, we impose a rule that each alternative must be an object, and each object must have at most one named property.
    """
    schema_json_5 = {
        "title": "REDEFINES-RECORD",
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "type": "object",
        "properties": {
          "REDEFINES-A": {
            "oneOf": [
              {
                "title": "A",
                "$anchor": "REDEFINES-A.A",
                "type": "object",
                "properties": {
                  "A": {
                    "title": "A",
                    "$anchor": "A",
                    "cobol": "05 A PICTURE X(6)",
                    "type": "string",
                    "contentEncoding": "cp037"
                  }
                }
              },
              {
                "title": "B",
                "$anchor": "REDEFINES-A.B",
                "type": "object",
                "properties": {
                  "B": {
                    "title": "B",
                    "$anchor": "B",
                    "cobol": "05 B REDEFINES A",
                    "type": "object",
                    "properties": {
                      "B-1": {
                        "title": "B-1",
                        "$anchor": "B-1",
                        "cobol": "10 B-1 PICTURE X(2)",
                        "type": "string",
                        "contentEncoding": "cp037"
                      },
                      "B-2": {
                        "title": "B-2",
                        "$anchor": "B-2",
                        "cobol": "10 B-2 PICTURE 9(4)",
                        "type": "string",
                        "contentEncoding": "cp037"
                      }
                    }
                  }
                }
              }
            ],
            "$anchor": "REDEFINES-A"
          },
          "A": {
            "title": "A",
            "$anchor": "A",
            "cobol": "05 A PICTURE X(6)",
            "$ref": "#REDEFINES-A.A"
          },
          "B": {
            "title": "B",
            "$anchor": "B",
            "cobol": "05 B REDEFINES A",
            "$ref": "#REDEFINES-A.B"
          },
          "C": {
            "title": "C",
            "$anchor": "C",
            "cobol": "05 C PICTURE 99V99",
            "type": "string",
            "contentEncoding": "cp037",
            "conversion": "decimal"
          }
        }
    }
    SchemaValidator.check_schema(schema_json_5)
    s = SchemaMaker.from_json(schema_json_5)
    s.print()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ObjectSchema({'title': 'REDEFINES-RECORD', '$anchor': 'REDEFINES-RECORD', 'cobol': '01 REDEFINES-RECORD', 'type': 'object'})",
        "  OneOfSchema({'$anchor': 'REDEFINES-A'})",
        "    ObjectSchema({'title': 'A', '$anchor': 'REDEFINES-A.A', 'type': 'object'})",
        "      AtomicSchema({'title': 'A', '$anchor': 'A', 'cobol': '05 A PICTURE X(6)', 'type': 'string', 'contentEncoding': 'cp037'})",
        "    ObjectSchema({'title': 'B', '$anchor': 'REDEFINES-A.B', 'type': 'object'})",
        "      ObjectSchema({'title': 'B', '$anchor': 'B', 'cobol': '05 B REDEFINES A', 'type': 'object'})",
        "        AtomicSchema({'title': 'B-1', '$anchor': 'B-1', 'cobol': '10 B-1 PICTURE X(2)', 'type': 'string', 'contentEncoding': 'cp037'})",
        "        AtomicSchema({'title': 'B-2', '$anchor': 'B-2', 'cobol': '10 B-2 PICTURE 9(4)', 'type': 'string', 'contentEncoding': 'cp037'})",
        "  RefToSchema({'title': 'A', '$anchor': 'A', 'cobol': '05 A PICTURE X(6)', '$ref': '#REDEFINES-A.A'})",
        "  RefToSchema({'title': 'B', '$anchor': 'B', 'cobol': '05 B REDEFINES A', '$ref': '#REDEFINES-A.B'})",
        "  AtomicSchema({'title': 'C', '$anchor': 'C', 'cobol': '05 C PICTURE 99V99', 'type': 'string', 'contentEncoding': 'cp037', 'conversion': 'decimal'})",
    ]

def test_schema_maker_forward_ref(capsys):
    schema_json_fwd = {
        "title": "REDEFINES-RECORD",
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "type": "object",
        "properties": {
          "A": {
            "title": "A",
            "$anchor": "A",
            "cobol": "05 A PICTURE X(6)",
            "$ref": "#REDEFINES-A.A"
          },
          "B": {
            "title": "B",
            "$anchor": "B",
            "cobol": "05 B REDEFINES A",
            "$ref": "#REDEFINES-A.B"
          },
          "REDEFINES-A": {
            "oneOf": [
                {
                    "title": "A",
                    "$anchor": "REDEFINES-A.A",
                    "type": "object",
                    "properties": {
                        "A": {
                            "title": "A",
                            "$anchor": "A",
                            "cobol": "05 A PICTURE X(6)",
                            "type": "string",
                            "contentEncoding": "cp037"
                        }
                    }
                },
                {
                    "title": "B",
                    "$anchor": "REDEFINES-A.B",
                    "type": "object",
                    "properties": {
                        "B": {
                            "title": "B",
                            "$anchor": "B",
                            "cobol": "05 B PICTURE 9(6)",
                            "type": "number",
                        }
                    }
                }
            ],
            "$anchor": "REDEFINES-A"
            }
        },
    }
    SchemaValidator.check_schema(schema_json_fwd)
    s = SchemaMaker.from_json(schema_json_fwd)
    s.print()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ObjectSchema({'title': 'REDEFINES-RECORD', '$anchor': 'REDEFINES-RECORD', 'cobol': '01 REDEFINES-RECORD', 'type': 'object'})",
        "  RefToSchema({'title': 'A', '$anchor': 'A', 'cobol': '05 A PICTURE X(6)', '$ref': '#REDEFINES-A.A'})",
        "  RefToSchema({'title': 'B', '$anchor': 'B', 'cobol': '05 B REDEFINES A', '$ref': '#REDEFINES-A.B'})",
        "  OneOfSchema({'$anchor': 'REDEFINES-A'})",
        "    ObjectSchema({'title': 'A', '$anchor': 'REDEFINES-A.A', 'type': 'object'})",
        "      AtomicSchema({'title': 'A', '$anchor': 'A', 'cobol': '05 A PICTURE X(6)', 'type': 'string', 'contentEncoding': 'cp037'})",
        "    ObjectSchema({'title': 'B', '$anchor': 'REDEFINES-A.B', 'type': 'object'})",
        "      AtomicSchema({'title': 'B', '$anchor': 'B', 'cobol': '05 B PICTURE 9(6)', 'type': 'number'})"
    ]

def test_schema_maker_bad_depends_on_ref():
    json_schema = {"type": "array", "items": {"type": "string"}, "maxItemsDependsOn": {"$ref": "#otherField"}}
    with pytest.raises(ValueError) as exception_info:
        s = SchemaMaker.from_json(json_schema)
    assert exception_info.value.args[0] == "unknown $ref in {'type': 'array', 'items': {'type': 'string'}, 'maxItemsDependsOn': {'$ref': '#otherField'}}; forward references for maxItemsDependsOn aren't supported"

def test_schema_maker_bad_forward_ref(capsys):
    schema_json_fwd = {
        "title": "REDEFINES-RECORD",
        "$anchor": "REDEFINES-RECORD",
        "cobol": "01 REDEFINES-RECORD",
        "type": "object",
        "properties": {
          "A": {
            "title": "A",
            "$anchor": "A",
            "cobol": "05 A PICTURE X(6)",
            "$ref": "#REDEFINES-A.A"
          },
          "B": {
            "title": "B",
            "$anchor": "B",
            "cobol": "05 B REDEFINES A",
            "$ref": "#REDEFINES-A.B"
          },
        },
    }
    SchemaValidator.check_schema(schema_json_fwd)
    with pytest.raises(ValueError) as exception_info:
        s = SchemaMaker.from_json(schema_json_fwd)
    assert exception_info.value.args[0] == "Cannot resolve '#REDEFINES-A.A' in ['A', 'B', 'REDEFINES-RECORD']"

def test_schema_maker_bad_type():
    json_schema = {"type": "some-extension", "otherStuff": {"$ref": "#otherField"}}
    with pytest.raises(ValueError) as exception_info:
        s = SchemaMaker.from_json(json_schema)
    assert exception_info.value.args[0] == (
        "Unknown source['type']='some-extension' in {'type': 'some-extension', "
        "'otherStuff': {'$ref': '#otherField'}}"
    )


### Unpackers

# TODO: Test with mocks before these integration tests

def test_cobol_ebcdic_unpacker():
    json_schema = {"type": "string", "cobol": "USAGE DISPLAY PICTURE X(42)"}
    expected = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    eu = EBCDIC()
    assert eu.calcsize(schema) == 42
    assert eu.value(schema, expected.encode("CP037")) == expected
    nav = eu.nav(schema, expected.encode("CP037"))
    assert nav.schema == schema

def test_bad_ebcdic_unpacker():
    json_schema = {"type": "string", "cobol": "USAGE DISPLAY"}
    expected = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    eu = EBCDIC()
    with pytest.raises(ValueError):
        assert eu.calcsize(schema) == 42
    assert eu.value(schema, expected.encode("CP037")) == expected


def test_cobol_text_unpacker():
    json_schema = {"type": "string", "cobol": "USAGE DISPLAY PICTURE X(42)"}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    tu = TextUnpacker()
    assert tu.calcsize(schema) == 42
    assert tu.value(schema, data) == data
    nav = tu.nav(schema, data)
    assert nav.schema == schema

def test_generic_text_unpacker():
    json_schema = {"type": "string", "maxLength": 42}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    tu = TextUnpacker()
    assert tu.calcsize(schema) == 42
    assert tu.value(schema, data) == data
    nav = tu.nav(schema, data)
    assert nav.schema == schema

def test_bad_text_unpacker():
    json_schema = {"type": "string", "regex": ".{42}"}
    schema = SchemaMaker.from_json(json_schema)
    tu = TextUnpacker()
    with pytest.raises(ValueError):
        assert tu.calcsize(schema) == 42

def test_delimited_unpacker():
    json_schema = {"type": "string", "maxLength": 42}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    du = Delimited()
    assert du.calcsize(schema) == 1
    assert du.value(schema, data) == data
    nav = du.nav(schema, data)
    assert nav.schema == schema

def test_csv_unpacker():
    json_schema = {"type": "number"}
    data = "42"
    schema = SchemaMaker.from_json(json_schema)
    cu = CSVUnpacker()
    assert cu.calcsize(schema) == 1
    assert cu.value(schema, data) == 42.0
    nav = cu.nav(schema, data)
    assert nav.schema == schema

def test_json_unpacker():
    json_schema = {"type": "number"}
    data = "42"
    schema = SchemaMaker.from_json(json_schema)
    ju = JSONUnpacker()
    assert ju.calcsize(schema) == 1
    assert ju.value(schema, data) == 42.0
    nav = ju.nav(schema, data)
    assert nav.schema == schema


### Instance & Unpacker Integration Tests

def test_bytes_instance(capsys):
    schema = SchemaMaker.from_json({"type": "object", "properties": {"field-1": {"type": "string", "cobol": "PIC X(12)"}}})
    data = BytesInstance('blahblahblah'.encode("CP037"))
    nav = EBCDIC().nav(schema, data)
    assert nav.name("field-1").value() == 'blahblahblah'
    nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "Field                                         Off Sz  Raw Value",
        "                                                0  12 '' ''",
        "  PIC X(12)                                     0  12 b'\\x82\\x93\\x81\\x88\\x82\\x93\\x81\\x88\\x82\\x93\\x81\\x88' 'blahblahblah'",
    ]


### Locations

def test_atomic_location():
    json_schema = {"type": "string", "cobol": "USAGE DISPLAY PICTURE X(42)"}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = AtomicLocation(schema, 0, 42)
    loc.unpacker = TextUnpacker()
    assert repr(loc) == (
        "AtomicLocation(AtomicSchema({'type': 'string', 'cobol': 'USAGE DISPLAY "
        "PICTURE X(42)'}), 0, 42)"
    )
    assert str(loc) == 'USAGE DISPLAY PICTURE X(42) 0 42'
    assert loc.value(data) == data
    assert loc.raw(data) == data

def test_cobol_array_location():
    json_schema = {"type": "array", "maxItems": 4, "cobol": "OCCURS 4 TIMES USAGE DISPLAY PICTURE X(10)",
                   "items": {"type": "string", "cobol": "USAGE DISPLAY PICTURE X(10)"}}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = ArrayLocation(schema, 10, 4, AtomicLocation(schema.items, 0, 10), 0, 40)
    loc.unpacker = TextUnpacker()
    loc.items.unpacker = TextUnpacker()
    assert repr(loc) == (
        "ArrayLocation(ArraySchema({'type': 'array', 'maxItems': 4, 'cobol': 'OCCURS "
        "4 TIMES USAGE DISPLAY PICTURE X(10)', 'items': {'type': 'string', 'cobol': "
        "'USAGE DISPLAY PICTURE X(10)'}}, AtomicSchema({'type': 'string', 'cobol': "
        "'USAGE DISPLAY PICTURE X(10)'})), 10, 4, USAGE DISPLAY PICTURE X(10) 0 10, "
        '0, 40)'
    )
    assert str(loc) == 'OCCURS 4 TIMES USAGE DISPLAY PICTURE X(10) 0 40'
    assert loc.value(data) == ['12345678 0', ' 2345678 0', ' 2345678 0', ' 2345678 0']
    assert loc.raw(data) == '12345678 0 2345678 0 2345678 0 2345678 0'

def test_generic_array_location():
    json_schema = {"type": "array", "maxItems": 4,
                   "items": {"type": "string", "cobol": "USAGE DISPLAY PICTURE X(10)"}}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = ArrayLocation(schema, 10, 4, AtomicLocation(schema.items, 0, 10), 0, 40)
    assert repr(loc) == (
        "ArrayLocation(ArraySchema({'type': 'array', 'maxItems': 4, 'items': {'type': "
        "'string', 'cobol': 'USAGE DISPLAY PICTURE X(10)'}}, AtomicSchema({'type': "
        "'string', 'cobol': 'USAGE DISPLAY PICTURE X(10)'})), 10, 4, USAGE DISPLAY "
        'PICTURE X(10) 0 10, 0, 40)'
    )
    assert str(loc) == 'Array [4] 0 40'

def test_cobol_object_location():
    json_schema = {"type": "object", "cobol": "01 RECORD", "properties": {"a": {"$anchor": "a", "type": "string", "cobol": "10 A USAGE DISPLAY PICTURE X(42)"}}}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = ObjectLocation(schema, {"a": AtomicLocation(schema.properties["a"], 0, 42)}, 0, 42)
    loc.unpacker = TextUnpacker()
    loc.properties["a"].unpacker = TextUnpacker()
    assert repr(loc) == (
         "ObjectLocation(ObjectSchema({'type': 'object', 'cobol': '01 RECORD', "
         "'properties': {'a': {'$anchor': 'a', 'type': 'string', 'cobol': '10 A USAGE "
         "DISPLAY PICTURE X(42)'}}}, {'a': AtomicSchema({'$anchor': 'a', 'type': "
         "'string', 'cobol': '10 A USAGE DISPLAY PICTURE X(42)'})}), {'a': "
         "AtomicLocation(AtomicSchema({'$anchor': 'a', 'type': 'string', 'cobol': '10 "
         "A USAGE DISPLAY PICTURE X(42)'}), 0, 42)}, 0, 42)"
    )
    assert str(loc) == '01 RECORD 0 42'
    assert loc.value(data) == {'a': '12345678 0 2345678 0 2345678 0 2345678 0 2'}
    assert loc.raw(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'

def test_generic_object_location():
    json_schema = {"type": "object", "properties": {"a": {"$anchor": "a", "type": "string", "cobol": "USAGE DISPLAY PICTURE X(42)"}}}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = ObjectLocation(schema, {"a": AtomicLocation(schema.properties["a"], 0, 42)}, 0, 42)
    assert repr(loc) == (
        "ObjectLocation(ObjectSchema({'type': 'object', 'properties': {'a': "
        "{'$anchor': 'a', 'type': 'string', 'cobol': 'USAGE DISPLAY PICTURE "
        "X(42)'}}}, {'a': AtomicSchema({'$anchor': 'a', 'type': 'string', 'cobol': "
        "'USAGE DISPLAY PICTURE X(42)'})}), {'a': "
        "AtomicLocation(AtomicSchema({'$anchor': 'a', 'type': 'string', 'cobol': "
        "'USAGE DISPLAY PICTURE X(42)'}), 0, 42)}, 0, 42)"
    )
    assert str(loc) == 'Object 0 42'


def test_cobol_oneof_location():
    json_schema = {"oneOf": [{"$anchor": "a", "type": "string", "cobol": "10 A USAGE DISPLAY PICTURE X(42)"}, {"$anchor": "b", "type": "number", "cobol": "10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A"}]}
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    loc = OneOfLocation(schema, [AtomicLocation(schema.alternatives[0], 0, 42), AtomicLocation(schema.alternatives[1], 0, 8)], 0, 42)
    loc.unpacker = TextUnpacker()
    loc.alternatives["a"].unpacker = TextUnpacker()
    loc.alternatives["b"].unpacker = TextUnpacker()
    assert repr(loc) == (
        "OneOfLocation(OneOfSchema({'oneOf': [{'$anchor': 'a', 'type': 'string', "
        "'cobol': '10 A USAGE DISPLAY PICTURE X(42)'}, {'$anchor': 'b', 'type': "
        "'number', 'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'}]}, "
        "[AtomicSchema({'$anchor': 'a', 'type': 'string', 'cobol': '10 A USAGE "
        "DISPLAY PICTURE X(42)'}), AtomicSchema({'$anchor': 'b', 'type': 'number', "
        "'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'})]), "
        "[AtomicLocation(AtomicSchema({'$anchor': 'a', 'type': 'string', 'cobol': '10 "
        "A USAGE DISPLAY PICTURE X(42)'}), 0, 42), "
        "AtomicLocation(AtomicSchema({'$anchor': 'b', 'type': 'number', 'cobol': '10 "
        "B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'}), 0, 8)], 0, 42)"
    )
    assert str(loc) == 'OneOf [a, b] 0 42'
    assert loc.value(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'
    assert loc.raw(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'

def test_cobol_refto_location():
    json_schema = {
        "type": "object",
        "cobol": "01 RECORD",
        "properties": {
            "REDEFINES-A": {
                "oneOf": [
                    {"$anchor": "REDEFINES-A.A", "type": "string", "cobol": "10 A USAGE DISPLAY PICTURE X(42)"},
                    {"$anchor": "REDEFINES-A.B", "type": "number", "cobol": "10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A"}]
            },
            "A": {"$anchor": "A", "$ref": "#REDEFINES-A.A"},
            "B": {"$anchor": "B", "$ref": "#REDEFINES-A.B"},
        }
    }
    data = "12345678 0 2345678 0 2345678 0 2345678 0 2"
    schema = SchemaMaker.from_json(json_schema)
    a_location = AtomicLocation(schema.properties["REDEFINES-A"].alternatives[0], 0, 42)
    b_location = AtomicLocation(schema.properties["REDEFINES-A"].alternatives[1], 0, 8)
    anchors = {"REDEFINES-A.A": a_location, "REDEFINES-A.B": b_location}
    loc = ObjectLocation(
        schema,
        {
            "REDEFINES-A": OneOfLocation(schema.properties["REDEFINES-A"], [a_location, b_location], 0, 42),
            "A": RefToLocation(schema.properties["A"], anchors, 0, 42),
            "B": RefToLocation(schema.properties["B"], anchors, 0, 8),
        },
        0, 42)
    loc.unpacker = TextUnpacker()
    loc.properties["REDEFINES-A"].unpacker = TextUnpacker()
    loc.properties["A"].unpacker = TextUnpacker()
    loc.properties["B"].unpacker = TextUnpacker()
    a_location.unpacker = TextUnpacker()
    b_location.unpacker = TextUnpacker()
    assert repr(loc) == (
        "ObjectLocation(ObjectSchema({'type': 'object', 'cobol': '01 RECORD', "
        "'properties': {'REDEFINES-A': {'oneOf': [{'$anchor': 'REDEFINES-A.A', "
        "'type': 'string', 'cobol': '10 A USAGE DISPLAY PICTURE X(42)'}, {'$anchor': "
        "'REDEFINES-A.B', 'type': 'number', 'cobol': '10 B USAGE DISPLAY PICTURE "
        "9999V9999 REDEFINES A'}]}, 'A': {'$anchor': 'A', '$ref': '#REDEFINES-A.A'}, "
        "'B': {'$anchor': 'B', '$ref': '#REDEFINES-A.B'}}}, {'REDEFINES-A': "
        "OneOfSchema({'oneOf': [{'$anchor': 'REDEFINES-A.A', 'type': 'string', "
        "'cobol': '10 A USAGE DISPLAY PICTURE X(42)'}, {'$anchor': 'REDEFINES-A.B', "
        "'type': 'number', 'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES "
        "A'}]}, [AtomicSchema({'$anchor': 'REDEFINES-A.A', 'type': 'string', 'cobol': "
        "'10 A USAGE DISPLAY PICTURE X(42)'}), AtomicSchema({'$anchor': "
        "'REDEFINES-A.B', 'type': 'number', 'cobol': '10 B USAGE DISPLAY PICTURE "
        "9999V9999 REDEFINES A'})]), 'A': RefToSchema({'$anchor': 'A', '$ref': "
        "'#REDEFINES-A.A'}, #REDEFINES-A.A), 'B': RefToSchema({'$anchor': 'B', '$ref': "
        "'#REDEFINES-A.B'}, #REDEFINES-A.B)}), {'REDEFINES-A': OneOfLocation(OneOfSchema({'oneOf': "
        "[{'$anchor': 'REDEFINES-A.A', 'type': 'string', 'cobol': '10 A USAGE DISPLAY "
        "PICTURE X(42)'}, {'$anchor': 'REDEFINES-A.B', 'type': 'number', 'cobol': '10 "
        "B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'}]}, [AtomicSchema({'$anchor': "
        "'REDEFINES-A.A', 'type': 'string', 'cobol': '10 A USAGE DISPLAY PICTURE "
        "X(42)'}), AtomicSchema({'$anchor': 'REDEFINES-A.B', 'type': 'number', "
        "'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'})]), "
        "[AtomicLocation(AtomicSchema({'$anchor': 'REDEFINES-A.A', 'type': 'string', "
        "'cobol': '10 A USAGE DISPLAY PICTURE X(42)'}), 0, 42), "
        "AtomicLocation(AtomicSchema({'$anchor': 'REDEFINES-A.B', 'type': 'number', "
        "'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'}), 0, 8)], 0, "
        "42), 'A': RefToLocation(RefToSchema({'$anchor': 'A', '$ref': "
        "'#REDEFINES-A.A'}, #REDEFINES-A.A), 0, 42), 'B': RefToLocation(RefToSchema({'$anchor': 'B', "
        "'$ref': '#REDEFINES-A.B'}, #REDEFINES-A.B), 0, 8)}, 0, 42)"
    )

    assert str(loc.properties["A"]) == 'RefTo #REDEFINES-A.A 0 42'
    assert str(loc.properties["B"]) == 'RefTo #REDEFINES-A.B 0 8'

    with pytest.raises(AttributeError):
        loc.properties["B"].properties
    with pytest.raises(AttributeError):
        loc.properties["B"].items

    assert loc.properties["B"].referent == AtomicLocation(AtomicSchema({'$anchor': 'REDEFINES-A.B', 'type': 'number', 'cobol': '10 B USAGE DISPLAY PICTURE 9999V9999 REDEFINES A'}), 0, 8)

    assert loc.value(data) == {
        'A': '12345678 0 2345678 0 2345678 0 2345678 0 2',
        'B': 12345678.0,
        'REDEFINES-A': '12345678 0 2345678 0 2345678 0 2345678 0 2'
    }
    assert loc.raw(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'
    assert loc.properties["A"].value(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'
    assert loc.properties["B"].value(data) == Decimal('12345678.0')
    assert loc.properties["A"].raw(data) == '12345678 0 2345678 0 2345678 0 2345678 0 2'
    assert loc.properties["B"].raw(data) == '12345678'

### Location Maker

def test_schema_location_maker(capsys):
    schema = SchemaMaker.from_json(
        {
            "title": "COBOL Example",
            "type": "object",
            "cobol": "01  SAMPLE-RECORD",
            "$anchor": "SAMPLE-RECORD",
            "properties": {
                "field-1": {"$anchor": "field-1", "type": "string", "contentEncoding": "cp037", "cobol": "05  FIELD-1 PIC X(10)"},
                "field-2": {"$anchor": "field-2", "type": "string", "contentEncoding": "packed-decimal", "conversion": "decimal", "cobol": "05  FIELD-2 PIC S9(5)V99"},
            }
        }
    )

    lm = LocationMaker(EBCDIC(), schema)
    loc = lm.from_schema()

    print(repr(loc))
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ObjectLocation(ObjectSchema({'title': 'COBOL Example', 'type': 'object', 'cobol': '01  SAMPLE-RECORD', '$anchor': 'SAMPLE-RECORD', 'properties': {'field-1': {'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}, 'field-2': {'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'}}}, {'field-1': AtomicSchema({'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}), 'field-2': AtomicSchema({'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'})}), {'field-1': AtomicLocation(AtomicSchema({'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}), 0, 10), 'field-2': AtomicLocation(AtomicSchema({'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'}), 10, 18)}, 0, 18)"
    ]

    buffer = 'ABCDEFGHIJ 1234599'.encode("CP037")
    assert loc.properties["field-1"].value(buffer) == 'ABCDEFGHIJ'
    assert loc.properties["field-2"].value(buffer) == Decimal('12345.99')

    lm.ndnav(buffer).dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "Field                                         Off Sz  Raw Value",
        "01  SAMPLE-RECORD                               0  18 '' ''",
        "  05  FIELD-1 PIC X(10)                         0  10 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8\\xc9\\xd1' 'ABCDEFGHIJ'",
        "  05  FIELD-2 PIC S9(5)V99                     10   8 b'@\\xf1\\xf2\\xf3\\xf4\\xf5\\xf9\\xf9' Decimal('12345.99')",
    ]

def test_bad_schema_location_maker(capsys):
    schema = SchemaMaker.from_json(
        {
            "title": "Occurs Depending On",
            "type": "object",
            "cobol": "01  SAMPLE-RECORD",
            "$anchor": "SAMPLE-RECORD",
            "properties": {
                "FIELD-1": {"$anchor": "FIELD-1",
                            "type": "number",
                            "contentEncoding": "packed-decimal",
                            "cobol": "05  FIELD-1 PIC S999"},
                "FIELD-2": {"$anchor": "FIELD-2",
                            "type": "array",
                            "cobol": "05  FIELD-2 OCCURS DEPENDING ON FIELD-1 PIC X(4)",
                            "maxItemsDependsOn": {"$ref": "#FIELD-1"},
                            "items": {
                                "type": "string",
                                "contentEncoding": "cp037",
                                "cobol": "PIC X(4)"
                            },
                        }
            }
        }
    )

    lm = LocationMaker(EBCDIC(), schema)
    with pytest.raises(ValueError) as exception_info:
        loc = lm.from_schema()
    assert exception_info.value.args[0] == 'an Occurs Depending On requires an instance'

    buffer = '0003ABCDEFGHIJKL'.encode("CP037")
    loc = lm.from_instance(buffer)
    assert loc.properties["FIELD-1"].value(buffer) == Decimal('3')
    assert loc.properties["FIELD-2"].value(buffer) == ['ABCD', 'EFGH', 'IJKL']

    lm.ndnav(buffer).dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01  SAMPLE-RECORD                               0  16 '' ''",
        "  05  FIELD-1 PIC S999                          0   4 b'\\xf0\\xf0\\xf0\\xf3' Decimal('3')",
        "  05  FIELD-2 OCCURS DEPENDING ON FIELD-1 PIC X(4)   4  12 '' ''",
        "    PIC X(4)                                    4   4 b'\\xc1\\xc2\\xc3\\xc4' 'ABCD'"
    ]

def test_instance_location_maker(capsys):
    schema = SchemaMaker.from_json(
        {
            "title": "COBOL Example",
            "type": "object",
            "cobol": "01  SAMPLE-RECORD",
            "$anchor": "SAMPLE-RECORD",
            "properties": {
                "field-1": {"$anchor": "field-1", "type": "string", "contentEncoding": "cp037", "cobol": "05  FIELD-1 PIC X(10)"},
                "field-2": {"$anchor": "field-2", "type": "string", "contentEncoding": "packed-decimal", "conversion": "decimal", "cobol": "05  FIELD-2 PIC S9(5)V99"},
            }
        }
    )

    lm = LocationMaker(EBCDIC(), schema)
    buffer = 'ABCDEFGHIJ 1234599'.encode("CP037")
    loc = lm.from_instance(buffer)

    print(repr(loc))
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "ObjectLocation(ObjectSchema({'title': 'COBOL Example', 'type': 'object', 'cobol': '01  SAMPLE-RECORD', '$anchor': 'SAMPLE-RECORD', 'properties': {'field-1': {'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}, 'field-2': {'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'}}}, {'field-1': AtomicSchema({'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}), 'field-2': AtomicSchema({'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'})}), {'field-1': AtomicLocation(AtomicSchema({'$anchor': 'field-1', 'type': 'string', 'contentEncoding': 'cp037', 'cobol': '05  FIELD-1 PIC X(10)'}), 0, 10), 'field-2': AtomicLocation(AtomicSchema({'$anchor': 'field-2', 'type': 'string', 'contentEncoding': 'packed-decimal', 'conversion': 'decimal', 'cobol': '05  FIELD-2 PIC S9(5)V99'}), 10, 18)}, 0, 18)"
    ]
    assert loc.properties["field-1"].value(buffer) == 'ABCDEFGHIJ'
    assert loc.properties["field-2"].value(buffer) == Decimal('12345.99')

    lm.ndnav(buffer).dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "Field                                         Off Sz  Raw Value",
        "01  SAMPLE-RECORD                               0  18 '' ''",
        "  05  FIELD-1 PIC X(10)                         0  10 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8\\xc9\\xd1' 'ABCDEFGHIJ'",
        "  05  FIELD-2 PIC S9(5)V99                     10   8 b'@\\xf1\\xf2\\xf3\\xf4\\xf5\\xf9\\xf9' Decimal('12345.99')",
    ]


### Test cases of COBOL schema

# We've manually translated COBOL DDE's into JSON Schema for these tests.
# See `cobol_parser` for the COBOL Parser that emits JSON Schema.

import pytest
from io import StringIO
import csv
from stingray.schema_instance import *

@pytest.fixture
def copy_t1_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Simple Record Case 1

        01  SOME-RECORD-1.
            05  SOME-COLUMN PIC X(5).

    """
    test_1_schema = {
      "title": "SOME-RECORD-1",
      "$anchor": "SOME-RECORD-1",
      "cobol": "01 SOME-RECORD-1",
      "type": "object",
      "properties": {
        "SOME-COLUMN": {
          "title": "SOME-COLUMN",
          "$anchor": "SOME-COLUMN",
          "cobol": "05 SOME-COLUMN PIC X(5)",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }
    assert SchemaValidator.check_schema(test_1_schema) is None
    test_1_data = BytesInstance('12345'.encode("CP037"))
    return test_1_schema, test_1_data

def test_copy_t1(copy_t1_schema_data, capsys) -> bool:
    test_1_schema, test_1_data = copy_t1_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_1_schema), test_1_data)
    nav = base_nav.name("SOME-COLUMN")
    assert nav.value() == "12345"
    with pytest.raises(TypeError):
        r = base_nav.index(2)
    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        "Field                                         Off Sz  Raw Value",
        "01 SOME-RECORD-1                                0   5 '' ''",
        "  05 SOME-COLUMN PIC X(5)                       0   5 b'\\xf1\\xf2\\xf3\\xf4\\xf5' '12345'",
    ]


@pytest.fixture
def copy_t2_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Occurs Record Case 2

        01  SOME-RECORD-2.
            05  FILLER PIC X(5).
            05  REPEAT OCCURS 4 PIC XXX.
    """

    test_2_schema = {
      "title": "SOME-RECORD-2",
      "$anchor": "SOME-RECORD-2",
      "cobol": "01 SOME-RECORD-2",
      "type": "object",
      "properties": {
        "FILLER-1": {
          "title": "FILLER",
          "$anchor": "FILLER-1",
          "cobol": "05 FILLER PIC X(5)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "REPEAT": {
          "title": "REPEAT",
          "$anchor": "REPEAT",
          "cobol": "05 REPEAT OCCURS 4 PIC XXX",
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "REPEAT": {
                "title": "REPEAT",
                "$anchor": "REPEAT",
                "cobol": "05 REPEAT OCCURS 4 PIC XXX",
                "type": "string",
                "contentEncoding": "cp037"
              }
            }
          },
          "maxItems": 4
        }
      }
    }

    assert SchemaValidator.check_schema(test_2_schema) is None
    test_2_data = BytesInstance('12345ABCDEFGHIJKL'.encode("CP037"))
    return test_2_schema, test_2_data


def test_copy_t2(copy_t2_schema_data, capsys) -> bool:
    test_2_schema, test_2_data = copy_t2_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_2_schema), test_2_data)
    nav = base_nav.name("REPEAT")
    assert nav.value() == [{'REPEAT': 'ABC'}, {'REPEAT': 'DEF'}, {'REPEAT': 'GHI'}, {'REPEAT': 'JKL'}]
    assert nav.index(0).value() == {'REPEAT': 'ABC'}
    assert nav.index(1).value() == {'REPEAT': 'DEF'}
    assert nav.index(2).value() == {'REPEAT': 'GHI'}
    assert nav.index(3).value() == {'REPEAT': 'JKL'}

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 SOME-RECORD-2                                0  17 '' ''",
        "  05 FILLER PIC X(5)                            0   5 b'\\xf1\\xf2\\xf3\\xf4\\xf5' '12345'",
        "  05 REPEAT OCCURS 4 PIC XXX                    5  12 '' ''",
        "                                                5   3 '' ''",
        "      05 REPEAT OCCURS 4 PIC XXX                5   3 b'\\xc1\\xc2\\xc3' 'ABC'"
    ]

@pytest.fixture
def copy_t3_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Nested Occurs Case 3

        01  SOME-RECORD-3.
            05  FILLER PIC X(5).
            05  REPEAT OCCURS 5.
                10  ITEM OCCURS 4 PIC XX.

    """
    test_3_schema = {
      "title": "SOME-RECORD-3",
      "$anchor": "SOME-RECORD-3",
      "cobol": "01 SOME-RECORD-3",
      "type": "object",
      "properties": {
        "FILLER-1": {
          "title": "FILLER",
          "$anchor": "FILLER-1",
          "cobol": "05 FILLER PIC X(5)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "REPEAT": {
          "title": "REPEAT",
          "$anchor": "REPEAT",
          "cobol": "05 REPEAT OCCURS 5",
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "ITEM": {
                "title": "ITEM",
                "$anchor": "ITEM",
                "cobol": "10 ITEM OCCURS 4 PIC XX",
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                    "ITEM": {
                      "title": "ITEM",
                      "$anchor": "ITEM",
                      "cobol": "10 ITEM OCCURS 4 PIC XX",
                      "type": "string",
                      "contentEncoding": "cp037"
                    }
                  }
                },
                "maxItems": 4
              }
            }
          },
          "maxItems": 5
        }
      }
    }

    assert SchemaValidator.check_schema(test_3_schema) is None
    test_3_data = BytesInstance('12345ABCDEFGHabcdefghIJKLMNOPijklmnopQRSTUVWZ'.encode("CP037"))
    return test_3_schema, test_3_data

def test_copy_t3(copy_t3_schema_data, capsys) -> bool:
    test_3_schema, test_3_data = copy_t3_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_3_schema), test_3_data)
    nav_item_1_0 = base_nav.name("REPEAT").index(1).name("ITEM").index(0)
    assert nav_item_1_0.value() == {'ITEM': 'ab'}, f"{nav_item_1_0.value()=} != {'ITEM': 'ab'}"
    nav_item_1_1 = base_nav.name("REPEAT").index(1).name("ITEM").index(1)
    assert nav_item_1_1.value() == {'ITEM': 'cd'}, f"{nav_item_1_1.value()=} != {'ITEM': 'ab'}"

    nav_item_2_0 = base_nav.name("REPEAT").index(2).name("ITEM").index(0)
    assert nav_item_2_0.value() == {'ITEM': 'IJ'}, f"{nav_item_2_0.value()=} != {'ITEM': 'IJ'}"
    nav_item_2_1 = base_nav.name("REPEAT").index(2).name("ITEM").index(1)
    assert nav_item_2_1.value() == {'ITEM': 'KL'}, f"{nav_item_2_1.value()=} != {'ITEM': 'KL'}"

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 SOME-RECORD-3                                0  45 '' ''",
        "  05 FILLER PIC X(5)                            0   5 b'\\xf1\\xf2\\xf3\\xf4\\xf5' '12345'",
        "  05 REPEAT OCCURS 5                            5  40 '' ''",
        "                                                5   8 '' ''",
        "      10 ITEM OCCURS 4 PIC XX                   5   8 '' ''",
        "                                                5   2 '' ''",
        "          10 ITEM OCCURS 4 PIC XX               5   2 b'\\xc1\\xc2' 'AB'"
    ]

@pytest.fixture
def copy_3_schema_data() -> tuple[JSON, BytesInstance]:
    """
          * COPY3.COB
           01  SURVEY-RESPONSES.
               05  QUESTION-NUMBER OCCURS 10.
                   10  RESPONSE-CATEGORY OCCURS 3.
                       15  ANSWER PIC 99.
    """
    copy_3_schema = {
      "title": "SURVEY-RESPONSES",
      "$anchor": "SURVEY-RESPONSES",
      "cobol": "01 SURVEY-RESPONSES",
      "type": "object",
      "properties": {
        "QUESTION-NUMBER": {
          "title": "QUESTION-NUMBER",
          "$anchor": "QUESTION-NUMBER",
          "cobol": "05 QUESTION-NUMBER           OCCURS 10 TIMES",
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "RESPONSE-CATEGORY": {
                "title": "RESPONSE-CATEGORY",
                "$anchor": "RESPONSE-CATEGORY",
                "cobol": "10 RESPONSE-CATEGORY     OCCURS 3 TIMES",
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                    "ANSWER": {
                      "title": "ANSWER",
                      "$anchor": "ANSWER",
                      "cobol": "15 ANSWER            PIC 99",
                      "type": "string",
                      "contentEncoding": "cp037",
                      "conversion": "decimal"
                    }
                  }
                },
                "maxItems": 3
              }
            }
          },
          "maxItems": 10
        }
      }
    }
    assert SchemaValidator.check_schema(copy_3_schema) is None

    copy_3_data = BytesInstance("111213212223313233414243515253616263717273818283919293010203".encode("CP037"))
    return copy_3_schema, copy_3_data

def test_copy_3(copy_3_schema_data, capsys) -> bool:
    test_4_schema, test_4_data = copy_3_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_4_schema), test_4_data)
    nav_item_0_0 = base_nav.name("QUESTION-NUMBER").index(0).name("RESPONSE-CATEGORY").index(0)
    assert nav_item_0_0.name("ANSWER").value() == Decimal('11'), f'{nav_item_0_0.value()=} != 11'

    nav_item_1_0 = base_nav.name("QUESTION-NUMBER").index(1).name("RESPONSE-CATEGORY").index(0)
    assert nav_item_1_0.name("ANSWER").value() == Decimal('21'), f'{nav_item_1_0.value()=} != 21'
    nav_item_1_1 = base_nav.name("QUESTION-NUMBER").index(1).name("RESPONSE-CATEGORY").index(1)
    assert nav_item_1_1.name("ANSWER").value() == Decimal('22'), f'{nav_item_1_1.value()=} != 22'
    nav_item_1_2 = base_nav.name("QUESTION-NUMBER").index(1).name("RESPONSE-CATEGORY").index(2)
    assert nav_item_1_2.name("ANSWER").value() == Decimal('23'), f'{nav_item_1_2.value()=} != 23'

    nav_item_8_2 = base_nav.name("QUESTION-NUMBER").index(8).name("RESPONSE-CATEGORY").index(2)
    assert nav_item_8_2.name("ANSWER").value() == Decimal('93'), f'{nav_item_8_2.value()=} != 93'

    # Group-Level (Non-elementary item) response...
    nav_item_1 = base_nav.name("QUESTION-NUMBER").index(1).name("RESPONSE-CATEGORY")
    assert nav_item_1.value() == [{'ANSWER': Decimal('21')}, {'ANSWER': Decimal('22')}, {'ANSWER': Decimal('23')}], f"{nav_item_1.value()=} != [{{'ANSWER': '21'}}, {{'ANSWER': '22'}}, {{'ANSWER': '23'}}]"

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 SURVEY-RESPONSES                             0  60 '' ''",
        "  05 QUESTION-NUMBER           OCCURS 10 TIMES   0  60 '' ''",
        "                                                0   6 '' ''",
        "      10 RESPONSE-CATEGORY     OCCURS 3 TIMES   0   6 '' ''",
        "                                                0   2 '' ''",
        "          15 ANSWER            PIC 99           0   2 b'\\xf1\\xf1' Decimal('11')"
    ]


@pytest.fixture
def copy_5_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Redefines Case 1

        * COPY5.COB
        01  REDEFINES-RECORD.
             05  A PICTURE X(6).
             05  B REDEFINES A.
                 10  B-1 PICTURE X(2).
                 10  B-2 PICTURE 9(4).
             05  C PICTURE 99V99.
    """
    copy_5_schema = {
      "title": "REDEFINES-RECORD",
      "$anchor": "REDEFINES-RECORD",
      "cobol": "01 REDEFINES-RECORD",
      "type": "object",
      "properties": {
        "REDEFINES-A": {
          "oneOf": [
            {
              "title": "A",
              "$anchor": "A",
              "cobol": "05 A PICTURE X(6)",
              "type": "string",
              "contentEncoding": "cp037"
            },
            {
              "title": "B",
              "$anchor": "B",
              "cobol": "05 B REDEFINES A",
              "type": "object",
              "properties": {
                "B-1": {
                  "title": "B-1",
                  "$anchor": "B-1",
                  "cobol": "10 B-1 PICTURE X(2)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "B-2": {
                  "title": "B-2",
                  "$anchor": "B-2",
                  "cobol": "10 B-2 PICTURE 9(4)",
                  "type": "string",
                  "contentEncoding": "cp037"
                }
              }
            }
          ],
          "$anchor": "REDEFINES-A"
        },
        "A": {
          "title": "A",
          "cobol": "05 A PICTURE X(6)",
          "$ref": "#A"
        },
        "B": {
          "title": "B",
          "cobol": "05 B REDEFINES A",
          "$ref": "#B"
        },
        "C": {
          "title": "C",
          "$anchor": "C",
          "cobol": "05 C PICTURE 99V99",
          "type": "string",
          "contentEncoding": "cp037",
          "conversion": "decimal"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_5_schema) is None

    copy_5_data = BytesInstance("AB12345678".encode("CP037"))
    return copy_5_schema, copy_5_data

def test_copy_5(copy_5_schema_data, capsys) -> bool:
    test_5_schema, test_5_data = copy_5_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_5_schema), test_5_data)
    assert base_nav.name("A").value() == "AB1234"
    assert base_nav.name("B").value() == {'B-1': 'AB', 'B-2': Decimal('1234')}
    assert base_nav.name("C").value() == Decimal('56.78')

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 REDEFINES-RECORD                             0  10 '' ''",
        "                                                0   6 '' ''",
        "    05 A PICTURE X(6)                           0   6 b'\\xc1\\xc2\\xf1\\xf2\\xf3\\xf4' 'AB1234'",
        "    05 B REDEFINES A                            0   6 '' ''",
        "      10 B-1 PICTURE X(2)                       0   2 b'\\xc1\\xc2' 'AB'",
        "      10 B-2 PICTURE 9(4)                       2   4 b'\\xf1\\xf2\\xf3\\xf4' Decimal('1234')",
        "  05 C PICTURE 99V99                            6   4 b'\\xf5\\xf6\\xf7\\xf8' Decimal('56.78')"
    ]

@pytest.fixture
def copy_6_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Redefines Case 2

          * COPY6.COB
           01  REDEFINES-RECORD.
               05 NAME-2.
                  10 SALARY PICTURE XXX.
                  10 SO-SEC-NO PICTURE X(9).
                  10 MONTH PICTURE XX.
               05 NAME-1 REDEFINES NAME-2.
                  10 WAGE PICTURE 999V999.
                  10 EMP-NO PICTURE X(6).
                  10 YEAR PICTURE XX.


    """
    copy_6_schema = {
      "title": "REDEFINES-RECORD",
      "$anchor": "REDEFINES-RECORD",
      "cobol": "01 REDEFINES-RECORD",
      "type": "object",
      "properties": {
        "REDEFINES-NAME-2": {
          "oneOf": [
            {
              "title": "NAME-2",
              "$anchor": "NAME-2",
              "cobol": "05 NAME-2",
              "type": "object",
              "properties": {
                "SALARY": {
                  "title": "SALARY",
                  "$anchor": "SALARY",
                  "cobol": "10 SALARY PICTURE XXX",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "SO-SEC-NO": {
                  "title": "SO-SEC-NO",
                  "$anchor": "SO-SEC-NO",
                  "cobol": "10 SO-SEC-NO PICTURE X(9)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "MONTH": {
                  "title": "MONTH",
                  "$anchor": "MONTH",
                  "cobol": "10 MONTH PICTURE XX",
                  "type": "string",
                  "contentEncoding": "cp037"
                }
              },
              "anchor": "REDEFINES-NAME-2.NAME-2"
            },
            {
              "title": "NAME-1",
              "$anchor": "NAME-1",
              "cobol": "05 NAME-1 REDEFINES NAME-2",
              "type": "object",
              "properties": {
                "WAGE": {
                  "title": "WAGE",
                  "$anchor": "WAGE",
                  "cobol": "10 WAGE PICTURE 999V999",
                  "type": "string",
                  "contentEncoding": "cp037",
                  "conversion": "decimal"
                },
                "EMP-NO": {
                  "title": "EMP-NO",
                  "$anchor": "EMP-NO",
                  "cobol": "10 EMP-NO PICTURE X(6)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "YEAR": {
                  "title": "YEAR",
                  "$anchor": "YEAR",
                  "cobol": "10 YEAR PICTURE XX",
                  "type": "string",
                  "contentEncoding": "cp037"
                }
              },
              "anchor": "REDEFINES-NAME-2.NAME-1"
            }
          ],
          "$anchor": "REDEFINES-NAME-2"
        },
        "NAME-2": {
          "title": "NAME-2",
          "cobol": "05 NAME-2",
          "$ref": "#NAME-2"
        },
        "NAME-1": {
          "title": "NAME-1",
          "cobol": "05 NAME-1 REDEFINES NAME-2",
          "$ref": "#NAME-1"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_6_schema) is None

    copy_6_data = BytesInstance("ABC123456789DE".encode("CP037"))
    return copy_6_schema, copy_6_data

def test_copy_6(copy_6_schema_data, capsys) -> bool:
    test_6_schema, test_6_data = copy_6_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_6_schema), test_6_data)
    assert base_nav.name("NAME-2").name("SALARY").value() == 'ABC'
    assert base_nav.name("NAME-2").name("SO-SEC-NO").value() == '123456789'
    assert base_nav.name("NAME-2").name("MONTH").value() == 'DE'

    assert base_nav.name("NAME-1").name("WAGE").value() == Decimal('123.123')
    assert base_nav.name("NAME-1").name("EMP-NO").value() == '456789'
    assert base_nav.name("NAME-1").name("YEAR").value() == 'DE'

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 REDEFINES-RECORD                             0  14 '' ''",
        "                                                0  14 '' ''",
        "    05 NAME-2                                   0  14 '' ''",
        "      10 SALARY PICTURE XXX                     0   3 b'\\xc1\\xc2\\xc3' 'ABC'",
        "      10 SO-SEC-NO PICTURE X(9)                 3   9 b'\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6\\xf7\\xf8\\xf9' '123456789'",
        "      10 MONTH PICTURE XX                      12   2 b'\\xc4\\xc5' 'DE'",
        "    05 NAME-1 REDEFINES NAME-2                  0  14 '' ''",
        "      10 WAGE PICTURE 999V999                   0   6 b'\\xc1\\xc2\\xc3\\xf1\\xf2\\xf3' Decimal('123.123')",
        "      10 EMP-NO PICTURE X(6)                    6   6 b'\\xf4\\xf5\\xf6\\xf7\\xf8\\xf9' '456789'",
        "      10 YEAR PICTURE XX                       12   2 b'\\xc4\\xc5' 'DE'"
    ]

@pytest.fixture
def copy_7_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Redefines Case 3

          * COPY7.COB
           01  REDEFINES-RECORD.
               05 REGULAR-EMPLOYEE.
                  10 LOCATION PICTURE A(8).
                  10 GRADE PICTURE X(4).
                  10 SEMI-MONTHLY-PAY PICTURE 9999V99.
                  10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY
                      PICTURE 999V999.
               05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
                  10 LOCATION PICTURE A(8).
                  10 FILLER PICTURE X(6).
                  10 HOURLY-PAY PICTURE 99V99.
    """
    copy_7_schema = {
      "title": "REDEFINES-RECORD",
      "$anchor": "REDEFINES-RECORD",
      "cobol": "01 REDEFINES-RECORD",
      "type": "object",
      "properties": {
        "REDEFINES-REGULAR-EMPLOYEE": {
          "oneOf": [
            {
              "title": "REGULAR-EMPLOYEE",
              "$anchor": "REGULAR-EMPLOYEE",
              "cobol": "05 REGULAR-EMPLOYEE",
              "type": "object",
              "properties": {
                "REDEFINES-SEMI-MONTHLY-PAY": {
                  "oneOf": [
                    {
                      "title": "SEMI-MONTHLY-PAY",
                      "$anchor": "SEMI-MONTHLY-PAY",
                      "cobol": "10 SEMI-MONTHLY-PAY PICTURE 9999V99",
                      "type": "string",
                      "contentEncoding": "cp037",
                      "conversion": "decimal",
                      "anchor": "REDEFINES-SEMI-MONTHLY-PAY.SEMI-MONTHLY-PAY"
                    },
                    {
                      "title": "WEEKLY-PAY",
                      "$anchor": "WEEKLY-PAY",
                      "cobol": "10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY\n           PICTURE 999V999",
                      "type": "string",
                      "contentEncoding": "cp037",
                      "conversion": "decimal",
                      "anchor": "REDEFINES-SEMI-MONTHLY-PAY.WEEKLY-PAY"
                    }
                  ],
                  "$anchor": "REDEFINES-SEMI-MONTHLY-PAY"
                },
                "LOCATION": {
                  "title": "LOCATION",
                  "$anchor": "LOCATION",
                  "cobol": "10 LOCATION PICTURE A(8)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "GRADE": {
                  "title": "GRADE",
                  "$anchor": "GRADE",
                  "cobol": "10 GRADE PICTURE X(4)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "SEMI-MONTHLY-PAY": {
                  "title": "SEMI-MONTHLY-PAY",
                  "cobol": "10 SEMI-MONTHLY-PAY PICTURE 9999V99",
                  "$ref": "#SEMI-MONTHLY-PAY"
                },
                "WEEKLY-PAY": {
                  "title": "WEEKLY-PAY",
                  "cobol": "10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY\n           PICTURE 999V999",
                  "$ref": "#WEEKLY-PAY"
                }
              },
              "anchor": "REDEFINES-REGULAR-EMPLOYEE.REGULAR-EMPLOYEE"
            },
            {
              "title": "TEMPORARY-EMPLOYEE",
              "$anchor": "TEMPORARY-EMPLOYEE",
              "cobol": "05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE",
              "type": "object",
              "properties": {
                "LOCATION": {
                  "title": "LOCATION",
                  "$anchor": "LOCATION",
                  "cobol": "10 LOCATION PICTURE A(8)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "FILLER-1": {
                  "title": "FILLER",
                  "$anchor": "FILLER-1",
                  "cobol": "10 FILLER PICTURE X(6)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "HOURLY-PAY": {
                  "title": "HOURLY-PAY",
                  "$anchor": "HOURLY-PAY",
                  "cobol": "10 HOURLY-PAY PICTURE 99V99",
                  "type": "string",
                  "contentEncoding": "cp037",
                  "conversion": "decimal"
                }
              },
              "anchor": "REDEFINES-REGULAR-EMPLOYEE.TEMPORARY-EMPLOYEE"
            }
          ],
          "$anchor": "REDEFINES-REGULAR-EMPLOYEE"
        },
        "REGULAR-EMPLOYEE": {
          "title": "REGULAR-EMPLOYEE",
          "cobol": "05 REGULAR-EMPLOYEE",
          "$ref": "#REGULAR-EMPLOYEE"
        },
        "TEMPORARY-EMPLOYEE": {
          "title": "TEMPORARY-EMPLOYEE",
          "cobol": "05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE",
          "$ref": "#TEMPORARY-EMPLOYEE"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_7_schema) is None

    copy_7_data = [
        BytesInstance("ABCDEFGHijkl123456".encode("CP037")),
        BytesInstance("ABCDEFGHijklmn1234".encode("CP037")),
    ]
    return copy_7_schema, copy_7_data

def test_copy_7(copy_7_schema_data, capsys) -> bool:
    test_7_schema, test_7_data = copy_7_schema_data
    base_nav = [EBCDIC().nav(SchemaMaker.from_json(test_7_schema), instance) for instance in test_7_data]
    assert base_nav[0].name("REGULAR-EMPLOYEE").name("SEMI-MONTHLY-PAY").value() == Decimal('1234.56')
    base_nav[0].dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 REDEFINES-RECORD                             0  18 '' ''",
        "                                                0  18 '' ''",
        "    05 REGULAR-EMPLOYEE                         0  18 '' ''",
        "                                                0   6 '' ''",
        "        10 SEMI-MONTHLY-PAY PICTURE 9999V99     0   6 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6' Decimal('1234.56')",
        '        10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY',
        "           PICTURE 999V999   0   6 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6' Decimal('123.456')",
        "      10 LOCATION PICTURE A(8)                  6   8 b'\\xc7\\xc8\\x89\\x91\\x92\\x93\\xf1\\xf2' 'GHijkl12'",
        "      10 GRADE PICTURE X(4)                    14   4 b'\\xf3\\xf4\\xf5\\xf6' '3456'",
        "    05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE   0  18 '' ''",
        "      10 LOCATION PICTURE A(8)                  0   8 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8' 'ABCDEFGH'",
        "      10 FILLER PICTURE X(6)                    8   6 b'\\x89\\x91\\x92\\x93\\xf1\\xf2' 'ijkl12'",
        "      10 HOURLY-PAY PICTURE 99V99              14   4 b'\\xf3\\xf4\\xf5\\xf6' Decimal('34.56')",
    ]

    assert base_nav[1].name("TEMPORARY-EMPLOYEE").name("HOURLY-PAY").value() == Decimal('12.34')
    base_nav[1].dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
            'Field                                         Off Sz  Raw Value',
            "01 REDEFINES-RECORD                             0  18 '' ''",
            "                                                0  18 '' ''",
            "    05 REGULAR-EMPLOYEE                         0  18 '' ''",
            "                                                0   6 '' ''",
            "        10 SEMI-MONTHLY-PAY PICTURE 9999V99     0   6 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6' Decimal('1234.56')",
            '        10 WEEKLY-PAY REDEFINES SEMI-MONTHLY-PAY',
            "           PICTURE 999V999   0   6 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6' Decimal('123.456')",
            "      10 LOCATION PICTURE A(8)                  6   8 b'\\xc7\\xc8\\x89\\x91\\x92\\x93\\x94\\x95' 'GHijklmn'",
            "      10 GRADE PICTURE X(4)                    14   4 b'\\xf1\\xf2\\xf3\\xf4' '1234'",
            "    05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE   0  18 '' ''",
            "      10 LOCATION PICTURE A(8)                  0   8 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8' 'ABCDEFGH'",
            "      10 FILLER PICTURE X(6)                    8   6 b'\\x89\\x91\\x92\\x93\\x94\\x95' 'ijklmn'",
            "      10 HOURLY-PAY PICTURE 99V99              14   4 b'\\xf1\\xf2\\xf3\\xf4' Decimal('12.34')"
        ]


@pytest.fixture
def copy_8_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Redefines Case 4

          * COPY8.COB
           01  REDEFINES-RECORD.
               05 REGULAR-EMPLOYEE.
                   10 LOCATION PICTURE A(8).
                   10 GRADE PICTURE X(4).
                   10 SEMI-MONTHLY-PAY PICTURE 999V999.
               05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE.
                   10 LOCATION PICTURE A(8).
                   10 FILLER PICTURE X(6).
                   10 HOURLY-PAY PICTURE 99V99.
                   10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999.

    """
    copy_8_schema = {
      "title": "REDEFINES-RECORD",
      "$anchor": "REDEFINES-RECORD",
      "cobol": "01 REDEFINES-RECORD",
      "type": "object",
      "properties": {
        "REDEFINES-REGULAR-EMPLOYEE": {
          "oneOf": [
            {
              "title": "REGULAR-EMPLOYEE",
              "$anchor": "REGULAR-EMPLOYEE",
              "cobol": "05 REGULAR-EMPLOYEE",
              "type": "object",
              "properties": {
                "LOCATION": {
                  "title": "LOCATION",
                  "$anchor": "LOCATION",
                  "cobol": "10 LOCATION PICTURE A(8)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "GRADE": {
                  "title": "GRADE",
                  "$anchor": "GRADE",
                  "cobol": "10 GRADE PICTURE X(4)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "SEMI-MONTHLY-PAY": {
                  "title": "SEMI-MONTHLY-PAY",
                  "$anchor": "SEMI-MONTHLY-PAY",
                  "cobol": "10 SEMI-MONTHLY-PAY PICTURE 999V999",
                  "type": "string",
                  "contentEncoding": "cp037",
                  "conversion": "decimal"
                }
              },
              "anchor": "REDEFINES-REGULAR-EMPLOYEE.REGULAR-EMPLOYEE"
            },
            {
              "title": "TEMPORARY-EMPLOYEE",
              "$anchor": "TEMPORARY-EMPLOYEE",
              "cobol": "05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE",
              "type": "object",
              "properties": {
                "REDEFINES-HOURLY-PAY": {
                  "oneOf": [
                    {
                      "title": "HOURLY-PAY",
                      "$anchor": "HOURLY-PAY",
                      "cobol": "10 HOURLY-PAY PICTURE 99V99",
                      "type": "string",
                      "contentEncoding": "cp037",
                      "conversion": "decimal",
                      "anchor": "REDEFINES-HOURLY-PAY.HOURLY-PAY"
                    },
                    {
                      "title": "CODE-H",
                      "$anchor": "CODE-H",
                      "cobol": "10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999",
                      "type": "string",
                      "contentEncoding": "cp037",
                      "conversion": "decimal",
                      "anchor": "REDEFINES-HOURLY-PAY.CODE-H"
                    }
                  ],
                  "$anchor": "REDEFINES-HOURLY-PAY"
                },
                "LOCATION": {
                  "title": "LOCATION",
                  "$anchor": "LOCATION",
                  "cobol": "10 LOCATION PICTURE A(8)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "FILLER-1": {
                  "title": "FILLER",
                  "$anchor": "FILLER-1",
                  "cobol": "10 FILLER PICTURE X(6)",
                  "type": "string",
                  "contentEncoding": "cp037"
                },
                "HOURLY-PAY": {
                  "title": "HOURLY-PAY",
                  "cobol": "10 HOURLY-PAY PICTURE 99V99",
                  "$ref": "#HOURLY-PAY"
                },
                "CODE-H": {
                  "title": "CODE-H",
                  "cobol": "10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999",
                  "$ref": "#CODE-H"
                }
              },
              "anchor": "REDEFINES-REGULAR-EMPLOYEE.TEMPORARY-EMPLOYEE"
            }
          ],
          "$anchor": "REDEFINES-REGULAR-EMPLOYEE"
        },
        "REGULAR-EMPLOYEE": {
          "title": "REGULAR-EMPLOYEE",
          "cobol": "05 REGULAR-EMPLOYEE",
          "$ref": "#REGULAR-EMPLOYEE"
        },
        "TEMPORARY-EMPLOYEE": {
          "title": "TEMPORARY-EMPLOYEE",
          "cobol": "05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE",
          "$ref": "#TEMPORARY-EMPLOYEE"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_8_schema) is None

    copy_8_data = [
        BytesInstance("ABCDEFGHijkl123456".encode("CP037")),
        BytesInstance("ABCDEFGHijklmn1234".encode("CP037")),
    ]
    return copy_8_schema, copy_8_data

def test_copy_8(copy_8_schema_data, capsys) -> bool:
    test_8_schema, test_8_data = copy_8_schema_data
    base_nav = [EBCDIC().nav(SchemaMaker.from_json(test_8_schema), instance) for instance in test_8_data]
    # REDEFINES-RECORD.REGULAR-EMPLOYEE.SEMI-MONTHLY-PAY
    assert base_nav[0].name("REGULAR-EMPLOYEE").name("SEMI-MONTHLY-PAY").value() == Decimal('123.456')
    base_nav[0].dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 REDEFINES-RECORD                             0  18 '' ''",
        "                                                0  18 '' ''",
        "    05 REGULAR-EMPLOYEE                         0  18 '' ''",
        "      10 LOCATION PICTURE A(8)                  0   8 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8' 'ABCDEFGH'",
        "      10 GRADE PICTURE X(4)                     8   4 b'\\x89\\x91\\x92\\x93' 'ijkl'",
        "      10 SEMI-MONTHLY-PAY PICTURE 999V999      12   6 b'\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6' Decimal('123.456')",
        "    05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE   0  18 '' ''",
        "                                                0   4 '' ''",
        "        10 HOURLY-PAY PICTURE 99V99             0   4 b'\\xc1\\xc2\\xc3\\xc4' Decimal('12.34')",
        "        10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999   0   4 b'\\xc1\\xc2\\xc3\\xc4' Decimal('1234')",
        "      10 LOCATION PICTURE A(8)                  4   8 b'\\xc5\\xc6\\xc7\\xc8\\x89\\x91\\x92\\x93' 'EFGHijkl'",
        "      10 FILLER PICTURE X(6)                   12   6 b'\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6' '123456'",
    ]

    # REDEFINES-RECORD.TEMPORARY-EMPLOYEE.HOURLY-PAY
    assert base_nav[1].name("TEMPORARY-EMPLOYEE").name("HOURLY-PAY").value() == Decimal('12.34')
    assert base_nav[1].name("TEMPORARY-EMPLOYEE").name("CODE-H").value() == Decimal('1234')
    base_nav[1].dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
         'Field                                         Off Sz  Raw Value',
         "01 REDEFINES-RECORD                             0  18 '' ''",
         "                                                0  18 '' ''",
         "    05 REGULAR-EMPLOYEE                         0  18 '' ''",
         "      10 LOCATION PICTURE A(8)                  0   8 b'\\xc1\\xc2\\xc3\\xc4\\xc5\\xc6\\xc7\\xc8' 'ABCDEFGH'",
         "      10 GRADE PICTURE X(4)                     8   4 b'\\x89\\x91\\x92\\x93' 'ijkl'",
         "      10 SEMI-MONTHLY-PAY PICTURE 999V999      12   6 b'\\x94\\x95\\xf1\\xf2\\xf3\\xf4' Decimal('451.234')",
         "    05 TEMPORARY-EMPLOYEE REDEFINES REGULAR-EMPLOYEE   0  18 '' ''",
         "                                                0   4 '' ''",
         "        10 HOURLY-PAY PICTURE 99V99             0   4 b'\\xc1\\xc2\\xc3\\xc4' Decimal('12.34')",
         "        10 CODE-H REDEFINES HOURLY-PAY PICTURE 9999   0   4 b'\\xc1\\xc2\\xc3\\xc4' Decimal('1234')",
         "      10 LOCATION PICTURE A(8)                  4   8 b'\\xc5\\xc6\\xc7\\xc8\\x89\\x91\\x92\\x93' 'EFGHijkl'",
         "      10 FILLER PICTURE X(6)                   12   6 b'\\x94\\x95\\xf1\\xf2\\xf3\\xf4' 'mn1234'"
    ]

@pytest.fixture
def copy_9_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Redefines Case 5

    Valid. But kind of silly; the `REDEFINES` is irrelevant.

             01  DETAIL-LINE.
                 05  QUESTION                    PIC ZZ.
                 05  PRINT-YES                   PIC ZZ.
                 05  PRINT-NO                    PIC ZZ.
                 05  NOT-SURE                    PIC ZZ.
             01  SUMMARY-LINE REDEFINES DETAIL-LINE.
                 05  COUNT                       PIC ZZ.
                 05  FILLER                      PIC XX.
                 05  FILLER                      PIC XX.
                 05  FILLER                      PIC XX.

    """
    copy_9_schema_1 = {
      "title": "DETAIL-LINE",
      "$anchor": "DETAIL-LINE",
      "cobol": "01 DETAIL-LINE",
      "type": "object",
      "properties": {
        "QUESTION": {
          "title": "QUESTION",
          "$anchor": "QUESTION",
          "cobol": "05 QUESTION                    PIC ZZ",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "PRINT-YES": {
          "title": "PRINT-YES",
          "$anchor": "PRINT-YES",
          "cobol": "05 PRINT-YES                   PIC ZZ",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "PRINT-NO": {
          "title": "PRINT-NO",
          "$anchor": "PRINT-NO",
          "cobol": "05 PRINT-NO                    PIC ZZ",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "NOT-SURE": {
          "title": "NOT-SURE",
          "$anchor": "NOT-SURE",
          "cobol": "05 NOT-SURE                    PIC ZZ",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }

    copy_9_schema_2 = {
      "title": "SUMMARY-LINE",
      "$anchor": "SUMMARY-LINE",
      "cobol": "01 SUMMARY-LINE REDEFINES DETAIL-LINE",
      "type": "object",
      "properties": {
        "COUNT": {
          "title": "COUNT",
          "$anchor": "COUNT",
          "cobol": "05 COUNT                       PIC ZZ",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "FILLER-1": {
          "title": "FILLER",
          "$anchor": "FILLER-1",
          "cobol": "05 FILLER                      PIC XX",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "FILLER-2": {
          "title": "FILLER",
          "$anchor": "FILLER-2",
          "cobol": "05 FILLER                      PIC XX",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "FILLER-3": {
          "title": "FILLER",
          "$anchor": "FILLER-3",
          "cobol": "05 FILLER                      PIC XX",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_9_schema_1) is None
    assert SchemaValidator.check_schema(copy_9_schema_2) is None

    copy_9_data = BytesInstance("01020304".encode("CP037"))
    return [copy_9_schema_1, copy_9_schema_2], copy_9_data

def test_copy_9(copy_9_schema_data, capsys) -> bool:
    test_9_schema, test_9_data = copy_9_schema_data
    test_9_schema_1, test_9_schema_2 = test_9_schema
    base_nav_1 = EBCDIC().nav(SchemaMaker.from_json(test_9_schema_1), test_9_data)

    assert base_nav_1.name("QUESTION").value() == '01'
    assert base_nav_1.name("PRINT-YES").value() == '02'
    assert base_nav_1.name("PRINT-NO").value() == '03'
    assert base_nav_1.name("NOT-SURE").value() == '04'

    base_nav_2 = EBCDIC().nav(SchemaMaker.from_json(test_9_schema_2), test_9_data)

    assert base_nav_2.name("COUNT").value() == '01'

    base_nav_2.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 SUMMARY-LINE REDEFINES DETAIL-LINE           0   8 '' ''",
        "  05 COUNT                       PIC ZZ         0   2 b'\\xf0\\xf1' '01'",
        "  05 FILLER                      PIC XX         2   2 b'\\xf0\\xf2' '02'",
        "  05 FILLER                      PIC XX         4   2 b'\\xf0\\xf3' '03'",
        "  05 FILLER                      PIC XX         6   2 b'\\xf0\\xf4' '04'"
    ]

@pytest.fixture
def copy_10_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Occurs Depending On Case 1

          * COPY10.COB
           01  MAIN-AREA.
               03 REC-1.
                  05 FIELD-1                       PIC 9.
                  05 FIELD-2 OCCURS 1 TO 5 TIMES
                     DEPENDING ON FIELD-1        PIC X(05).
    """
    copy_10_schema = {
      "title": "MAIN-AREA",
      "$anchor": "MAIN-AREA",
      "cobol": "01 MAIN-AREA",
      "type": "object",
      "properties": {
        "REC-1": {
          "title": "REC-1",
          "$anchor": "REC-1",
          "cobol": "03 REC-1",
          "type": "object",
          "properties": {
            "FIELD-1": {
              "title": "FIELD-1",
              "$anchor": "FIELD-1",
              "cobol": "05 FIELD-1                       PIC 9",
              "type": "string",
              "contentEncoding": "cp037",
              "conversion": "decimal"
            },
            "FIELD-2": {
              "title": "FIELD-2",
              "$anchor": "FIELD-2",
              "cobol": "05 FIELD-2 OCCURS 1 TO 5 TIMES\n          DEPENDING ON FIELD-1        PIC X(05)",
              "type": "array",
              "items": {
                "type": "object",
                "properties": {
                  "FIELD-2": {
                    "cobol": "05 FIELD-2 OCCURS 1 TO 5 TIMES\n          DEPENDING ON FIELD-1        PIC X(05)",
                    "type": "string",
                    "contentEncoding": "cp037"
                  }
                }
              },
              "maxItemsDependsOn": {
                "$ref": "#FIELD-1"
              }
            }
          }
        }
      }
    }

    assert SchemaValidator.check_schema(copy_10_schema) is None

    copy_10_data = BytesInstance("3111112222233333".encode("CP037"))
    return copy_10_schema, copy_10_data

def test_copy_10(copy_10_schema_data, capsys) -> bool:
    test_10_schema, test_10_data = copy_10_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_10_schema), test_10_data)

    assert base_nav.name("REC-1").name("FIELD-1").value() == Decimal('3')
    assert base_nav.name("REC-1").name("FIELD-2").index(0).value() == {'FIELD-2': '11111'}
    assert base_nav.name("REC-1").name("FIELD-2").index(1).value() == {'FIELD-2': '22222'}
    assert base_nav.name("REC-1").name("FIELD-2").index(2).value() == {'FIELD-2': '33333'}
    with pytest.raises(IndexError):
        base_nav.name("REC-1").name("FIELD-2").index(3).value()

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 MAIN-AREA                                    0  16 '' ''",
        "  03 REC-1                                      0  16 '' ''",
        "    05 FIELD-1                       PIC 9      0   1 b'\\xf3' Decimal('3')",
        '    05 FIELD-2 OCCURS 1 TO 5 TIMES',
        "          DEPENDING ON FIELD-1        PIC X(05)   1  15 '' ''",
        "                                                1   5 '' ''",
        '        05 FIELD-2 OCCURS 1 TO 5 TIMES',
        "          DEPENDING ON FIELD-1        PIC X(05)   1   5 b'\\xf1\\xf1\\xf1\\xf1\\xf1' '11111'"
    ]

@pytest.fixture
def copy_11_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Occurs Depending on case 2

          * COPY11.COB
           01  MAIN-AREA.
               03 REC-1.
                  05 FIELD-1                       PIC 9.
                  05 FIELD-3                       PIC 9.
                  05 FIELD-2 OCCURS 1 TO 5 TIMES
                       DEPENDING ON FIELD-1        PIC X(05).
               03 REC-2.
                  05 FIELD-4 OCCURS 1 TO 5 TIMES
                       DEPENDING ON FIELD-3        PIC X(05).
    """
    copy_11_schema = {
      "title": "MAIN-AREA",
      "$anchor": "MAIN-AREA",
      "cobol": "01 MAIN-AREA",
      "type": "object",
      "properties": {
        "REC-1": {
          "title": "REC-1",
          "$anchor": "REC-1",
          "cobol": "03 REC-1",
          "type": "object",
          "properties": {
            "FIELD-1": {
              "title": "FIELD-1",
              "$anchor": "FIELD-1",
              "cobol": "05 FIELD-1                       PIC 9",
              "type": "string",
              "contentEncoding": "cp037",
              "conversion": "decimal"
            },
            "FIELD-3": {
              "title": "FIELD-3",
              "$anchor": "FIELD-3",
              "cobol": "05 FIELD-3                       PIC 9",
              "type": "string",
              "contentEncoding": "cp037",
              "conversion": "decimal"
            },
            "FIELD-2": {
              "title": "FIELD-2",
              "$anchor": "FIELD-2",
              "cobol": "05 FIELD-2 OCCURS 1 TO 5 TIMES\n            DEPENDING ON FIELD-1        PIC X(05)",
              "type": "array",
              "items": {
                "type": "object",
                "properties": {
                  "FIELD-2": {
                    "cobol": "05 FIELD-2 OCCURS 1 TO 5 TIMES\n            DEPENDING ON FIELD-1        PIC X(05)",
                    "type": "string",
                    "contentEncoding": "cp037"
                  }
                }
              },
              "maxItemsDependsOn": {
                "$ref": "#FIELD-1"
              }
            }
          }
        },
        "REC-2": {
          "title": "REC-2",
          "$anchor": "REC-2",
          "cobol": "03 REC-2",
          "type": "object",
          "properties": {
            "FIELD-4": {
              "title": "FIELD-4",
              "$anchor": "FIELD-4",
              "cobol": "05 FIELD-4 OCCURS 1 TO 5 TIMES\n            DEPENDING ON FIELD-3        PIC X(05)",
              "type": "array",
              "items": {
                "type": "object",
                "properties": {
                  "FIELD-4": {
                    "cobol": "05 FIELD-4 OCCURS 1 TO 5 TIMES\n            DEPENDING ON FIELD-3        PIC X(05)",
                    "type": "string",
                    "contentEncoding": "cp037"
                  }
                }
              },
              "maxItemsDependsOn": {
                "$ref": "#FIELD-3"
              }
            }
          }
        }
      }
    }
    assert SchemaValidator.check_schema(copy_11_schema) is None

    copy_11_data = BytesInstance("321111122222333334444455555".encode("CP037"))
    return copy_11_schema, copy_11_data

def test_copy_11(copy_11_schema_data, capsys) -> bool:
    test_11_schema, test_11_data = copy_11_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_11_schema), test_11_data)

    assert base_nav.name("REC-1").name("FIELD-1").value() == 3
    assert base_nav.name("REC-1").name("FIELD-3").value() == 2
    assert base_nav.name("REC-1").name("FIELD-2").index(0).value() == {'FIELD-2': '11111'}
    assert base_nav.name("REC-1").name("FIELD-2").index(1).value() == {'FIELD-2': "22222"}
    assert base_nav.name("REC-1").name("FIELD-2").index(2).value() == {'FIELD-2': "33333"}
    assert base_nav.name("REC-2").name("FIELD-4").index(0).value() == {'FIELD-4': "44444"}
    assert base_nav.name("REC-2").name("FIELD-4").index(1).value() == {'FIELD-4': "55555"}

    base_nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 MAIN-AREA                                    0  27 '' ''",
        "  03 REC-1                                      0  17 '' ''",
        "    05 FIELD-1                       PIC 9      0   1 b'\\xf3' Decimal('3')",
        "    05 FIELD-3                       PIC 9      1   1 b'\\xf2' Decimal('2')",
        '    05 FIELD-2 OCCURS 1 TO 5 TIMES',
        "            DEPENDING ON FIELD-1        PIC X(05)   2  15 '' ''",
        "                                                2   5 '' ''",
        '        05 FIELD-2 OCCURS 1 TO 5 TIMES',
        "            DEPENDING ON FIELD-1        PIC X(05)   2   5 b'\\xf1\\xf1\\xf1\\xf1\\xf1' '11111'",
        "  03 REC-2                                     17  10 '' ''",
        '    05 FIELD-4 OCCURS 1 TO 5 TIMES',
        "            DEPENDING ON FIELD-3        PIC X(05)  17  10 '' ''",
        "                                               17   5 '' ''",
        '        05 FIELD-4 OCCURS 1 TO 5 TIMES',
        "            DEPENDING ON FIELD-3        PIC X(05)  17   5 b'\\xf4\\xf4\\xf4\\xf4\\xf4' '44444'"
    ]


# Multiple 01-levels -- case 1
#
# Similar to COPY09.COB without the REDEFINES.


@pytest.fixture
def copy_13_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Multiple 01-levels -- case 2

          * COPY13.COB
           01  GENERIC-RECORD.
               05 HEADER PIC X(3).
               05 GENERIC-FIELD PIC X(17).

            01 ABC-SPECIFIC-RECORD.
               05 ITEM-1 PIC X(10).
               05 ITEM-2 PIC X(7).

            01 DEF-ANOTHER-RECORD.
               05 ITEM-3 PIC X(7).
               05 ITEM-4 PIC X(10).
    """
    copy_13_schema_1 = {
      "title": "GENERIC-RECORD",
      "$anchor": "GENERIC-RECORD",
      "cobol": "01 GENERIC-RECORD",
      "type": "object",
      "properties": {
        "HEADER": {
          "title": "HEADER",
          "$anchor": "HEADER",
          "cobol": "05 HEADER PIC X(3)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "GENERIC-FIELD": {
          "title": "GENERIC-FIELD",
          "$anchor": "GENERIC-FIELD",
          "cobol": "05 GENERIC-FIELD PIC X(17)",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }

    copy_13_schema_2 = {
      "title": "ABC-SPECIFIC-RECORD",
      "$anchor": "ABC-SPECIFIC-RECORD",
      "cobol": "01 ABC-SPECIFIC-RECORD",
      "type": "object",
      "properties": {
        "ITEM-1": {
          "title": "ITEM-1",
          "$anchor": "ITEM-1",
          "cobol": "05 ITEM-1 PIC X(10)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "ITEM-2": {
          "title": "ITEM-2",
          "$anchor": "ITEM-2",
          "cobol": "05 ITEM-2 PIC X(7)",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }

    copy_13_schema_3 = {
      "title": "DEF-ANOTHER-RECORD",
      "$anchor": "DEF-ANOTHER-RECORD",
      "cobol": "01 DEF-ANOTHER-RECORD",
      "type": "object",
      "properties": {
        "ITEM-3": {
          "title": "ITEM-3",
          "$anchor": "ITEM-3",
          "cobol": "05 ITEM-3 PIC X(7)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "ITEM-4": {
          "title": "ITEM-4",
          "$anchor": "ITEM-4",
          "cobol": "05 ITEM-4 PIC X(10)",
          "type": "string",
          "contentEncoding": "cp037"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_13_schema_1) is None
    assert SchemaValidator.check_schema(copy_13_schema_2) is None
    assert SchemaValidator.check_schema(copy_13_schema_3) is None

    copy_13_data_1 = BytesInstance("ABC0123456789TUVWXYZDEFG".encode("CP037"))
    copy_13_data_2 = BytesInstance("DEF0123456QRSTUVWXYZ".encode("CP037"))
    return [copy_13_schema_1, copy_13_schema_2, copy_13_schema_3], [copy_13_data_1, copy_13_data_2]


def test_copy_13(copy_13_schema_data, capsys) -> bool:
    test_13_schema, test_13_data = copy_13_schema_data
    test_13_schema_1, test_13_schema_2, test_13_schema_3 = test_13_schema

    base_nav_1 = [EBCDIC().nav(SchemaMaker.from_json(test_13_schema_1), instance) for instance in test_13_data]

    assert base_nav_1[0].name("HEADER").value() == "ABC"
    body = base_nav_1[0].name("GENERIC-FIELD").raw_instance()
    body_nav = EBCDIC().nav(SchemaMaker.from_json(test_13_schema_2), body)
    assert body_nav.name("ITEM-1").value() == "0123456789"
    assert body_nav.name("ITEM-2").value() == "TUVWXYZ"
    body_nav.dump()

    assert base_nav_1[1].name("HEADER").value() == "DEF"
    body = base_nav_1[1].name("GENERIC-FIELD").raw_instance()
    body_nav = EBCDIC().nav(SchemaMaker.from_json(test_13_schema_3), body)
    assert body_nav.name("ITEM-3").value() == '0123456'
    assert body_nav.name("ITEM-4").value() == 'QRSTUVWXYZ'
    body_nav.dump()

    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                         Off Sz  Raw Value',
        "01 ABC-SPECIFIC-RECORD                          0  17 '' ''",
        "  05 ITEM-1 PIC X(10)                           0  10 b'\\xf0\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6\\xf7\\xf8\\xf9' '0123456789'",
        "  05 ITEM-2 PIC X(7)                           10   7 b'\\xe3\\xe4\\xe5\\xe6\\xe7\\xe8\\xe9' 'TUVWXYZ'",
        'Field                                         Off Sz  Raw Value',
        "01 DEF-ANOTHER-RECORD                           0  17 '' ''",
        "  05 ITEM-3 PIC X(7)                            0   7 b'\\xf0\\xf1\\xf2\\xf3\\xf4\\xf5\\xf6' '0123456'",
        "  05 ITEM-4 PIC X(10)                           7  10 b'\\xd8\\xd9\\xe2\\xe3\\xe4\\xe5\\xe6\\xe7\\xe8\\xe9' 'QRSTUVWXYZ'"
    ]


@pytest.fixture
def copy_14_schema_data() -> tuple[JSON, BytesInstance]:
    """
    Complex Picture

    This is partially a test of :py:mod:`estruct` as well as instance navigation.

          * COPY14.COB
           01  GENERIC-RECORD.
                  04 NUMBER-1     PIC SV9(5)   COMP-3.
                  04 NUMBER-2     PIC SV9(5)   COMP-3.
                  04 NUMBER-3     PIC SV9(05)  COMP-3.
                  04 NUMBER-4     PIC SV9(5)   COMP-3.

    """
    copy_14_schema = {
      "title": "GENERIC-RECORD",
      "$anchor": "GENERIC-RECORD",
      "cobol": "01 GENERIC-RECORD",
      "type": "object",
      "properties": {
        "NUMBER-1": {
          "title": "NUMBER-1",
          "$anchor": "NUMBER-1",
          "cobol": "04 NUMBER-1     PIC SV9(5)   COMP-3",
          "type": "string",
          "contentEncoding": "packed-decimal",
          "conversion": "decimal"
        },
        "NUMBER-2": {
          "title": "NUMBER-2",
          "$anchor": "NUMBER-2",
          "cobol": "04 NUMBER-2     PIC SV9(5)   COMP-3",
          "type": "string",
          "contentEncoding": "packed-decimal",
          "conversion": "decimal"
        },
        "NUMBER-3": {
          "title": "NUMBER-3",
          "$anchor": "NUMBER-3",
          "cobol": "04 NUMBER-3     PIC SV9(05)  COMP-3",
          "type": "string",
          "contentEncoding": "packed-decimal",
          "conversion": "decimal"
        },
        "NUMBER-4": {
          "title": "NUMBER-4",
          "$anchor": "NUMBER-4",
          "cobol": "04 NUMBER-4     PIC SV9(5)   COMP-3",
          "type": "string",
          "contentEncoding": "packed-decimal",
          "conversion": "decimal"
        }
      }
    }

    assert SchemaValidator.check_schema(copy_14_schema) is None

    buffer14 = (
          b"\x12\x34\x5c" # NUMBER-1=".12345"
          b"\x67\x89\x0c" # NUMBER-2=".67890"
          b"\x00\x12\x0c" # NUMBER-3=".00120"
          b"\x98\x76\x5d" # NUMBER-4="-.98765"
    )
    copy_14_data = BytesInstance(buffer14)
    return copy_14_schema, copy_14_data

def test_copy_14(copy_14_schema_data, capsys) -> bool:
    test_14_schema, test_14_data = copy_14_schema_data
    base_nav = EBCDIC().nav(SchemaMaker.from_json(test_14_schema), test_14_data)

    assert base_nav.name("NUMBER-1").value() == Decimal('0.12345')
    assert base_nav.name("NUMBER-2").value() == Decimal("0.67890")
    assert base_nav.name("NUMBER-3").value() == Decimal("0.00120")
    assert base_nav.name("NUMBER-4").value() == Decimal("-0.98765")


@pytest.fixture
def issue_1_schema() -> JSON:
    """
    Issue 1 -- COBOL Parsing Bug

                 02 GROUP-LABL.
                    03 LABL-STDY-GP-AR-CT           PIC 9(00004).
                    03 LABL-STDY-GP-AR
                       OCCURS 0 TO 40 TIMES DEPENDING ON LABL-STDY-GP-AR-CT.
                       05 LABL-PRSN-ID              PIC 9(00009).
                       05 LABL-GNPR-ID              PIC 9(00009).
                       05 LABL-GNRC-ID              PIC 9(00009).
                       05 LABL-GNRC-AT              PIC -9(00012).9(6).
                       05 LABL-GNRC-QY              PIC -9(00015).
                       05 LABL-GNRC-CD              PIC X(00006).
                       05 LABL-GNRC-PT              PIC -9(00003).9(4).
                       05 LABL-GNRC-TX              PIC X(00030).
                       05 LABL-QRY-FILL-50-TX       PIC X(00050).

    """
    issue_1_schema = {
      "title": "GROUP-LABL",
      "$anchor": "GROUP-LABL",
      "cobol": "02 GROUP-LABL",
      "type": "object",
      "properties": {
        "LABL-STDY-GP-AR-CT": {
          "title": "LABL-STDY-GP-AR-CT",
          "$anchor": "LABL-STDY-GP-AR-CT",
          "cobol": "03 LABL-STDY-GP-AR-CT           PIC 9(00004)",
          "type": "string",
          "contentEncoding": "cp037"
        },
        "LABL-STDY-GP-AR": {
          "title": "LABL-STDY-GP-AR",
          "$anchor": "LABL-STDY-GP-AR",
          "cobol": "03 LABL-STDY-GP-AR\n          OCCURS 0 TO 40 TIMES DEPENDING ON LABL-STDY-GP-AR-CT",
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "LABL-PRSN-ID": {
                "title": "LABL-PRSN-ID",
                "$anchor": "LABL-PRSN-ID",
                "cobol": "05 LABL-PRSN-ID              PIC 9(00009)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNPR-ID": {
                "title": "LABL-GNPR-ID",
                "$anchor": "LABL-GNPR-ID",
                "cobol": "05 LABL-GNPR-ID              PIC 9(00009)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-ID": {
                "title": "LABL-GNRC-ID",
                "$anchor": "LABL-GNRC-ID",
                "cobol": "05 LABL-GNRC-ID              PIC 9(00009)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-AT": {
                "title": "LABL-GNRC-AT",
                "$anchor": "LABL-GNRC-AT",
                "cobol": "05 LABL-GNRC-AT              PIC -9(00012).9(6)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-QY": {
                "title": "LABL-GNRC-QY",
                "$anchor": "LABL-GNRC-QY",
                "cobol": "05 LABL-GNRC-QY              PIC -9(00015)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-CD": {
                "title": "LABL-GNRC-CD",
                "$anchor": "LABL-GNRC-CD",
                "cobol": "05 LABL-GNRC-CD              PIC X(00006)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-PT": {
                "title": "LABL-GNRC-PT",
                "$anchor": "LABL-GNRC-PT",
                "cobol": "05 LABL-GNRC-PT              PIC -9(00003).9(4)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-GNRC-TX": {
                "title": "LABL-GNRC-TX",
                "$anchor": "LABL-GNRC-TX",
                "cobol": "05 LABL-GNRC-TX              PIC X(00030)",
                "type": "string",
                "contentEncoding": "cp037"
              },
              "LABL-QRY-FILL-50-TX": {
                "title": "LABL-QRY-FILL-50-TX",
                "$anchor": "LABL-QRY-FILL-50-TX",
                "cobol": "05 LABL-QRY-FILL-50-TX       PIC X(00050)",
                "type": "string",
                "contentEncoding": "cp037"
              }
            }
          },
          "maxItemsDependsOn": {
            "$ref": "#LABL-STDY-GP-AR-CT"
          }
        }
      }
    }

    assert SchemaValidator.check_schema(issue_1_schema) is None
    return issue_1_schema

def test_issue_1(issue_1_schema) -> bool:
    # TODO: Check the resulting object model.
    # This is awkward to set up because each group level item contains EVERYTHING below it.
    # Some kind of bottom-up construction is required.
    assert SchemaMaker.from_json(issue_1_schema) is not None

### Nav Class Hierarchy

@pytest.fixture
def ndnav_object():
    unpacker = MagicMock(
        name="TextUnpacker"
    )
    loc = MagicMock(
        name="ObjectLocation",
        schema=MagicMock(
            type="object",
        ),
        properties={"ITEM": Mock(referent=Mock(name="ITEM location", schema=sentinel.SUBSCHEMA))},
        start=0,
        end=0,
        size=0,
    )
    instance = MagicMock(
        name="TextInstance",
        spec=TextInstance,
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    ndnav = NDNav(unpacker, loc, instance)
    return unpacker, loc, instance, ndnav

def test_object_ndnav(ndnav_object):
    unpacker, loc, instance, ndnav = ndnav_object
    assert re.fullmatch(
        r"NDNav\(\<MagicMock name='TextUnpacker' id='\d+'\>, "
        r"\<MagicMock name='ObjectLocation' id='\d+'\>, "
        r"\<MagicMock name='TextInstance' spec='TextInstance' id='\d+'\>\)",
        repr(ndnav)
    )
    assert ndnav.schema == loc.schema
    new_nav = ndnav.name("ITEM")
    assert new_nav.unpacker() == unpacker
    assert new_nav.instance == instance
    assert new_nav.schema == sentinel.SUBSCHEMA
    assert ndnav.raw_instance() == str(sentinel.INSTANCE_SLICE)
    with pytest.raises(TypeError):
        ndnav.index(42)

@pytest.fixture
def ndnav_array():
    unpacker = MagicMock(
        name="TextUnpacker"
    )
    subschema = Mock(
        name="Subschema.items",
        spec=AtomicSchema,
        _attributes={"type": "string"}
    )
    loc = MagicMock(
        name="ArrayLocation",
        schema=MagicMock(
            name="Subschema",
            spec=ArraySchema,
            type="array",
            items=subschema
        ),
        items=Mock(referent=Mock(name="ITEM location", schema=Mock(type="string"))),
        item_count=42,
        start=0,
        end=0,
        size=0,
    )
    instance = MagicMock(
        name="TextInstance",
        spec=TextInstance,
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    ndnav = NDNav(unpacker, loc, instance)
    return unpacker, loc, instance, ndnav

def test_array_ndnav(ndnav_array):
    unpacker, loc, instance, ndnav = ndnav_array
    assert re.fullmatch(
        r"NDNav\(\<MagicMock name='TextUnpacker' id='\d+'\>, "
        r"\<MagicMock name='ArrayLocation' id='\d+'\>, "
        r"\<MagicMock name='TextInstance' spec='TextInstance' id='\d+'\>\)",
        repr(ndnav)
    ), f"Bad {ndnav!r}"
    assert ndnav.schema == loc.schema
    new_nav = ndnav.index(1)
    assert new_nav.unpacker() == unpacker
    assert new_nav.instance == instance
    assert new_nav.schema == loc.schema.items
    assert ndnav.raw_instance() == str(sentinel.INSTANCE_SLICE)
    with pytest.raises(TypeError):
        ndnav.name("Nope")

@pytest.fixture
def dnav_object():
    unpacker = MagicMock(
        spec=JSONUnpacker,
        name="JSONUnpacker"
    )
    schema = Mock(
        spec=ObjectSchema,
        name="Schema",
        type="object",
        properties={"ITEM": sentinel.SUBSCHEMA},
        dump_iter=Mock(return_value=iter([(0, Mock(type="mock"), (), "42")]))
    )
    instance = {"ITEM": 42}
    dnav = DNav(unpacker, schema, instance)
    return unpacker, schema, instance, dnav

def test_object_dnav(dnav_object, capsys):
    unpacker, schema, instance, dnav = dnav_object
    assert re.fullmatch(
        r"DNav\(\<MagicMock name='JSONUnpacker' spec='JSONUnpacker' id='\d+'\>, "
        r"\<Mock name='Schema' spec='ObjectSchema' id='\d+'\>, "
        r"\{'ITEM': 42\}\)",
        repr(dnav)
    ), f"Bad {dnav!r}"
    new_nav_name = dnav.name("ITEM")
    assert new_nav_name.unpacker() == unpacker
    assert new_nav_name.instance == 42
    assert new_nav_name.schema == sentinel.SUBSCHEMA
    assert dnav.value() == instance
    dnav.dump()
    out, err = capsys.readouterr()
    assert out == (
        'Field                                                 Value\n'
        "object                                                '42'\n"
    )
    with pytest.raises(TypeError):
        dnav.index(42)


@pytest.fixture
def dnav_array():
    unpacker = MagicMock(
        spec=JSONUnpacker,
        name="JSONUnpacker"
    )
    schema = Mock(
        name="Schema",
        type="array",
        items=sentinel.SUBSCHEMA,
        dump_iter=Mock(return_value=iter([(0, Mock(type="mock"), (), "42")]))
    )
    instance = MagicMock(
        name="ListInstance",
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    dnav = DNav(unpacker, schema, instance)
    return unpacker, schema, instance, dnav

def test_array_dnav(dnav_array, capsys):
    unpacker, schema, instance, dnav = dnav_array
    assert re.fullmatch(
        r"DNav\(\<MagicMock name='JSONUnpacker' spec='JSONUnpacker' id='\d+'\>, "
        r"\<Mock name='Schema' id='\d+'\>, "
        r"\<MagicMock name='ListInstance' id='\d+'\>\)",
        repr(dnav)
    ), f"Bad {dnav!r}"
    new_nav_index = dnav.index(0)
    assert new_nav_index.unpacker() == unpacker
    assert new_nav_index.instance == sentinel.INSTANCE_SLICE
    assert new_nav_index.schema == sentinel.SUBSCHEMA
    dnav.dump()
    out, err = capsys.readouterr()
    assert out == (
        'Field                                                 Value\n'
        "array                                                 '42'\n"
    )
    with pytest.raises(TypeError):
        dnav.name("Nope")

@pytest.fixture
def wbnav_object():
    unpacker = MagicMock(
        name="WBUnpacker"
    )
    schema = Mock(
        name="Schema",
        type="object",
        properties={"ITEM": Mock(name="Subschema", attributes={})},
        attributes={}
    )
    instance = MagicMock(
        name="WBRow",
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    wbnav = WBNav(unpacker, schema, instance)
    return unpacker, schema, instance, wbnav

def test_object_wbnav(wbnav_object):
    unpacker, schema, instance, wbnav = wbnav_object
    assert re.fullmatch(
        r"WBNav\(\<MagicMock name='WBUnpacker' id='\d+'\>, "
        r"\<Mock name='Schema' id='\d+'\>, "
        r"\<MagicMock name='WBRow' id='\d+'\>\)",
        repr(wbnav)
    ), f"Bad {wbnav!r}"
    new_nav_name = wbnav.name("ITEM")
    assert new_nav_name.unpacker() == unpacker
    assert new_nav_name.instance == sentinel.INSTANCE_SLICE
    assert new_nav_name.schema == schema.properties["ITEM"]
    assert wbnav.value() == instance
    with pytest.raises(TypeError):
        wbnav.index(0)


@pytest.fixture
def wbnav_array():
    unpacker = MagicMock(
        name="WBUnpacker"
    )
    schema = Mock(
        name="Schema",
        type="array",
        items=Mock(name="Subschema", attributes={})
    )
    instance = MagicMock(
        name="WBRow",
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    wbnav = WBNav(unpacker, schema, instance)
    return unpacker, schema, instance, wbnav

def test_array_wbnav(wbnav_array):
    unpacker, schema, instance, wbnav = wbnav_array
    assert re.fullmatch(
        r"WBNav\(\<MagicMock name='WBUnpacker' id='\d+'\>, "
        r"\<Mock name='Schema' id='\d+'\>, "
        r"\<MagicMock name='WBRow' id='\d+'\>\)",
        repr(wbnav)
    ), f"Bad {wbnav!r}"
    new_nav_index = wbnav.index(42)
    assert new_nav_index.unpacker() == unpacker
    assert new_nav_index.instance == sentinel.INSTANCE_SLICE
    assert new_nav_index.schema == schema.items
    assert wbnav.value() == instance
    with pytest.raises(TypeError):
        wbnav.name("Nope")


@pytest.fixture
def csvnav_object():
    unpacker = MagicMock(
        name="CSVUnpacker"
    )
    schema = Mock(
        name="Schema",
        type="object",
        properties={"ITEM": Mock(name="Subschema", attributes={})}
    )
    instance = MagicMock(
        name="CSVRow",
        __getitem__=Mock(return_value=sentinel.INSTANCE_SLICE)
    )
    csvnav = CSVNav(unpacker, schema, instance)
    return unpacker, schema, instance, csvnav

def test_csvnav(csvnav_object):
    unpacker, schema, instance, csvnav = csvnav_object
    assert re.fullmatch(
        r"CSVNav\(\<MagicMock name='CSVUnpacker' id='\d+'\>, "
        r"\<Mock name='Schema' id='\d+'\>, "
        r"\<MagicMock name='CSVRow' id='\d+'\>\)",
        repr(csvnav)
    ), f"Bad {csvnav!r}"
    new_nav = csvnav.name("ITEM")
    assert new_nav.unpacker() == unpacker
    assert new_nav.instance == sentinel.INSTANCE_SLICE
    assert new_nav.schema == schema.properties["ITEM"]
    assert csvnav.value() == instance


### Instances

# These tests are effectively ``Nav`` test cases.
# They may be redundant.

def test_object_list_instance():
    object_schema = Mock(
        name="Schema",
        spec=ObjectSchema,
        type="object",
        properties={
            "F1": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "string"}),
            "F2": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "intger"}),
            "F4": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "number"}),
        },
        _attributes={},
        attributes={"type": "object"},
    )
    unpacker = Mock(name="WBUnpacker", calcsize=Mock(return_value=1), value=Mock(return_value="VALUE"))
    instance = ["Hello World", "42", "3.1415926"]
    nav = WBNav(unpacker, object_schema, instance)
    # instance.schema(object_schema).unpacker(unpacker)
    assert instance == instance
    assert re.fullmatch(
        r"WBNav\(\<Mock name='WBUnpacker' id='\d+'\>, \<Mock name='ITEM Subschema' spec='AtomicSchema' id='\d+'\>, 'Hello World'\)",
        repr(nav.name("F1"))
    )
    assert nav.name("F1").value() == "Hello World"
    assert nav.name("F2").value() == "42"
    with pytest.raises(TypeError):
        nav.name("F2").name("EXPECTED ERROR").value()
    with pytest.raises(TypeError):
        nav.index(0).value()

def test_array_list_instance(capsys):
    array_schema = Mock(
        name="Schema",
        spec=ObjectSchema,
        type="array",
        items=Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "string"}),
        _attributes={},
        attributes={"type": "object"},
        dump_iter=Mock(return_value=iter([(0, Mock(name="SCHEMA", attributes={"$anchor": "name"}), (), sentinel.VALUE)]))
    )
    unpacker = Mock(name="TextUnpacker", calcsize=Mock(return_value=42), value=Mock(return_value="VALUE"))
    instance = ["Hello World", 42, "3.1415926"]
    nav = WBNav(unpacker, array_schema, instance)
    # instance.schema(array_schema).unpacker(unpacker)
    assert instance == instance
    assert re.fullmatch(
        r"WBNav\(\<Mock name='TextUnpacker' id='\d+'\>, \<Mock name='ITEM Subschema' spec='AtomicSchema' id='\d+'\>, 'Hello World'\)",
        repr(nav.index(0))
    )
    assert nav.index(0).value() == "Hello World"
    assert nav.index(1).value() == 42
    with pytest.raises(TypeError):
        nav.name("F2").value()
    nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                                 Value',
        'name                                                  sentinel.VALUE'
    ]

def test_object_json_instance(capsys):
    object_schema = Mock(
        name="Schema",
        spec=ObjectSchema,
        type="object",
        properties={
            "F1": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "string"}),
            "F2": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "intger"}),
            "F4": Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "number"}),
        },
        _attributes={},
        attributes={"type": "object"},
        dump_iter=Mock(return_value=iter([(0, sentinel.SCHEMA, (), sentinel.VALUE)]))
    )
    unpacker = Mock(name="JSONUnpacker", calcsize=Mock(return_value=42), value=Mock(return_value="VALUE"))
    instance = {"F1": "Hello World", "F2": "42"}
    nav = DNav(unpacker, object_schema, instance)
    # instance.schema(object_schema).unpacker(unpacker)
    assert instance == instance
    assert re.fullmatch(
        r"DNav\(\<Mock name='JSONUnpacker' id='\d+'\>, \<Mock name='ITEM Subschema' spec='AtomicSchema' id='\d+'\>, 'Hello World'\)",
        repr(nav.name("F1"))
    ), f"Bad {nav.name('F1')!r}"
    assert nav.name("F1").value() == "Hello World"
    assert nav.name("F2").value() == "42"
    with pytest.raises(TypeError):
        nav.index(0)
    nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                                 Value',
        'object                                                sentinel.VALUE'
    ]


def test_array_json_instance(capsys):
    array_schema = Mock(
        name="Schema",
        spec=ObjectSchema,
        type="array",
        items=Mock(name="ITEM Subschema", spec=AtomicSchema, _attributes={}, attributes={"type": "string"}),
        _attributes={},
        attributes={"type": "array"},
        dump_iter=Mock(return_value=iter([(0, sentinel.SCHEMA, (), sentinel.VALUE)]))
    )
    unpacker = Mock(name="JSONUnpacker", calcsize=Mock(return_value=42), value=Mock(return_value="VALUE"))
    instance = ["Hello World", 42, 3.1415926]
    nav = DNav(unpacker, array_schema, instance)
    # instance.schema(array_schema).unpacker(unpacker)
    assert re.fullmatch(
        r"DNav\(\<Mock name='JSONUnpacker' id='\d+'\>, \<Mock name='ITEM Subschema' spec='AtomicSchema' id='\d+'\>, 'Hello World'\)",
        repr(nav.index(0))
    ), f"Bad {nav.index(0)!r}"
    assert nav.index(0).value() == "Hello World"
    assert nav.index(1).value() == 42
    with pytest.raises(TypeError):
        nav.name("Nope")
    nav.dump()
    out, err = capsys.readouterr()
    assert out.splitlines() == [
        'Field                                                 Value',
        'array                                                 sentinel.VALUE'
    ]
