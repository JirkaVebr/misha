package com.preprocessor.parser

import com.preprocessor.ast.Ast.{Type, Value}

class TypesSpec extends BaseParserSpec {

	behavior of "The type parser"

	it should "parse the Unit type" in {
		assert(parse("Unit") == Type.Unit)
	}

	it should "parse the Boolean type" in {
		assert(parse("Boolean") == Type.Boolean)
	}

	it should "parse the String type" in {
		assert(parse("String") == Type.String)
	}

	it should "parse the Integer type" in {
		assert(parse("Integer") == Type.Integer)
	}


	it should "parse a nullary Function type" in {
		assert(parse("() => Unit") == Type.Function(List(), Type.Unit))
	}

	it should "parse a unary Function type" in {
		assert(parse("(String) => Unit") == Type.Function(List(Type.String), Type.Unit))
	}

	it should "parse a binary Function type" in {
		assert(parse("(Integer, String) => Unit") == Type.Function(List(Type.Integer, Type.String), Type.Unit))
	}

	it should "parse a unary Function type with a trailing comma" in {
		assert(parse("(Integer, ) => Unit") == Type.Function(List(Type.Integer), Type.Unit))
	}

	it should "parse a large Function type spanning several lines" in {
		assert(parse(
			"""
				|(
				|	Integer,
				|	String,
				|) => String
			""".stripMargin.trim) == Type.Function(List(Type.Integer, Type.String), Type.String))
	}


	it should "parse a union type" in {
		assert(parse("Integer | String | Boolean") == Type.Union(Set(Type.Integer, Type.String, Type.Boolean)))
	}

	it should "parse a literal type" in {
		assert(parse("123") == Type.Literal(Value.Number(123)))
		assert(parse("true") == Type.Literal(Value.Boolean(true)))
	}

	it should "parse a literal type in a union" in {
		assert(parse("Integer | 123") == Type.Union(Set(Type.Integer, Type.Literal(Value.Number(123)))))
	}

	it should "parse a basic subtraction type" in {
		assert(parse("Integer -- 123") == Type.Subtraction(Type.Integer, Type.Literal(Value.Number(123))))
	}

	it should "parse a complex subtraction type" in {
		assert(parse("Integer | String | Boolean -- 123 | true") ==
			Type.Subtraction(
				Type.Union(Set(Type.Integer, Type.String, Type.Boolean)),
				Type.Union(Set(Type.Literal(Value.Number(123)), Type.Literal(Value.Boolean(true))))
			)
		)
	}


	protected def parse(input: String): Type.Any = parseRule(input, _.Type)
}
