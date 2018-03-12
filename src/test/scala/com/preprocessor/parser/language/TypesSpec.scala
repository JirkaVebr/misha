package com.preprocessor.parser.language

import com.preprocessor.ast.Language.{Type, Value}
import com.preprocessor.parser.BaseParserSpec

class TypesSpec extends BaseParserSpec {

	behavior of "The type parser"

	it should "parse builtin types" in {
		assert(parse("Any") === Type.Any)
		assert(parse("Color") === Type.Color)
		assert(parse("Boolean") === Type.Boolean)
		assert(parse("Percentage") === Type.Percentage)
		assert(parse("Scalar") === Type.Scalar)
		assert(parse("String") === Type.String)
		assert(parse("Unit") === Type.Unit)
	}

	it should "parse a facultative type" in {
		assert(parse("Scalar?") === Type.Union(Set(Type.Scalar, Type.Unit)))
		assert(parse("() => Scalar?") === Type.Function(List(), Type.Union(Set(Type.Scalar, Type.Unit))))
	}

	it should "parse type aliases" in {
		assert(parse("MyCustomType") === Type.TypeAlias("MyCustomType"))
	}

	it should "parse a nullary Function type" in {
		assert(parse("() => Unit") === Type.Function(List(), Type.Unit))
	}

	it should "parse a unary Function type" in {
		assert(parse("(String) => Unit") === Type.Function(List(Type.String), Type.Unit))
	}

	it should "parse a binary Function type" in {
		assert(parse("(Scalar, String) => Unit") === Type.Function(List(Type.Scalar, Type.String), Type.Unit))
	}

	it should "parse a unary Function type with a trailing comma" in {
		assert(parse("(Scalar, ) => Unit") === Type.Function(List(Type.Scalar), Type.Unit))
	}

	it should "parse a large Function type spanning several lines" in {
		assert(parse(
			"""(
				|	Scalar,
				|	String,
				|) => String
			""".stripMargin.trim) === Type.Function(List(Type.Scalar, Type.String), Type.String))
	}


	it should "parse a union type" in {
		assert(parse("Scalar | String | Boolean") === Type.Union(Set(Type.Scalar, Type.String, Type.Boolean)))
	}

	it should "parse a literal type" in {
		assert(parse("123") === Type.Literal(Value.Scalar(123)))
		assert(parse("true") === Type.Literal(Value.Boolean(true)))
	}

	it should "parse a literal type in a union" in {
		assert(parse("Scalar | 123") === Type.Union(Set(Type.Scalar, Type.Literal(Value.Scalar(123)))))
	}

	it should "parse a basic subtraction type" in {
		assert(parse("Scalar -- 123") === Type.Subtraction(Type.Scalar, Type.Literal(Value.Scalar(123))))
	}

	it should "parse a complex subtraction type" in {
		assert(parse("Scalar | String | Boolean -- 123 | true") ===
			Type.Subtraction(
				Type.Union(Set(Type.Scalar, Type.String, Type.Boolean)),
				Type.Union(Set(Type.Literal(Value.Scalar(123)), Type.Literal(Value.Boolean(true))))
			)
		)
	}

	it should "parse built-in parametrized types" in {
		assert(parse("List[Scalar]") === Type.List(Type.Scalar))
		assert(parse("Formula[Length]") === Type.Formula(Type.TypeAlias("Length")))
		assert(parse("Map[String, Percentage]") === Type.Map(Type.String, Type.Percentage))
	}


	protected def parse(input: String): Type.Any = parseLanguageRule(input, _.Type)
}
