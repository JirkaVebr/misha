package com.preprocessor.parser.language

import com.preprocessor.ast.Language.Expression._
import com.preprocessor.ast.Language.Statement._
import com.preprocessor.ast.Language.Term.{FunctionCall, ParentSelector}
import com.preprocessor.ast.Language.Value.{Important, Rgba, Scalar}
import com.preprocessor.ast.Language.{Term, Type, Value, ValueSymbolDeclaration}
import com.preprocessor.ast.NumberUnit.UnitOfMeasure
import com.preprocessor.parser.BaseParserSpec
import com.preprocessor.spec.ColorKeywords


class StatementsSpec extends BaseParserSpec {

	behavior of "The statement parser"

	// TODO allow empty rules?
	/*it should "correctly parse an empty rule rule" in {
		assert(parse(
			"""div
				|""".stripMargin) === NoOp)
	}*/

	it should "correctly parse a simple rule" in {
		assert(parse(
			"""div a strong
				|	123
				|""".stripMargin) === Rule(Vector(Left("div a strong")), Block(Scalar(123))))
	}

	it should "not get confused by at-rules" in {
		assert(parse(
			"""@keyframes foo
				|	123
				|""".stripMargin) === Rule(Vector(Left("@keyframes foo")), Block(Scalar(123))))
	}

	it should "allow multi-line selectors if separated by commas" in {
		assert(parse(
			"""div,
				|a strong,
				|.foo
				|	123
				|""".stripMargin) === Rule(Vector(Left("div,\na strong,\n.foo")), Block(Scalar(123))))
	}

	it should "correctly pares implicit expression interpolation within selectors" in {
		assert(parse(
			"""& &-active
				|	123
				|""".stripMargin) ===
			Rule(Vector(
				Right(Vector(ParentSelector)),
				Left(" "),
				Right(Vector(ParentSelector)),
				Left("-active")
			), Block(Scalar(123))))
	}

	it should "correctly parse explicit expression interpolation within selectors" in {
		assert(parse(
			"""d{i}v
				|	123
				|""".stripMargin) === Rule(Vector(Left("d"), Right(Vector(Value.String("i"))), Left("v")), Block(Scalar(123))))
		assert(parse(
			"""*:first{"-" + "type"}
				|	123
				|""".stripMargin) ===
			Rule(Vector(
				Left("*:first"),
				Right(Vector(BinaryOperation(Addition, Value.String("-"), Value.String("type"))))
			), Block(Scalar(123))))
	}

	it should "correctly parse a type alias declaration" in {
		assert(parse("@type NumOrString = Scalar | String") ===
			TypeAliasDeclaration(Type.TypeAlias("NumOrString"), Type.Union(Set(Type.Scalar, Type.String))))
	}

	it should "correctly parse expressions as statements" in {
		assert(parse("1 + 2 + 3") ===
			BinaryOperation(Addition, BinaryOperation(Addition, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("[1, 2, 3]") === Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
	}

	it should "correctly parse a variable declaration" in {
		assert(parse("@let $myVar = 1") === VariableDeclaration(ValueSymbolDeclaration("myVar", None, Scalar(1))))
		assert(parse("@let $myVar: Scalar = 1") === VariableDeclaration(ValueSymbolDeclaration("myVar", Some(Type.Scalar), Scalar(1))))
	}

	it should "correctly parse a block" in {
		assert(parse(
			"""@let $myVar =
				|	123
				|	456
				|""".stripMargin) === VariableDeclaration(ValueSymbolDeclaration("myVar", None, Block(Sequence(Scalar(123), Scalar(456))))))
	}

	it should "correctly parse a property" in {
		assert(parse("@property(\"color\", blue)") === Property(Value.String("color"), ColorKeywords.map("blue"), None))
		assert(parse("@property(\"color\", blue, !important)") ===
			Property(Value.String("color"), ColorKeywords.map("blue"), Some(Important)))
	}

	it should "correctly parse a sugared property/function call" in {
		assert(parse("color red") === FunctionCall(Term.Variable("color"), List(Rgba(255, 0, 0))))
		assert(parse("transition color .3s, opacity .5s ease-in-out") === FunctionCall(
			Term.Variable("transition"), Vector(Term.List(Vector(
				Term.List(Vector(Value.String("color"), Value.Dimensioned(.3, UnitOfMeasure(Map("s" -> 1))))),
				Term.List(Vector(Value.String("opacity"), Value.Dimensioned(.5, UnitOfMeasure(Map("s" -> 1))), Value.String("ease-in-out")))
			)))))
	}

	it should "correctly parse noOp" in {
		assert(parse("@noOp") === NoOp)
		assert(parse("@noop") === NoOp)
		assert(parse("@no-op") === NoOp)
		assert(parse("@no-Op") === NoOp)
		assert(parse("@NOOP") === NoOp)
	}

	protected def parse(input: java.lang.String): Statement = parseLanguageRule(input, _.Statement)
}
