package com.mishaLang.parser.language

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Statement._
import com.mishaLang.ast.Language.Term.{FunctionCall, ParentSelector, Variable}
import com.mishaLang.ast.Language.Value.{Important, Number, Rgba}
import com.mishaLang.ast.Language.{Term, Type, Value, ValueSymbolDeclaration}
import com.mishaLang.ast.NumberUnit.Atomic
import com.mishaLang.parser.BaseParserSpec
import com.mishaLang.spec.ColorKeywords
import com.mishaLang.spec.units.Time.Second


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
				|""".stripMargin) === Rule(Vector(Left("div a strong")), Block(Number(123))))
	}

	it should "not get confused by at-rules" in {
		assert(parse(
			"""@keyframes foo
				|	123
				|""".stripMargin) === Rule(Vector(Left("@keyframes foo")), Block(Number(123))))
	}

	it should "allow selector lists, separated by commas, on a single line" in {
		assert(parse(
			"""div, a strong, .foo
				|	123
				|""".stripMargin) === Rule(Vector(Left("div, a strong, .foo")), Block(Number(123))))
	}

	it should "allow multi-line selectors if separated by commas" in {
		assert(parse(
			"""div,
				|a strong,
				|.foo
				|	123
				|""".stripMargin) === Rule(Vector(Left("div,\na strong,\n.foo")), Block(Number(123))))
	}

	it should "correctly pares implicit expression interpolation within selectors" in {
		assert(parse(
			"""& &-active
				|	123
				|""".stripMargin) ===
			Rule(Vector(
				Right(ParentSelector),
				Left(" "),
				Right(ParentSelector),
				Left("-active")
			), Block(Number(123))))
	}

	it should "correctly parse explicit expression interpolation within selectors" in {
		assert(parse(
			"""d{i}v
				|	123
				|""".stripMargin) === Rule(Vector(Left("d"), Right(Term.List(Vector(Value.String("i")))), Left("v")), Block(Number(123))))
		assert(parse(
			"""*:first{"-" + "type"}
				|	123
				|""".stripMargin) ===
			Rule(Vector(
				Left("*:first"),
				Right(Term.List(Vector(BinaryOperation(Addition, Value.String("-"), Value.String("type")))))
			), Block(Number(123))))
	}

	it should "correctly parse escaped values" in {
		assert(parse(
			"""se\{lect\&or
				|	123
				|""".stripMargin) === Rule(Vector(Left("se{lect&or")), Block(Number(123))))
	}

	it should "correctly parse a type alias declaration" in {
		assert(parse("@type NumOrString = Scalar | String") ===
			TypeAliasDeclaration(Type.TypeAlias("NumOrString"), Type.Union(Set(Type.Scalar, Type.String))))
	}

	it should "correctly parse expressions as statements" in {
		assert(parse("1 + 2 + 3") ===
			BinaryOperation(Addition, BinaryOperation(Addition, Number(1), Number(2)), Number(3))
		)
		assert(parse("[1, 2, 3]") === Term.List(Vector(Number(1), Number(2), Number(3))))
	}

	it should "correctly parse a variable declaration" in {
		assert(parse("@let $myVar = 1") === VariableDeclaration(ValueSymbolDeclaration("myVar", None, Number(1))))
		assert(parse("@let $myVar: Scalar = 1") === VariableDeclaration(ValueSymbolDeclaration("myVar", Some(Type.Scalar), Number(1))))
	}

	it should "correctly parse a block" in {
		assert(parse(
			"""@let $myVar =
				|	123
				|	456
				|""".stripMargin) === VariableDeclaration(ValueSymbolDeclaration("myVar", None, Block(Sequence(Number(123), Number(456))))))
	}

	it should "correctly parse a function declaration" in {
		assert(parse(
			"""@let $color = ($color: Color) =>
				|	@property("color", $color)
				|""".stripMargin
		) === VariableDeclaration(ValueSymbolDeclaration("color", None, Term.Function(
			None, Vector(ValueSymbolDeclaration[Unit]("color", Some(Type.Color), Unit)), Vector(), None, Block(
				Property("color", Variable("color"))
			)
		))))
	}

	it should "correctly parse a property" in {
		assert(parse("@property(\"color\", blue)") === Property(Value.String("color"), ColorKeywords.Colors("blue"), None))
		assert(parse("@property(\"color\", blue, !important)") ===
			Property(Value.String("color"), ColorKeywords.Colors("blue"), Some(Important)))
	}

	it should "correctly parse a sugared property/function call" in {
		assert(parse("color red") === FunctionCall(Term.Variable("color"), List(Rgba(255, 0, 0))))
		assert(parse("transition color .3s, opacity .5s ease-in-out") === FunctionCall(
			Term.Variable("transition"), Vector(Term.List(Vector(
				Term.List(Vector(Value.String("color"), Value.Number(.3, Atomic(Second)))),
				Term.List(Vector(Value.String("opacity"), Value.Number(.5, Atomic(Second)), Value.String("ease-in-out")))
			)))))
	}

	it should "correctly parse noOp" in {
		assert(parse("@noOp") === NoOp)
		assert(parse("@noop") === NoOp)
		assert(parse("@no-op") === NoOp)
		assert(parse("@no-Op") === NoOp)
		assert(parse("@NOOP") === NoOp)
	}

	it should "correctly parse an each loop" in {
		assert(parse(
			"""@each $i @in [1 2 3]
				|	123
				|""".stripMargin) === Each(Variable("i"), Term.List(Vector(
			Value.Number(1), Value.Number(2), Value.Number(3)
		)), Block(Value.Number(123))))
	}

	protected def parse(input: java.lang.String): Statement = parseLanguageRule(input, _.Statement)
}
