package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression.{Addition, BinaryOperation}
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.{Term, Type, Value}
import com.preprocessor.ast.Ast.Value.Number


class StatementsSpec extends BaseParserSpec {

	behavior of "The statement parser"

	it should "correctly parse a no-op statement" in {
		assert(parse("") == NoOp)
	}

	it should "correctly parse an empty rule" in { // TODO
		assert(parse(
			""""div a strong" {
				|}
			""".stripMargin) == Rule(Value.String("div a strong"), sequence(NoOp)))
	}

	it should "correctly parse a type alias declaration" in {
		assert(parse("@type IntOrString = Integer | String") ==
			TypeAliasDeclaration(Type.TypeAlias("IntOrString"), Type.Union(Set(Type.Integer, Type.String))))
	}

	it should "correctly parse expressions as statements" in {
		assert(parse("1 + 2 + 3") ==
			BinaryOperation(Addition, BinaryOperation(Addition, Number(1), Number(2)), Number(3))
		)
		assert(parse("[1, 2, 3]") == Term.List(Vector(Number(1), Number(2), Number(3))))
	}

	//it should "correctly parse a function call" in {
	//	assert(parse("color red") == Rule(Value.String("div a strong"), sequence(NoOp)))
	//}

	// Everything will be a sequence, really, so this will make the tests more concise.
	protected def parse(input: java.lang.String): Statement = stripSequence(parseRule(input, _.Statement))


	private def sequence(statement: Statement): Statement = Sequence(statement, NoOp)

	private def stripSequence(statement: Statement): Statement = statement match {
		case Sequence(inner, NoOp) => inner
		case otherStatement => fail(otherStatement.toString)
	}
}
