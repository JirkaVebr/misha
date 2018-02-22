package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Value.Scalar
import com.preprocessor.ast.Ast.{Term, Type, Value}


class StatementsSpec extends BaseParserSpec {

	behavior of "The statement parser"

	it should "correctly parse a simple rule" in { // TODO use indent/dedent
		assert(parse(
			""""div a strong"
				|{123
				|}""".stripMargin) == Rule(Value.String("div a strong"), Block(Scalar(123))))
	}

	it should "correctly parse a type alias declaration" in {
		assert(parse("@type IntOrString = Integer | String") ==
			TypeAliasDeclaration(Type.TypeAlias("IntOrString"), Type.Union(Set(Type.Integer, Type.String))))
	}

	it should "correctly parse expressions as statements" in {
		assert(parse("1 + 2 + 3") ==
			BinaryOperation(Addition, BinaryOperation(Addition, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("[1, 2, 3]") == Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
	}

	it should "correctly parse a variable declaration" in {
		assert(parse("@let $myVar = 1") == VariableDeclaration("myVar", None, Scalar(1)))
		assert(parse("@let $myVar: Number = 1") == VariableDeclaration("myVar", Some(Type.Number), Scalar(1)))
	}

	it should "correctly parse a block" in { // TODO
		assert(parse(
			"""{123
				|@let $myVar = 1
				|789
				|}""".stripMargin) == Block(Sequence(Sequence(Scalar(123), VariableDeclaration("myVar", None, Scalar(1))), Scalar(789))))
	}

	//it should "correctly parse a function call" in {
	//	assert(parse("color red") == Rule(Value.String("div a strong"), sequence(NoOp)))
	//}

	protected def parse(input: java.lang.String): Statement = parseRule(input, _.Statement)
}
