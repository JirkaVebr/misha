package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term.{FunctionCall, Variable}
import com.preprocessor.ast.Ast.Value._

class ExpressionSpec extends BaseParserSpec {

	behavior of "The expression parser"

	it should "correctly parse addition & subtraction" in {
		assert(parse("1 + 2 + 3") ==
			BinaryOperation(Addition, BinaryOperation(Addition, Number(1), Number(2)), Number(3))
		)
		assert(parse("3 - 2 - 1") ==
			BinaryOperation(Subtraction, BinaryOperation(Subtraction, Number(3), Number(2)), Number(1))
		)
	}

	it should "correctly parse multiplication" in {
		assert(parse("1 * 2 * 3") ==
			BinaryOperation(Multiplication, BinaryOperation(Multiplication, Number(1), Number(2)), Number(3))
		)
		assert(parse("3 / 2 / 1") ==
			BinaryOperation(Division, BinaryOperation(Division, Number(3), Number(2)), Number(1))
		)
		assert(parse("3 % 2 % 1") ==
			BinaryOperation(Remainder, BinaryOperation(Remainder, Number(3), Number(2)), Number(1))
		)
	}

	it should "correctly parse variable names" in {
		assert(parse("$myVariable") == Variable("myVariable"))
	}

	it should "correctly parse a nullary function call" in {
		assert(parse("myFunction()") == FunctionCall(Variable("myFunction")))
		assert(parse("$myFunction()") == FunctionCall(Variable("myFunction")))
		assert(parse("($myFunction)()") == FunctionCall(Variable("myFunction")))
	}

	it should "correctly parse a sub-expression" in {
		assert(parse("(1)") == Number(1))
	}

	it should "correctly parse a conditional" in {
		assert(parse("@if ($myVariable) 1 @else 2") ==
			Conditional(Variable("myVariable"), Number(1), Some(Number(2))))
	}

	/*it should "correctly parse left associativity" in {
		assert(parse("1 lop 2 lop 3 lop 4") == BinLop(BinLop(BinLop(Num(1), Num(2)), Num(3)), Num(4)))
	}*/

	/*it should "correctly parse right associativity" in {
		assert(parse("1 rop1 2 rop1 3 rop2 4") == BinRop1(Num(1), BinRop1(Num(2), BinRop2(Num(3), Num(4)))))
	}*/

	//protected def parse(input: String): Expr = parseRule(input, _.AssociativityTest)

	protected def parse(input: java.lang.String): Expression = parseRule(input, _.Expression)

}
