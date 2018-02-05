package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term
import com.preprocessor.ast.Ast.Term.{FunctionCall, Variable}
import com.preprocessor.ast.Ast.Value._

class ExpressionSpec extends BaseParserSpec {

	behavior of "The expression parser"

	it should "correctly parse logical operations" in {
		assert(parse("false && true") == BinaryOperation(LogicalAnd, Boolean(false), Boolean(true)))
		assert(parse("true || false") == BinaryOperation(LogicalOr, Boolean(true), Boolean(false)))
		assert(parse("$a || $b && $c") == BinaryOperation(LogicalOr, Variable("a"), BinaryOperation(LogicalAnd, Variable("b"), Variable("c"))))
	}

	it should "correctly parse equality comparison" in {
		assert(parse("1 == 2 == 3") ==
			BinaryOperation(IsEqualTo, BinaryOperation(IsEqualTo, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 != 2 != 3") ==
			UnaryOperation(LogicalNegation, BinaryOperation(
				IsEqualTo, UnaryOperation(LogicalNegation, BinaryOperation(IsEqualTo, Number(1), Number(2))), Number(3))
			)
		)
	}

	it should "correctly parse other comparison" in {
		assert(parse("1 < 2 < 3") ==
			BinaryOperation(LowerThan, BinaryOperation(LowerThan, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 <= 2 <= 3") ==
			BinaryOperation(LowerEquals, BinaryOperation(LowerEquals, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 > 2 > 3") ==
			BinaryOperation(GreaterThan, BinaryOperation(GreaterThan, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 >= 2 >= 3") ==
			BinaryOperation(GreaterEquals, BinaryOperation(GreaterEquals, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 @in 2 @in 3") ==
			BinaryOperation(In, BinaryOperation(In, Number(1), Number(2)), Number(3))
		)
	}

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

	it should "correctly parse exponentiation" in {
		assert(parse("2 ^ 3 ^ 4") == BinaryOperation(Exponentiation, Number(2), BinaryOperation(Exponentiation, Number(3), Number(4))))
	}

	it should "correctly parse unary expressions" in {
		assert(parse("-(1)") == UnaryOperation(ArithmeticNegation, Number(1)))
		assert(parse("---(1)") == UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Number(1)))))
		assert(parse("---1") == UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Number(1)))))
		assert(parse("!true") == UnaryOperation(LogicalNegation, Boolean(true)))
		assert(parse("!!!true") == UnaryOperation(LogicalNegation, UnaryOperation(LogicalNegation, UnaryOperation(LogicalNegation, Boolean(true)))))
	}

	it should "correctly parse variable names" in {
		assert(parse("$myVariable") == Variable("myVariable"))
	}

	it should "correctly parse a nullary function call" in {
		assert(parse("myFunction()") == FunctionCall(Variable("myFunction")))
		assert(parse("$myFunction()") == FunctionCall(Variable("myFunction")))
		assert(parse("($myFunction)()") == FunctionCall(Variable("myFunction")))
	}

	it should "correctly parse a function call with arguments" in {
		assert(parse("myFunction(1)") == FunctionCall(Variable("myFunction"), Vector(Number(1))))
		assert(parse("myFunction(1, 2)") == FunctionCall(Variable("myFunction"), Vector(Number(1), Number(2))))
		assert(parse("myFunction(1, 2,)") == FunctionCall(Variable("myFunction"), Vector(Number(1), Number(2))))
		assert(parse("$myFunction(1 + 2,)") ==
			FunctionCall(Variable("myFunction"), Vector(BinaryOperation(Addition, Number(1), Number(2)))))
		// This should fail but doesn't yet "myFunction(,)"
	}

	it should "correctly parse a sub-expression" in {
		assert(parse("(1)") == Number(1))
	}

	it should "correctly parse a delimited list" in {
		assert(parse("[1 2 3 ]") == Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1, 2, 3]") == Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1, 2, 3,]") == Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1 2 + 2 3]") == Term.List(Vector(Number(1), BinaryOperation(Addition, Number(2), Number(2)), Number(3))))
		assert(parse("[]") == Term.List(Vector()))
		assert(parse("[,]") == Term.List(Vector())) // This probably shouldn't be allowed
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
