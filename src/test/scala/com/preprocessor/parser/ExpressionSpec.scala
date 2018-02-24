package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term
import com.preprocessor.ast.Ast.Term.{FunctionCall, MemberAccess, ParentSelector, Variable}
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
			BinaryOperation(IsEqualTo, BinaryOperation(IsEqualTo, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("1 != 2 != 3") ==
			UnaryOperation(LogicalNegation, BinaryOperation(
				IsEqualTo, UnaryOperation(LogicalNegation, BinaryOperation(IsEqualTo, Scalar(1), Scalar(2))), Scalar(3))
			)
		)
	}

	it should "correctly parse other comparison" in {
		assert(parse("1 < 2 < 3") ==
			BinaryOperation(LowerThan, BinaryOperation(LowerThan, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("1 <= 2 <= 3") ==
			BinaryOperation(LowerEquals, BinaryOperation(LowerEquals, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("1 > 2 > 3") ==
			BinaryOperation(GreaterThan, BinaryOperation(GreaterThan, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("1 >= 2 >= 3") ==
			BinaryOperation(GreaterEquals, BinaryOperation(GreaterEquals, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("1 @in 2 @in 3") ==
			BinaryOperation(In, BinaryOperation(In, Scalar(1), Scalar(2)), Scalar(3))
		)
	}

	it should "correctly parse addition & subtraction" in {
		assert(parse("1 + 2 + 3") ==
			BinaryOperation(Addition, BinaryOperation(Addition, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("3 - 2 - 1") ==
			BinaryOperation(Subtraction, BinaryOperation(Subtraction, Scalar(3), Scalar(2)), Scalar(1))
		)
	}

	it should "correctly parse multiplication" in {
		assert(parse("1 * 2 * 3") ==
			BinaryOperation(Multiplication, BinaryOperation(Multiplication, Scalar(1), Scalar(2)), Scalar(3))
		)
		assert(parse("3 / 2 / 1") ==
			BinaryOperation(Division, BinaryOperation(Division, Scalar(3), Scalar(2)), Scalar(1))
		)
		assert(parse("3 % 2 % 1") ==
			BinaryOperation(Remainder, BinaryOperation(Remainder, Scalar(3), Scalar(2)), Scalar(1))
		)
	}

	it should "correctly parse exponentiation" in {
		assert(parse("2 ^ 3 ^ 4") == BinaryOperation(Exponentiation, Scalar(2), BinaryOperation(Exponentiation, Scalar(3), Scalar(4))))
	}

	it should "correctly parse unary expressions" in {
		assert(parse("-(1)") == UnaryOperation(ArithmeticNegation, Scalar(1)))
		assert(parse("---(1)") == UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Scalar(1)))))
		assert(parse("---1") == UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Scalar(1)))))
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
		assert(parse("myFunction(1)") == FunctionCall(Variable("myFunction"), Vector(Scalar(1))))
		assert(parse("myFunction(1, 2)") == FunctionCall(Variable("myFunction"), Vector(Scalar(1), Scalar(2))))
		assert(parse("myFunction(1, 2,)") == FunctionCall(Variable("myFunction"), Vector(Scalar(1), Scalar(2))))
		assert(parse("$myFunction(1 + 2,)") ==
			FunctionCall(Variable("myFunction"), Vector(BinaryOperation(Addition, Scalar(1), Scalar(2)))))
		// This should fail but doesn't yet "myFunction(,)"
	}

	it should "correctly parse a sub-expression" in {
		assert(parse("(1)") == Scalar(1))
	}

	it should "correctly parse a magic symbol" in {
		assert(parse("&") == ParentSelector)
		assert(parse("&") == ParentSelector)
	}

	it should "correctly parse a delimited list" in {
		assert(parse("[1 2 3 ]") == Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
		assert(parse("[1, 2, 3]") == Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
		assert(parse("[1, 2, 3,]") == Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))))
		assert(parse("[1 2 + 2 3]") == Term.List(Vector(Scalar(1), BinaryOperation(Addition, Scalar(2), Scalar(2)), Scalar(3))))
		assert(parse("[]") == Term.List(Vector()))
		assert(parse("[,]") == Term.List(Vector())) // This probably shouldn't be allowed
	}

	it should "correctly parse an undelimited list" in {
		assert(parse("myFunction(1 2 3, 2)") ==
			FunctionCall(Variable("myFunction"), Vector(Term.List(Vector(Scalar(1), Scalar(2), Scalar(3))), Scalar(2))))
		assert(parse("myFunction(1 2 + 3 4, 2)") ==
			FunctionCall(Variable("myFunction"), Vector(Term.List(Vector(Scalar(1), BinaryOperation(Addition, Scalar(2), Scalar(3)), Scalar(4))), Scalar(2))))
	}

	it should "correctly parse a conditional" in {
		assert(parse("@if ($myVariable) 1 @else 2") ==
			Conditional(Variable("myVariable"), Scalar(1), Some(Scalar(2))))
	}

	it should "correctly parse computed member access" in {
		assert(parse("$a[1 + 2]") == MemberAccess(Variable("a"), BinaryOperation(Addition, Scalar(1), Scalar(2))))
	}

	it should "correctly parse member access" in {
		assert(parse("$a.someMember") == MemberAccess(Variable("a"), String("someMember")))
		assert(parse("''.length") == MemberAccess(String(""), String("length")))
		assert(parse("123.456.foo") == MemberAccess(Scalar(123.456), String("foo")))
		assert(parse("1.2e3.bar") == MemberAccess(Scalar(1200), String("bar")))
		// TODO fix precedence //assert(parse("1.2e3.bar()") == FunctionCall(MemberAccess(Scalar(1200), String("barr"))))
	}

	it should "correctly parse complicated arithmetic expressions" in {
		assert(parse("2 * 3 ^ 4") == BinaryOperation(Multiplication, Scalar(2), BinaryOperation(Exponentiation, Scalar(3), Scalar(4))))
		assert(parse("(1 + 2) / 3") == BinaryOperation(Division, BinaryOperation(Addition, Scalar(1), Scalar(2)), Scalar(3)))
	}


	protected def parse(input: java.lang.String): Expression = parseRule(input, _.Expression)

}
