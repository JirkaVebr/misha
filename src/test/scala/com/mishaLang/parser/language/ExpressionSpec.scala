package com.mishaLang.parser.language

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Term._
import com.mishaLang.ast.Language.Value._
import com.mishaLang.ast.Language.{Term, Type, ValueSymbolDeclaration}
import com.mishaLang.interpreter.Symbol.ValueSymbol
import com.mishaLang.parser.BaseParserSpec

class ExpressionSpec extends BaseParserSpec {

	behavior of "The expression parser"

	it should "correctly parse logical operations" in {
		assert(parse("false && true") === BinaryOperation(LogicalAnd, Boolean(false), Boolean(true)))
		assert(parse("true || false") === BinaryOperation(LogicalOr, Boolean(true), Boolean(false)))
		assert(parse("$a || $b && $c") === BinaryOperation(LogicalOr, Variable("a"), BinaryOperation(LogicalAnd, Variable("b"), Variable("c"))))
	}

	it should "correctly parse equality comparison" in {
		assert(parse("1 == 2 == 3") ==
			BinaryOperation(IsEqualTo, BinaryOperation(IsEqualTo, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 != 2 != 3") ===
			UnaryOperation(LogicalNegation, BinaryOperation(
				IsEqualTo, UnaryOperation(LogicalNegation, BinaryOperation(IsEqualTo, Number(1), Number(2))), Number(3))
			)
		)
	}

	it should "correctly parse other comparison" in {
		assert(parse("1 < 2 < 3") ===
			BinaryOperation(LowerThan, BinaryOperation(LowerThan, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 <= 2 <= 3") ===
			BinaryOperation(LowerEquals, BinaryOperation(LowerEquals, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 > 2 > 3") ===
			BinaryOperation(GreaterThan, BinaryOperation(GreaterThan, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 >= 2 >= 3") ===
			BinaryOperation(GreaterEquals, BinaryOperation(GreaterEquals, Number(1), Number(2)), Number(3))
		)
		assert(parse("1 @in 2 @in 3") ===
			BinaryOperation(In, BinaryOperation(In, Number(1), Number(2)), Number(3))
		)
	}

	it should "correctly parse addition & subtraction" in {
		assert(parse("1 + 2 + 3") ===
			BinaryOperation(Addition, BinaryOperation(Addition, Number(1), Number(2)), Number(3))
		)
		assert(parse("3 - 2 - 1") ===
			BinaryOperation(Subtraction, BinaryOperation(Subtraction, Number(3), Number(2)), Number(1))
		)
	}

	it should "correctly parse multiplication" in {
		assert(parse("1 * 2 * 3") ===
			BinaryOperation(Multiplication, BinaryOperation(Multiplication, Number(1), Number(2)), Number(3))
		)
		assert(parse("3 / 2 / 1") ===
			BinaryOperation(Division, BinaryOperation(Division, Number(3), Number(2)), Number(1))
		)
		assert(parse("3 % 2 % 1") ===
			BinaryOperation(Remainder, BinaryOperation(Remainder, Number(3), Number(2)), Number(1))
		)
	}

	it should "correctly parse exponentiation" in {
		assert(parse("2 ^ 3 ^ 4") === BinaryOperation(Exponentiation, Number(2), BinaryOperation(Exponentiation, Number(3), Number(4))))
	}

	it should "correctly parse unary expressions" in {
		assert(parse("-(1)") === UnaryOperation(ArithmeticNegation, Number(1)))
		assert(parse("---(1)") === UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Number(1)))))
		assert(parse("---1") === UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, UnaryOperation(ArithmeticNegation, Number(1)))))
		assert(parse("!true") === UnaryOperation(LogicalNegation, Boolean(true)))
		assert(parse("!!!true") === UnaryOperation(LogicalNegation, UnaryOperation(LogicalNegation, UnaryOperation(LogicalNegation, Boolean(true)))))
	}

	it should "correctly parse variable names" in {
		assert(parse("$myVariable") === Variable("myVariable"))
	}

	it should "correctly parse property variable names" in {
		assert(parse("$$myProperty") === PropertyVariable("myProperty"))
	}

	it should "correctly parse a nullary function call" in {
		assert(parse("myFunction()") === FunctionCall(Variable("myFunction")))
		assert(parse("$myFunction()") === FunctionCall(Variable("myFunction")))
		assert(parse("($myFunction)()") === FunctionCall(Variable("myFunction")))
	}

	it should "correctly parse a function call with arguments" in {
		assert(parse("myFunction(1)") === FunctionCall(Variable("myFunction"), Vector(Number(1))))
		assert(parse("myFunction(1, 2)") === FunctionCall(Variable("myFunction"), Vector(Number(1), Number(2))))
		assert(parse("myFunction(1, 2,)") === FunctionCall(Variable("myFunction"), Vector(Number(1), Number(2))))
		assert(parse("$myFunction(1 + 2,)") ===
			FunctionCall(Variable("myFunction"), Vector(BinaryOperation(Addition, Number(1), Number(2)))))
		// This should fail but doesn't yet "myFunction(,)"
	}

	it should "correctly parse a sub-expression" in {
		assert(parse("(1)") === Number(1))
	}

	it should "correctly parse a magic symbol" in {
		assert(parse("&") === ParentSelector)
		assert(parse("&") === ParentSelector)
	}

	it should "correctly parse a delimited list" in {
		assert(parse("[1 2 3 ]") === Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1, 2, 3]") === Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1, 2, 3,]") === Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse("[1 2 + 2 3]") === Term.List(Vector(Number(1), BinaryOperation(Addition, Number(2), Number(2)), Number(3))))
		assert(parse("[]") === Term.List(Vector()))
		assert(parse("[,]") === Term.List(Vector())) // This probably shouldn't be allowed
	}

	it should "correctly parse a delimited list spanning several lines" in {
		assert(parse(
			"""[
				|	1
				|	2
				|	3
				|]"""".stripMargin) === Term.List(Vector(Number(1), Number(2), Number(3))))
		assert(parse(
			"""[
				|	1,
				|	2,
				|	3
				|]""".stripMargin) === Term.List(Vector(Number(1), Number(2), Number(3))))
	}

	it should "correctly parse an undelimited list" in {
		assert(parse("myFunction(1 2 3, 2)") ===
			FunctionCall(Variable("myFunction"), Vector(Term.List(Vector(Number(1), Number(2), Number(3))), Number(2))))
		assert(parse("myFunction(1 2 + 3 4, 2)") ===
			FunctionCall(Variable("myFunction"), Vector(Term.List(Vector(Number(1), BinaryOperation(Addition, Number(2), Number(3)), Number(4))), Number(2))))
	}

	it should "correctly parse a simple conditional" in {
		assert(parse("@if ($myVariable) 1 @else 2") ===
			Conditional(Variable("myVariable"), Number(1), Some(Number(2))))
		assert(parse("@if ($myVariable) 1") ===
			Conditional(Variable("myVariable"), Number(1), None))
	}

	it should "correctly parse a conditional spanning several liens" in {
		assert(parse(
			"""@if ($myVariable) 1
				|@else
				|	2
				|""".stripMargin) ===
			Conditional(Variable("myVariable"), Number(1), Some(Block(Number(2)))))
		assert(parse(
			"""@if ($myVariable)
				|	1
				|@else
				|	2
				|""".stripMargin) ===
			Conditional(Variable("myVariable"), Block(Number(1)), Some(Block(Number(2)))))
	}

	it should "correctly parse computed member access" in {
		assert(parse("$a[1 + 2]") === MemberAccess(Variable("a"), BinaryOperation(Addition, Number(1), Number(2))))
	}

	it should "correctly parse member access" in {
		assert(parse("$a.someMember") === MemberAccess(Variable("a"), String("someMember")))
		assert(parse("''.length") === MemberAccess(String(""), String("length")))
		assert(parse("123.456.foo") === MemberAccess(Number(123.456), String("foo")))
		assert(parse("1.2e3.bar") === MemberAccess(Number(1200), String("bar")))
		assert(parse("1.2e3.bar()") === FunctionCall(MemberAccess(Number(1200), String("bar"))))
	}

	it should "correctly parse anonymous functions" in {
		assert(parse("() => 123") === Term.Function(None, Vector(), Vector(), None, Block(Number(123))))
		assert(parse("(): Unit => 123") === Term.Function(None, Vector(), Vector(), Some(Type.Unit), Block(Number(123))))
		assert(parse("($a): Unit => 123") ===
			Term.Function(None, Vector(ValueSymbolDeclaration("a", None, None)), Vector(), Some(Type.Unit), Block(Number(123))))
		assert(parse("($a: Scalar): Unit => 123") ===
			Term.Function(None, Vector(ValueSymbolDeclaration("a", Some(Type.Scalar), ())), Vector(), Some(Type.Unit), Block(Number(123))))
		assert(parse("($a: Scalar = 1): Unit => 123") ===
			Term.Function(None, Vector(), Vector(ValueSymbolDeclaration("a", Some(Type.Scalar), Number(1))), Some(Type.Unit), Block(Number(123))))
		assert(parse(
			"""($a: Scalar = 1): Unit =>
				|	123
				|""".stripMargin) ===
			Term.Function(None, Vector(), Vector(ValueSymbolDeclaration("a", Some(Type.Scalar), Number(1))), Some(Type.Unit), Block(Number(123))))
		assert(parse("recursiveName ($a: Scalar = 1): Unit => 123") ===
			Term.Function(Some(ValueSymbol("recursiveName")), Vector(), Vector(ValueSymbolDeclaration("a", Some(Type.Scalar), Number(1))), Some(Type.Unit), Block(Number(123))))
	}

	it should "correctly parse complicated arithmetic expressions" in {
		assert(parse("2 * 3 ^ 4") === BinaryOperation(Multiplication, Number(2), BinaryOperation(Exponentiation, Number(3), Number(4))))
		assert(parse("(1 + 2) / 3") === BinaryOperation(Division, BinaryOperation(Addition, Number(1), Number(2)), Number(3)))
	}


	protected def parse(input: java.lang.String): Expression = parseLanguageRule(input, _.Expression)

}
