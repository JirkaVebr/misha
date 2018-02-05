package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term._
import com.preprocessor.ast.Ast._
import org.parboiled2._

trait Expr
case class Num(n: Int) extends Expr
case class BinLop(l: Expr, r: Expr) extends Expr
case class BinRop1(l: Expr, r: Expr) extends Expr
case class BinRop2(l: Expr, r: Expr) extends Expr

trait L3_Expressions { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics
	with L1_Literals
	with L2_Types =>

	/*def AssociativityTest: Rule1[Expr] = rule {
		rightAssoc
	}

	/*def leftAssoc: Rule1[Expr] = rule {
		num ~ zeroOrMore(
			"lop" ~ num ~> BinLop
		)
	}*/

	def rightAssoc: Rule1[Expr] = rule {
		num ~ zeroOrMore(
			"rop1" ~ rightAssoc ~> BinRop1 |
			"rop2" ~ rightAssoc ~> BinRop2
		)
	}

	def num: Rule1[Num] = rule {
		(capture(oneOrMore(CharPredicate.Digit)) ~ whitespace) ~> ((a: String) => Num(Integer.parseInt(a)))
	}*/


	def Expression: Rule1[Expression] = rule {
		assignment
	}

	private def assignment: Rule1[Expression] = rule { // Right associative
		logical ~ zeroOrMore(
			"="  ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, r)) |
			"+=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Addition, l, r))) |
			"-=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Subtraction, l, r))) |
			"*=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Multiplication, l, r))) |
			"/=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Division, l, r))) |
			"^=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Exponentiation, l, r))) |
			"%=" ~ assignment ~> ((l: Expression, r: Expression) => BinaryOperation(Equals, l, BinaryOperation(Remainder, l, r)))
		)
	}

	private def logical: Rule1[Expression] = rule { // Left associative
		equalityComparison ~ zeroOrMore(
			("&&" ~ equalityComparison ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalAnd, l, r))) |
			("||" ~ equalityComparison ~> ((l: Expression, r: Expression) => BinaryOperation(LogicalOr, l, r)))
		)
	}

	private def equalityComparison: Rule1[Expression] = rule { // Left associative
		otherComparison ~ zeroOrMore(
			("==" ~ otherComparison ~> ((l: Expression, r: Expression) => BinaryOperation(EqualTo, l, r))) |
			("!=" ~ otherComparison ~> ((l: Expression, r: Expression) =>
				UnaryOperation(LogicalNegation, BinaryOperation(EqualTo, l, r))))
		)
	}

	private def otherComparison: Rule1[Expression] = rule { // Left associative
		addition ~ zeroOrMore(
			("<=" ~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerEquals, l, r))) |
			("<" ~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(LowerEquals, l, r))) |
			(">=" ~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterEquals, l, r))) |
			(">" ~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(GreaterThan, l, r))) |
			("@in" ~ addition ~> ((l: Expression, r: Expression) => BinaryOperation(In, l, r)))
		)
	}

	private def addition: Rule1[Expression] = rule { // Left associative
		multiplication ~ zeroOrMore(
			("+" ~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Addition, l, r))) |
			("-" ~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Subtraction, l, r)))
		)
	}

	private def multiplication: Rule1[Expression] = rule { // Left associative
		exponentiation ~ zeroOrMore(
			("*" ~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Multiplication, l, r))) |
			("/" ~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Division, l, r))) |
			("%" ~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Remainder, l, r)))
		)
	}

	private def exponentiation: Rule1[Expression] = rule { // Right associative
		factor ~ zeroOrMore(
			"^" ~ exponentiation ~> ((l: Expression, r: Expression) => BinaryOperation(Exponentiation, l, r))
		)
	}

	private def factor: Rule1[Expression] = rule {
		functionCall | conditional | delimitedList | unaryOperation | variable | subExpression | Literal
	}

	private def unaryOperation: Rule1[UnaryOperation] = rule {
		("-" ~ Expression ~> ((e: Expression) => UnaryOperation(ArithmeticNegation, e))) |
		("!" ~ Expression ~> ((e: Expression) => UnaryOperation(LogicalNegation, e)))
	}

	private def variable: Rule1[Term.Variable] = rule {
		// Deliberately using $ as a char and not a string as not to allow whitespace there
		'$' ~ variableName ~> Term.Variable
	}

	private def delimitedList: Rule1[Term.List] = rule {
		("[" ~ listBody ~ "]" | "(" ~ listBody ~ ")") ~> Term.List
	}

	private def listBody: Rule1[Seq[Expression]] = rule {
		zeroOrMore(Expression).separatedBy(whitespace) ~ whitespace
	}

	private def variableName: Rule1[java.lang.String] = rule {
		capture(oneOrMore(CharPredicate.AlphaNum)) ~ whitespace
	}

	private def functionCall: Rule1[FunctionCall] = rule {
		// The error is just IntelliJ being dumb.
		(functionName ~ "(" ~ zeroOrMore(Expression).separatedBy(",") ~ optional(",") ~ ")") ~> (
			(function: Expression, arguments: Seq[Expression]) => FunctionCall(function, arguments)
		)
	}

	private def functionName: Rule1[Expression] = rule {
		subExpression | variable | (variableName ~> Term.Variable)
	}

	private def subExpression: Rule1[Expression] = rule {
		"(" ~ Expression ~ ")"
	}

	private def conditional: Rule1[Conditional] = rule {
		// The error is just IntelliJ being dumb.
		("@if" ~ subExpression ~ Expression ~ optional("@else" ~ Expression)) ~> (
			(condition: Expression, consequent: Expression, alternative: Option[Expression]) => Conditional(
				condition, consequent, alternative
			)
		)
	}
}
