package com.preprocessor.parser

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast._
import org.parboiled2._


trait L3_Expressions { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics
	with L1_Literals
	with L2_Types =>


	def Expression: Rule1[Expression] = rule {
		addition | conditional
	}

	def addition: Rule1[Expression] = rule {
		multiplication ~ zeroOrMore(
			("+" ~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Addition, l, r))) |
			("-" ~ multiplication ~> ((l: Expression, r: Expression) => BinaryOperation(Subtraction, l, r)))
		)
	}

	def multiplication: Rule1[Expression] = rule {
		factor ~ zeroOrMore(
			("*" ~ factor ~> ((l: Expression, r: Expression) => BinaryOperation(Multiplication, l, r))) |
			("/" ~ factor ~> ((l: Expression, r: Expression) => BinaryOperation(Division, l, r)))
		)
	}

	def factor: Rule1[Expression] = rule {
		term | subExpression | arithmeticNegation // | exponentiation
	}

	def term: Rule1[Term.Term] = rule {
		boolean | functionCall | number | variable | delimitedList
	}

	def arithmeticNegation: Rule1[UnaryOperation] = rule {
		("-" ~ Expression) ~> ((e: Expression) => UnaryOperation(ArithmeticNegation, e))
	}

	// TODO remove the left-recursion
	def exponentiation: Rule1[BinaryOperation] = rule {
		(Expression ~ "^" ~ Expression) ~> ((l: Expression, r: Expression) => BinaryOperation(Exponentiation, l, r))
	}

	def number: Rule1[Term.Number] = rule {
		(digits ~ whitespace) ~> Term.Number
	}

	def digits: Rule1[Double] = rule {
		capture(CharPredicate.Digit19 ~ zeroOrMore(CharPredicate.Digit)) ~> ((numeric: String) => numeric.toDouble)
	}

	def variable: Rule1[Term.Variable] = rule {
		// Deliberately using $ as a char and not a string as not to allow whitespace there
		'$' ~ variableName ~> Term.Variable
	}

	def boolean: Rule1[Term.Boolean] = rule {
		((capture("true") | capture("false")) ~ whitespace) ~> ((literal: String) => Term.Boolean(literal == "true"))
	}


	def delimitedList: Rule1[Term.List] = rule {
		("[" ~ listBody ~ "]" | "(" ~ listBody ~ ")") ~> Term.List
	}

	def listBody: Rule1[Seq[Expression]] = rule {
		zeroOrMore(Expression).separatedBy(whitespace) ~ whitespace
	}

	//	def string: Rule1[Expression.StringTerm] = rule {
	//		quotedString
	//	}
	//
	//	def quotedString: Rule1[Expression.StringTerm] = rule {
	//		"\"" ~ capture("d") ~> Expression.StringTerm
	//	}

	def variableName: Rule1[java.lang.String] = rule {
		capture(oneOrMore(CharPredicate.AlphaNum)) ~ whitespace
	}

	def functionCall: Rule1[Term.FunctionCall] = rule {
		functionName ~ "(" ~ zeroOrMore(Expression).separatedBy(",") ~ optional(",") ~ ")" ~> Term.FunctionCall
	}

	def functionName: Rule1[Expression] = rule {
		subExpression | variable | (variableName ~> Term.Variable)
	}

	private def subExpression: Rule1[SubExpression] = rule {
		"(" ~ Expression ~ ")" ~> SubExpression
	}

	private def conditional: Rule1[Conditional] = rule {
		("@if" ~ subExpression ~ Expression ~ optional("@else" ~ Expression)) ~> Conditional
	}
}
