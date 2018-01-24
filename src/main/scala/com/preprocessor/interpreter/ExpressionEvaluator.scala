package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value._
import com.preprocessor.ast.Ast._
import com.preprocessor.interpreter.operators.{Adder, Multiplier}

object ExpressionEvaluator {

	def evaluate(expression: Expression): Value = expression match {
		/*case ScalarTerm(n) => NumberValue(n)
		case BooleanTerm(b) => BooleanValue(b)
		case StringTerm(s) => StringValue(s)
		case SubExpression(e) => evaluate(e)

		case AdditionExpression(l, r) => Adder.add(evaluate(l), evaluate(r))
		case SubtractionExpression(l, r) => Adder.subtract(evaluate(l), evaluate(r))

		case MultiplicationExpression(l, r) => Multiplier.multiply(evaluate(l), evaluate(r))
		case DivisionExpression(l, r) => Multiplier.divide(evaluate(l), evaluate(r))*/

		case _ => sys.error("todo")
	}
}
