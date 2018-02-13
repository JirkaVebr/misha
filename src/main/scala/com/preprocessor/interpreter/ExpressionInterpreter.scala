package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Term.Term

import scala.util.Try

object ExpressionInterpreter {

	def run(expression: Expression)(implicit state: EvalState): Try[EvalState] = expression match {
		case BinaryOperation(operator, left, right) => sys.error("todo") // TODO
		case UnaryOperation(operator, value) => sys.error("todo") // TODO
		case Conditional(condition, consequent, alternative) => sys.error("todo") // TODO
		case StringInterpolation(components) => sys.error("todo") // TODO
		case term: Term => TermInterpreter.run(term)
	}
}
