package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Statement._

import scala.util.{Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): Try[EvalState] = statement match {
		case NoOp => Success(state)
		case Sequence(current, following) =>
			//run(following)(run(current))
			sys.error("todo")

		case TypeAliasDeclaration(alias, subType) => sys.error("todo") // TODO
		case VariableDeclaration(name, typeAnnotation, value) => sys.error("todo") // TODO
		case FunctionDeclaration(name, typeAnnotation, value) => sys.error("todo") // TODO
		case Rule(head, body) => sys.error("todo") // TODO

		case expression: Expression => ExpressionInterpreter.run(expression)
	}
}
