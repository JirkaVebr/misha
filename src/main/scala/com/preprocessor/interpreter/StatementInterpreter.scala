package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Statement._

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): EvalState = statement match {
		case NoOp => state
		case Sequence(current, following) => run(following)(run(current))

		case TypeAliasDeclaration(alias, subType) => sys.error("todo") // TODO
		case VariableDeclaration(name, typeAnnotation, value) => sys.error("todo") // TODO
		case FunctionDeclaration(name, typeAnnotation, value) => sys.error("todo") // TODO
		case Rule(head, body) => sys.error("todo") // TODO

		case expression: Expression => ExpressionInterpreter.run(expression)
	}
}
