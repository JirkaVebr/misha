package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError.{DuplicateVariableDeclaration, TypeAnnotationMismatch}
import com.preprocessor.interpreter.typing.{Inference, Subtype}

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): Try[EvalState] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case TypeAliasDeclaration(alias, subType) => sys.error("todo") // TODO
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case FunctionDeclaration(name, typeAnnotation, value) => sys.error("todo") // TODO
		case Rule(head, body) => sys.error("todo") // TODO

		case expression: Expression => ExpressionInterpreter.run(expression)
	}

	private def runSequence(sequence: Sequence)(implicit state: EvalState): Try[EvalState] =
		Interpreter.chainRun[Statement](List(sequence.current, sequence.following), state, run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success((_, finalState)) => Success(finalState)
		}

	private def runVariableDeclaration(varDeclaration: VariableDeclaration)(implicit state: EvalState): Try[EvalState] = {
		if (state.environment.isInCurrentScope(varDeclaration.name))
			state.fail(DuplicateVariableDeclaration, varDeclaration)
		else ExpressionInterpreter.run(varDeclaration.value) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterValue) => varDeclaration.typeAnnotation match {
				case Some(annotatedType) =>
					if (Subtype.isSubtypeOf(stateAfterValue.valueRecord.recordType, annotatedType))
						stateAfterValue.withUpdatedValue(
							varDeclaration.name,
							ValueRecord(stateAfterValue.valueRecord.value, annotatedType)
						)
					else
						stateAfterValue.fail(TypeAnnotationMismatch, varDeclaration)
				case None =>
					stateAfterValue.withUpdatedValue(
						varDeclaration.name,
						ValueRecord(stateAfterValue.valueRecord.value, Inference.inferTypeForValue(stateAfterValue.valueRecord.value))
					)
			}
		}
	}
}
