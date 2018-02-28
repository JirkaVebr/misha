package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError.{DuplicateVariableDeclaration, NonNarrowingTypeAlias, TypeAnnotationMismatch}
import com.preprocessor.interpreter.typing.{Inference, Subtype}

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): Try[EvalState] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case typeAlias: TypeAliasDeclaration => runTypeAliasDeclaration(typeAlias)
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case Rule(head, body) => sys.error("todo") // TODO

		case expression: Expression => ExpressionInterpreter.run(expression)
	}

	private def runSequence(sequence: Sequence)(implicit state: EvalState): Try[EvalState] =
		Interpreter.chainRun[Statement](List(sequence.current, sequence.following), state, run(_)(_)) match {
			case Failure(exception) => Failure(exception)
			case Success((_, finalState)) => Success(finalState)
		}

	private def runTypeAliasDeclaration(typeAlias: TypeAliasDeclaration)(implicit state: EvalState): Try[EvalState] = {
		val existingType = state.environment.lookup(typeAlias.alias.name)

		if (existingType.nonEmpty && !Subtype.isSubtypeOf(typeAlias.subType, existingType.get)) {
			state.fail(NonNarrowingTypeAlias, typeAlias)
		} else {
			state.withNewSymbol(typeAlias.alias.name)(typeAlias.subType)
		}
	}

	private def runVariableDeclaration(varDeclaration: VariableDeclaration)(implicit state: EvalState): Try[EvalState] = {
		val declaration = varDeclaration.declaration
		if (state.environment.isInCurrentScope(declaration.name))
			state.fail(DuplicateVariableDeclaration, varDeclaration)
		else ExpressionInterpreter.run(declaration.value) match {
			case Failure(exception) => Failure(exception)
			case Success(stateAfterValue) => declaration.typeAnnotation match {
				case Some(annotatedType) =>
					if (Subtype.isSubtypeOf(stateAfterValue.valueRecord.recordType, annotatedType))
						stateAfterValue.withNewSymbol(declaration.name)(
							ValueRecord(stateAfterValue.valueRecord.value, annotatedType)
						)
					else
						stateAfterValue.fail(TypeAnnotationMismatch, varDeclaration)
				case None =>
					stateAfterValue.withNewSymbol(declaration.name)(
						ValueRecord(stateAfterValue.valueRecord.value, Inference.inferTypeForValue(stateAfterValue.valueRecord.value))
					)
			}
		}
	}
}
