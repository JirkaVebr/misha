package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.Expression
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.{Statement, Value}
import com.preprocessor.ast.Symbol.PropertySymbol
import com.preprocessor.ast.{PropertyRecord, ValueRecord}
import com.preprocessor.error.ProgramError._
import com.preprocessor.interpreter.ops.StringOps
import com.preprocessor.interpreter.typing.{Inference, Subtype}

import scala.util.{Failure, Success, Try}

object StatementInterpreter {

	def run(statement: Statement)(implicit state: EvalState): Try[EvalState] = statement match {
		case sequence: Sequence => runSequence(sequence)
		case typeAlias: TypeAliasDeclaration => runTypeAliasDeclaration(typeAlias)
		case variableDeclaration: VariableDeclaration => runVariableDeclaration(variableDeclaration)
		case Rule(head, body) => sys.error("todo") // TODO
		case property: Statement.Property => runProperty(property)

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

	private def runProperty(property: Statement.Property)(implicit state: EvalState): Try[EvalState] =
		ExpressionInterpreter.run(property.name) match {
			case Failure(reason) => Failure(reason)
			case Success(stateAfterName) => stateAfterName.valueRecord.value match {
				case Value.String(name) => ExpressionInterpreter.run(property.value)(stateAfterName) match {
					case Failure(reason) => Failure(reason)
					case Success(stateAfterValue) => StringOps.castToString(stateAfterValue.valueRecord.value) match {
						case Some(valueString) =>
							val property = PropertyRecord(name, valueString.value, Set.empty) // TODO actually use flags

							stateAfterValue.withNewSymbol(PropertySymbol)(
								property :: (stateAfterValue.environment.lookupCurrent(PropertySymbol) match {
									case Some(properties) => properties
									case None => List.empty
								})
							)
						case None => stateAfterName.fail(IllegalPropertyValue, property.name)
					}
				}
				case _ => stateAfterName.fail(NonStringPropertyName, property.name)
			}
		}

}
