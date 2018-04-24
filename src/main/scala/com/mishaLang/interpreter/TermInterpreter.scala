package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.{Block, Expression}
import com.mishaLang.ast.Language.Statement.{Sequence, VariableDeclaration}
import com.mishaLang.ast.Language.Term._
import com.mishaLang.ast.Language.Value._
import com.mishaLang.ast.Language.{Term, Value, ValueSymbolDeclaration}
import com.mishaLang.ast.Selector._
import com.mishaLang.error.CompilerError
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.RuleContext.{AtRule, RuleSelector}
import com.mishaLang.interpreter.Symbol.RuleStoreSymbol
import com.mishaLang.interpreter.ops.{FunctionOps, TypeOps}

import scala.util.{Failure, Success, Try}

object TermInterpreter {

	def run(term: Term)(implicit state: EnvWithValue): Try[EnvWithValue] = term match {
		case value: Value => value match {
			case Value.Unit => state ~> Value.Unit
			case primitiveValue: Primitive => state ~> primitiveValue
			case compositeValue: Composite => compositeValue match {
				case tuple: Value.Tuple2 => runTupleValue(tuple)
				case list: Value.List => runListValue(list)
				case callable: Value.Callable => runCallableValue(callable)
				case formula: Value.Formula => ???
			}
		}
		case symbol: MagicSymbol => runMagicSymbol(symbol)
		case variable: Variable => runVariable(variable)
		case variable: PropertyVariable => runPropertyVariable(variable)
		case functionCall: FunctionCall => runFunctionCall(functionCall)
		case function: Term.Function => runFunctionTerm(function)
		case tuple: Term.Tuple2 => runTupleTerm(tuple)
		case list: Term.List => runListTerm(list)
		case memberAccess: MemberAccess => MemberAccessInterpreter.run(memberAccess)
	}


	def runParentSelector()(implicit state: EnvWithValue): Vector[Value.String] =
		state.environment.lookupContext() match {
			case Some(context) => context match {
				case RuleSelector(selector) => selector match {
					case SelectorList(selectors) =>
						selectors.map((s: NormalizedSelector) => {
							Value.String(s.toString)
						}).toVector
					case _ =>
						Vector(Value.String(selector.toString))
				}
				case _: AtRule => ???
			}
			case None => Vector()
		}


	private def runTupleValue(tuple: Value.Tuple2)(implicit state: EnvWithValue): Try[EnvWithValue] =
		state ~> tuple

	private def runTupleTerm(tuple: Term.Tuple2)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		Interpreter.chainRun[Expression](scala.List(tuple.first, tuple.second), state, ExpressionInterpreter.run(_)(_)) match {
			case Failure(reason) => Failure(reason)
			case Success(newEnvironment) =>
				val (second :: first :: Nil) = newEnvironment.value
				Success(EnvironmentWithValue(newEnvironment.environment, Value.Tuple2(first, second)))
		}
	}

	private def runMagicSymbol(symbol: MagicSymbol)(implicit state: EnvWithValue): Try[EnvWithValue] = symbol match {
		case ParentSelector => state ~> Value.List(runParentSelector())
	}

	private def runVariable(variable: Variable)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val variableValue = state.environment.lookup(variable.name)

		variableValue match {
			case Some(value) => state ~> value
			case None => state.fail(ReadingUndefinedVariable, variable)
		}
	}

	private def runPropertyVariable(variable: PropertyVariable)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		state.environment.lookupContext() match {
			case Some(ruleContext) =>
				state.environment.lookup(RuleStoreSymbol).get.get(ruleContext) match {
					case Some(propertyStore) =>
						propertyStore.get(variable.name) match {
							case Some(propertyRecords) =>
								if (propertyRecords.lengthCompare(1) == 0)
									state ~> propertyRecords.head.original
								else
									state ~> Value.List(propertyRecords.map(propertyRecord => propertyRecord.original).toVector)
							case None =>
								state.fail(UndefinedPropertyVariable, variable) // Undefined property
						}
					case None =>
						state.fail(UndefinedPropertyVariable, variable) // Empty rule
				}
			case None =>
				state.fail(UndefinedPropertyVariable, variable) // We aren't even in a rule
		}
	}

	private def runListValue(list: Value.List)(implicit state: EnvWithValue): Try[EnvWithValue] =
		state ~> list

	private def runListTerm(list: Term.List)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val chainResult = Interpreter.chainRun[Expression](list.items.toList, state, ExpressionInterpreter.run(_)(_))

		chainResult match {
			case Failure(exception) => Failure(exception)
			case Success(newEnvironment) =>
				runListValue(Value.List(newEnvironment.value.toVector.reverse))(EnvironmentWithValue(newEnvironment.environment))
		}
	}

	private def runCallableValue(callable: Value.Callable)(implicit state: EnvWithValue): Try[EnvWithValue] =
		// At this point the user has no way of creating a function value directly, and so we assume here that all its
		// possible forms are well-formed.
		state ~> callable

	private def runFunctionTerm(function: Term.Function)(implicit state: EnvWithValue): Try[EnvWithValue] =
		runCallableValue(Value.Lambda(
			function.recursiveName, function.mandatoryArguments.toVector, function.otherArguments.toVector,
			function.returnType, function.body, state.environment.meta.id
		))

	private def runFunctionCall(functionCall: Term.FunctionCall)(implicit state: EnvWithValue): Try[EnvWithValue] =
		ExpressionInterpreter.run(functionCall.function) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) => newState.value match {
				case function: Value.Callable =>
					Interpreter.chainRun[Expression](functionCall.arguments.toList, newState, ExpressionInterpreter.run(_)(_)) match {
						case Failure(exception) => Failure(exception)
						case Success(stateAfterArguments) =>
							val newestState = EnvironmentWithValue(stateAfterArguments.environment)
							val arguments = stateAfterArguments.value.toVector.reverse
							function match {
								case lambda: Lambda =>
									callLambda(lambda, functionCall, arguments)(newestState)
								case native: Native =>
									callNative(native, functionCall, arguments)(newestState)
								case group: PolymorphicGroup =>
									callPolymorphicGroup(group, functionCall, arguments)(newestState)
							}
					}
				case _ => newState.fail(InvokingANonFunction, functionCall)
			}
		}

	private def callNative(native: Native, functionCall: FunctionCall, arguments: Vector[Value])
												(implicit state: EnvWithValue): Try[EnvWithValue] = {
		FunctionOps.getNativeApplicationError(native, arguments) match {
			case Some(errorCode) => state.fail(errorCode, native, functionCall)
			case None => native.implementation(arguments) match {
				case Failure(exception) => Failure(exception)
				case Success(value) => state ~> value
			}
		}
	}

	private def callLambda(lambda: Lambda, functionCall: FunctionCall, arguments: Vector[Value])
												(implicit state: EnvWithValue): Try[EnvWithValue] = {
		FunctionOps.getLambdaApplicationError(lambda, arguments) match {
			case Some(errorCode) => state.fail(errorCode, lambda, functionCall)
			case None =>
				val expectedArguments = lambda.mandatoryArguments ++ lambda.otherArguments
				val supplied = arguments.zip(expectedArguments).map {
					case (argument, declaration) => VariableDeclaration(
						ValueSymbolDeclaration[Expression](declaration.name, declaration.typeAnnotation, argument)
					)
				}
				val notSupplied = lambda.otherArguments.takeRight(expectedArguments.length - arguments.length).map(VariableDeclaration)
				val firstExpression = lambda.body match {
					case Block(content) => content
					case _ => lambda.body
				}
				val recursiveDeclaration = lambda.recursiveName match {
					case Some(recursiveName) => Vector(
						VariableDeclaration(
							ValueSymbolDeclaration[Expression](recursiveName, None, lambda)
						)
					)
					case None => Vector()
				}
				val newBody = Sequence((recursiveDeclaration ++ supplied ++ notSupplied :+ firstExpression).toList)
				state.environment.getEnvironmentByScopeId(lambda.scopeId) match {
					case Some(environment) =>
						val subScope = state.environment.lookupContext() match {
							case Some(context) => environment.pushFunctionScope(context)
							case None => environment.pushFunctionScope()
						}
						subScope match {
							case Some(newLambdaScope) =>
								StatementInterpreter.run(newBody)(EnvironmentWithValue(newLambdaScope)) match {
									case Failure(exception) => Failure(exception)
									case Success(newState) =>
										newState.environment.getEnvironmentByScopeId(state.environment.meta.id) match {
											case Some(callSiteEnvironment) =>
												val callSiteState = EnvironmentWithValue(
													callSiteEnvironment, newState.value
												)
												lambda.returnType match {
													case Some(returnType) =>
														TypeOps.canBeAssignedTo(callSiteState.value, returnType) match {
															case Some(assignedValue) =>
																callSiteState ~> assignedValue
															case None =>
																callSiteState.fail(IllTypedReturn, lambda, functionCall)
														}
													case None => Success(callSiteState)
												}
											case None =>
												newState.failFatally(CompilerError("Undefined call site env"))
										}
								}
							case None =>
								state.fail(StackOverflow)
						}
					case None =>
						state.failFatally(CompilerError("Undefined lambda parent env"))
				}
		}
	}


	private def callPolymorphicGroup(group: PolymorphicGroup, functionCall: FunctionCall, arguments: Vector[Value])
																	(implicit state: EnvWithValue): Try[EnvWithValue] = {
		group.functions.find {
			function => FunctionOps.getFunctionApplicationError(function, arguments).isEmpty
		} match {
			case Some(function) => function match {
				case lambda: Lambda => callLambda(lambda, functionCall, arguments)
				case native: Native => callNative(native, functionCall, arguments)
			}
			case None =>
				// TODO this might be a bogus reason. Really, it should combine the aggregate results from the other places
				state.fail(IllTypedArgument)
		}
	}

}
