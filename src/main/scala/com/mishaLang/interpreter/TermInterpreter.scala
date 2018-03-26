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
import com.mishaLang.interpreter.ops.FunctionOps
import com.mishaLang.interpreter.typing.{Subtype, Typing}

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
				val (first :: second :: Nil) = newEnvironment.value
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

	private def runListValue(list: Value.List)(implicit state: EnvWithValue): Try[EnvWithValue] =
		state ~> list

	private def runListTerm(list: Term.List)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val chainResult = Interpreter.chainRun[Expression](list.items.toList, state, ExpressionInterpreter.run(_)(_))

		chainResult match {
			case Failure(exception) => Failure(exception)
			case Success(newEnvironment) =>
				runListValue(Value.List(newEnvironment.value.toVector))(EnvironmentWithValue(newEnvironment.environment))
		}
	}

	private def runCallableValue(callable: Value.Callable)(implicit state: EnvWithValue): Try[EnvWithValue] =
		// At this point the user has no way of creating a function value directly, and so we assume here that all its
		// possible forms are well-formed.
		state ~> callable

	private def runFunctionTerm(function: Term.Function)(implicit state: EnvWithValue): Try[EnvWithValue] =
		runCallableValue(Value.Lambda(
			function.recursiveName, function.mandatoryArguments.toVector, function.otherArguments.toVector,
			function.returnType, function.body, state.environment.scopeId
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
							function match {
								case lambda: Lambda =>
									callLambda(lambda, functionCall, stateAfterArguments.value.toVector)(newestState)
								case native: Native =>
									callNative(native, functionCall, stateAfterArguments.value.toVector)(newestState)
								case PolymorphicGroup(lambdas) => ???
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
				val newBody = (recursiveDeclaration ++ supplied ++ notSupplied).foldRight(firstExpression) {
					case (declaration, accumulator) => Sequence(declaration, accumulator)
				}
				state.environment.getEnvironmentByScopeId(lambda.scopeId) match {
					case Some(environment) =>
						val subScope = state.environment.lookupContext() match {
							case Some(context) => environment.pushSubScope(context)
							case None => environment.pushSubScope()
						}
						subScope match {
							case Some(newLambdaScope) =>
								StatementInterpreter.run(newBody)(EnvironmentWithValue(newLambdaScope)) match {
									case Failure(exception) => Failure(exception)
									case Success(newState) =>
										newState.environment.getEnvironmentByScopeId(state.environment.scopeId) match {
											case Some(callSiteEnvironment) =>
												val callSiteState = EnvironmentWithValue(
													callSiteEnvironment, newState.value
												)
												lambda.returnType match {
													case Some(returnType) =>
														if (Typing.canBeAssignedTo(callSiteState.value, returnType))
															Success(callSiteState)
														else
															callSiteState.fail(IllTypedReturn, lambda, functionCall)
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

}
