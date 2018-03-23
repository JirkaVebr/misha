package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.ast.Language.Term._
import com.mishaLang.ast.Language.Value._
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.ast.Selector._
import com.mishaLang.error.ProgramError._
import com.mishaLang.interpreter.RuleContext.{AtRule, RuleSelector}
import com.mishaLang.interpreter.ops.FunctionOps

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
		case ParentSelector => state ~> Value.List(runParentSelector().toList)
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
				runListValue(Value.List(newEnvironment.value))(EnvironmentWithValue(newEnvironment.environment))
		}
	}

	private def runCallableValue(callable: Value.Callable)(implicit state: EnvWithValue): Try[EnvWithValue] =
		// At this point the user has no way of creating a function value directly, and so we assume here that all its
		// possible forms are well-formed.
		state ~> callable

	private def runFunctionTerm(function: Term.Function)(implicit state: EnvWithValue): Try[EnvWithValue] =
		runCallableValue(Value.Lambda(
			function.mandatoryArguments.toVector, function.otherArguments.toVector, function.returnType,
			function.body, state.environment.scopeId
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
								case lambda: Lambda => callLambda(lambda, functionCall, stateAfterArguments.value)(newestState)
								case native: Native => callNative(native, functionCall, stateAfterArguments.value.toVector)(newestState)
								case PolymorphicGroup(lambdas) => ???
							}
					}
				case _ => newState.fail(InvokingANonFunction, functionCall)
			}
		}

	private def callNative(native: Native, functionCall: FunctionCall, arguments: Vector[Value])
												(implicit state: EnvWithValue): Try[EnvWithValue] = {
		FunctionOps.getNativeApplicationError(native.expectedType, arguments) match {
			case Some(errorCode) => state.fail(errorCode, native, functionCall)
			case None => native.implementation(arguments) match {
				case Failure(exception) => Failure(exception)
				case Success(value) => state ~> value
			}
		}
	}

	private def callLambda(lambda: Lambda, functionCall: FunctionCall, arguments: scala.List[Value])
												(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val lambdaMandatoryArity = lambda.mandatoryArguments.length
		val lambdaFurtherArity = lambda.otherArguments.length
		val suppliedArgumentsCount = functionCall.arguments.length

		if (suppliedArgumentsCount < lambdaMandatoryArity) state.fail(NotEnoughArguments, lambda, functionCall)
		else {
			???
		}
	}

}
