package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Term._
import com.preprocessor.ast.Language.Value._
import com.preprocessor.ast.Language.{Term, Value}
import com.preprocessor.ast.Selector._
import com.preprocessor.error.ProgramError.{InvokingANonFunction, NotEnoughArguments, ReadingUndefinedVariable}
import com.preprocessor.interpreter.RuleContext.{AtRule, RuleSelector}

import scala.util.{Failure, Success, Try}

object TermInterpreter {

	def run(term: Term)(implicit state: EnvWithValue): Try[EnvWithValue] = term match {
		case value: Value => value match {
			case Value.Unit => state ~> Value.Unit
			case primitiveValue: Primitive => state ~> primitiveValue
			case compositeValue: Composite => compositeValue match {
				case tuple: Value.Tuple2 => runTupleValue(tuple)
				case list: Value.List => runListValue(list)
				case function: Value.Function => runFunctionValue(function)
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
		case ParentSelector => state.environment.lookupContext() match {
			case Some(context) => context match {
				case RuleSelector(selector) => selector match {
					case SelectorList(selectors) =>
						state ~> Value.List(selectors.map((s: NormalizedSelector) => {
							Value.String(s.toString)
						}).toList)
					case _ =>
						state ~> Value.List(scala.List(Value.String(selector.toString)))
				}
				case _: AtRule => ???
			}
			case None => state ~> Value.List(scala.List())
		}
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

	private def runFunctionValue(function: Value.Function)(implicit state: EnvWithValue): Try[EnvWithValue] =
		// At this point the user has no way of creating a function value directly, and so we assume here that all its
		// possible forms are well-formed.
		state ~> function

	private def runFunctionTerm(function: Term.Function)(implicit state: EnvWithValue): Try[EnvWithValue] =
		runFunctionValue(Value.Lambda( // TODO check default argument positions
			function.mandatoryArguments, function.otherArguments, function.returnType, function.body, state.environment
		))

	private def runFunctionCall(functionCall: Term.FunctionCall)(implicit state: EnvWithValue): Try[EnvWithValue] =
		ExpressionInterpreter.run(functionCall.function) match {
			case Failure(exception) => Failure(exception)
			case Success(newState) => newState.value match {
				case function: Value.Function => function match {
					case lambda: Lambda => callLambda(lambda, functionCall)
					case PolymorphicGroup(lambdas) => ???
				}
				case _ => newState.fail(InvokingANonFunction, functionCall)
			}
		}

	private def callLambda(lambda: Lambda, functionCall: FunctionCall)(implicit state: EnvWithValue): Try[EnvWithValue] = {
		val lambdaMandatoryArity = lambda.mandatoryArguments.length
		val lambdaFurtherArity = lambda.otherArguments.length
		val suppliedArgumentsCount = functionCall.arguments.length

		if (suppliedArgumentsCount < lambdaMandatoryArity) state.fail(NotEnoughArguments, lambda, functionCall)
		else {
			???
		}
	}

}
