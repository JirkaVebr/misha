package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Lambda, Native}
import com.mishaLang.error.ProgramError.{IllTypedArgument, NotEnoughArguments, ProgramErrorCode, TooManyArguments}
import com.mishaLang.interpreter.EnvWithValue
import com.mishaLang.interpreter.typing.Typing

object FunctionOps {

	/**
		* Return value of None means that the function can be called
		*/
	def getFunctionApplicationError(function: Value.Function, arguments: Vector[Value.Value])
																 (implicit state: EnvWithValue): Option[ProgramErrorCode] =
		function match {
			case lambda: Lambda =>
				getLambdaApplicationError(lambda, arguments)
			case native: Native =>
				getNativeApplicationError(native, arguments)
		}


	def getNativeApplicationError(native: Native, arguments: Vector[Value.Value])
															 (implicit state: EnvWithValue): Option[ProgramErrorCode] = {
		val arityComparison = arguments.length - native.expectedType.length

		if (arityComparison == 0) {
			val zipped = native.expectedType.zip(arguments)
			val incorrectlyTyped = zipped.filterNot{
				case (expected, value) => Typing.canBeAssignedTo(value, expected)
			}
			if (incorrectlyTyped.isEmpty) {
				None
			} else {
				Some(IllTypedArgument)
			}
		} else if (arityComparison < 0)
			Some(NotEnoughArguments)
		else
			Some(TooManyArguments)
	}


	def getLambdaApplicationError(lambda: Lambda, arguments: Vector[Value.Value]): Option[ProgramErrorCode] = {
		/*val lambdaMandatoryArity = lambda.mandatoryArguments.length
		val lambdaFurtherArity = lambda.otherArguments.length
		val suppliedArgumentsCount = arguments.length

		if (suppliedArgumentsCount < lambdaMandatoryArity)
			Some(NotEnoughArguments)
		else {
			???
		}*/
		None // TODO
	}

}
