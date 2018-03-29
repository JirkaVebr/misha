package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Lambda, Native}
import com.mishaLang.error.ProgramError.{IllTypedArgument, NotEnoughArguments, ProgramErrorCode, TooManyArguments}
import com.mishaLang.interpreter.EnvWithValue

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
			val incorrectlyTyped = zipped.filter {
				case (expected, value) => TypeOps.canBeAssignedTo(value, expected).isEmpty
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


	def getLambdaApplicationError(lambda: Lambda, arguments: Vector[Value.Value])
															 (implicit state: EnvWithValue): Option[ProgramErrorCode] = {
		val mandatoryArity = lambda.mandatoryArguments.length
		val furtherArity = lambda.otherArguments.length
		val suppliedArgumentsCount = arguments.length

		if (suppliedArgumentsCount < mandatoryArity)
			Some(NotEnoughArguments)
		else if (suppliedArgumentsCount > mandatoryArity + furtherArity) {
			Some(TooManyArguments)
		} else {
			val zipped = (lambda.mandatoryArguments ++ lambda.otherArguments).zip(arguments)
			val incorrectlyTyped = zipped.filter {
				case (declaration, value) => declaration.typeAnnotation match {
					case Some(annotation) => TypeOps.canBeAssignedTo(value, annotation).isEmpty
					case None => false
				}
			}
			if (incorrectlyTyped.isEmpty) {
				None
			} else {
				Some(IllTypedArgument)
			}
		}
	}

}
