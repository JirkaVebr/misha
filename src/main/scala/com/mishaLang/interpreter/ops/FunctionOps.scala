package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.ast.Language.Value.{Lambda, Native}
import com.mishaLang.error.ProgramError.{IllTypedArgument, NotEnoughArguments, ProgramErrorCode, TooManyArguments}
import com.mishaLang.interpreter.typing.{Subtype, Typing}

object FunctionOps {

	/**
		* Return value of None means that the function can be called
		*/
	def getFunctionApplicationError(function: Value.Function, argVector: Vector[Value.Value]): Option[ProgramErrorCode] =
		function match {
			case Lambda(mandatoryArguments, otherArguments, returnType, body, scopeId) => ???
			case Native(expectedType, _) =>
				getNativeApplicationError(expectedType, argVector)
		}

	def getNativeApplicationError(expectedType: Vector[Type.Any], argVector: Vector[Value.Value]): Option[ProgramErrorCode] = {
		val arityComparison = argVector.length - expectedType.length

		if (arityComparison == 0) {
			val zipped = expectedType.zip(argVector)
			val incorrectlyTyped = zipped.filterNot{
				case (expected, value) => Subtype.isSubtypeOf(Typing.getType(value), expected)
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

}
