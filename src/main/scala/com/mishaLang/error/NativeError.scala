package com.mishaLang.error

class NativeError(val errorCode: NativeError.NativeErrorCode) extends Error


object NativeError {

	def apply(errorCode: NativeErrorCode): NativeError =
		new NativeError(errorCode)

	sealed trait NativeErrorCode {
		def message(): String
	}

	abstract class SimpleError(val errorMessage: String) extends NativeErrorCode {
		override def message(): String = errorMessage
	}


	case object StringIndexOutOfBounds extends SimpleError("Invalid index passed to charAt")

	case class ArgumentIsNotInteger(variable: String)
		extends SimpleError("Argument " + variable + " is not integer as it should") {
	}

	case class ArgumentIsOutOfBounds(variable: String)
		extends SimpleError("Argument " + variable + " is out of bounds") {
	}
}
