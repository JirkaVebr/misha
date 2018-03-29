package com.mishaLang.error

import com.mishaLang.ast.Language
import com.mishaLang.interpreter.AugmentedEnvironment

class ProgramError[T](val errorCode: ProgramError.ProgramErrorCode, val augmentedEnvironment: AugmentedEnvironment[T],
											val nodes: Language.Node*) extends Error


object ProgramError {
	sealed trait ProgramErrorCode {
		def message(nodes: Language.Node*): String
	}

	abstract class SimpleError(val message: String) extends ProgramErrorCode {
		override def message(nodes: Language.Node*): String = message
	}

	def apply[T](errorCode: ProgramError.ProgramErrorCode, augmentedEnvironment: AugmentedEnvironment[T], nodes: Language.Node*): ProgramError[T] =
		new ProgramError[T](errorCode, augmentedEnvironment, nodes: _*)

	case object AssigningToNonVariable extends SimpleError("Assignment to a non-variable")
	case object ComparingIncompatibleNumerics extends SimpleError("Numerical comparison of incompatible numerical values")
	case object ComparingNonNumber extends SimpleError("Numerical comparison of non-numerical values")
	case object DuplicateProperty extends SimpleError("Duplicate property. Use !duplicate if necessary.")
	case object DuplicateVariableDeclaration extends SimpleError("Duplicate declaration of a variable within the same scope")
	case object IllTypedArgument extends SimpleError("Type of argument value does not conform to the expected type")
	case object IllTypedAssignment extends SimpleError("Type of assignment value does not conform to the declared variable type")
	case object IllTypedFlagList extends SimpleError("The last argument of @property must be a list of flags")
	case object IllTypedReturn extends SimpleError("The function return value does not match its type annotation")
	case object IllegalListConcatenationOperand extends SimpleError("Concatenating a list with a non-list")
	case object IllegalNumericOperatorOperand extends SimpleError("Applying a numeric operator to an illegal type")
	case object IllegalPropertyValue extends SimpleError("Property value cannot be cast to string")
	case object IllegalStringConcatenationOperand extends SimpleError("Concatenation of a value that is not convertible to a string")
	case object InvokingANonFunction extends SimpleError("Invoking a non-function expression")
	case object LogicOnNonBooleans extends SimpleError("Attempting logical operations on non-boolean values")
	case object MultiplyingStringByNonInt extends SimpleError("Multiplying a string by a non-integer value")
	case object MultiplyingStringByNonScalar extends SimpleError("Multiplying a string by a non-scalar numeric value")
	case object MultiplyingStringByNonNumber extends SimpleError("Multiplying a string by a non-numeric value")
	case object NegatingNonBoolean extends SimpleError("Logical negation of a non-boolean")
	case object NegatingNonNumeric extends SimpleError("Arithmetic negation of a non-numeric")
	case object NonBooleanCondition extends SimpleError("Non-boolean condition value")
	case object NonIntScalarListRepeat extends SimpleError("Repeating a list with a factor that isn't a scalar integer")
	case object NonListIterable extends SimpleError("Iterating over a non-list value")
	case object NonNarrowingTypeAlias extends SimpleError("New value for an existing type name is not a subtype of the existing value")
	case object NonStringMemberCastFail extends SimpleError("Non string member name cannot be cast to string")
	case object NonStringPropertyName extends SimpleError("The name of a property must be a string")
	case object NonStringSelectorExpression extends SimpleError("Expression used as a part of a selector cannot be cast to string")
	case object NotEnoughArguments extends SimpleError("Not enough arguments for function")
	case object PropertyOutsideARule extends SimpleError("Using property outside a rule")
	case object ReadingUndefinedVariable extends SimpleError("Reading of an undefined variable")
	case object StackOverflow extends SimpleError("Stack overflow")
	case object TooManyArguments extends SimpleError("Too many arguments for function")
	case object TypeAnnotationMismatch extends SimpleError("Assigned value doesn't match the declared type annotation")
	case object UndefinedType extends SimpleError("Using an undefined type")
	case object UndefinedPropertyVariable extends SimpleError("Undefined property variable")
	case object WritingUninitializedVariable extends SimpleError("Writing to an uninitialized variable")

	case class UnexpectedType(expected: Language.Type.Any, actual: Language.Type.Any) extends ProgramErrorCode {
		override def message(nodes: Language.Node*): String = ???
	}
}
