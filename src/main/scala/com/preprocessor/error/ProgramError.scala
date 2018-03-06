package com.preprocessor.error

import com.preprocessor.ast.Language
import com.preprocessor.interpreter.EvalState

class ProgramError(val errorCode: ProgramError.ProgramErrorCode, val evalState: EvalState, val nodes: Language.Node*) extends Error


object ProgramError {
	sealed trait ProgramErrorCode {
		def message(nodes: Language.Node*): String
	}

	abstract class SimpleError(val message: String) extends ProgramErrorCode {
		override def message(nodes: Language.Node*): String = message
	}

	def apply(errorCode: ProgramError.ProgramErrorCode, evalState: EvalState, nodes: Language.Node*): ProgramError =
		new ProgramError(errorCode, evalState, nodes: _*)

	case object AssigningToNonVariable extends SimpleError("Assignment to a non-variable")
	case object ComparingIncompatibleNumerics extends SimpleError("Numerical comparison of incompatible numerical values")
	case object ComparingNonNumber extends SimpleError("Numerical comparison of non-numerical values")
	case object ConcatenatingIllegalOperand extends SimpleError("Concatenation of a value that is not convertible to a string")
	case object DuplicateVariableDeclaration extends SimpleError("Duplicate declaration of a variable within the same scope")
	case object IllegalNumericOperatorOperand extends SimpleError("Applying a numeric operator to an illegal type")
	case object IllegalPropertyValue extends SimpleError("Property value cannot be cast to string")
	case object IllTypedAssignment extends SimpleError("Type of assignment value does not conform to the declared variable type")
	case object IllTypedConditionBranches extends SimpleError("Miss-matched types of branches of a conditional expression")
	case object LogicOnNonBooleans extends SimpleError("Attempting logical operations on non-boolean values")
	case object MultiplyingStringByNonInt extends SimpleError("Multiplying a string by a non-integer value")
	case object MultiplyingStringByNonScalar extends SimpleError("Multiplying a string by a non-scalar numeric value")
	case object NegatingNonBoolean extends SimpleError("Logical negation of a non-boolean")
	case object NegatingNonNumeric extends SimpleError("Arithmetic negation of a non-numeric")
	case object NonBooleanCondition extends SimpleError("Non-boolean condition value")
	case object NonNarrowingTypeAlias extends SimpleError("New value for an existing type name is not a subtype of the existing value")
	case object NonStringPropertyName extends SimpleError("The name of a property must be a string")
	case object ReadingUndefinedVariable extends SimpleError("Reading of an undefined variable")
	case object TypeAnnotationMismatch extends SimpleError("Assigned value doesn't match the declared type annotation")
	case object WritingUninitializedVariable extends SimpleError("Writing to an uninitialized variable")

	case class UnexpectedType(expected: Language.Type.Any, actual: Language.Type.Any) extends ProgramErrorCode {
		override def message(nodes: Language.Node*): String = ???
	}
}
