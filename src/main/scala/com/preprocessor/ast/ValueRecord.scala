package com.preprocessor.ast

import com.preprocessor.ast.Ast.{Type, Value}

/**
	* This object encapsulates a key-value pair as a more convenient alternative to tuples. Its usage throughout the
	* interpreter is twofold and in both cases potentially slightly misleading as the value of `recordType` may differ
	* (and indeed likely will!) from the value of `value.valueType`:
	* - It is what is stored in an environment under a variable name. In that context the recordType represents the
	*   declared or inferred type of the variable which can be used for later type-checking during re-assignment. In
	*   that case `recordType` can be a supertype of `value.valueType`.
	* - It represents an intermediate result of a computation. For instance, an if expression without an "else"
	*   sub-expression may actually evaluate to a particular value with a particular type but the overall type for the
	*   general case is different because it may be undefined when the condition evaluates to false
	*/
case class ValueRecord(value: Value.Value, recordType: Type.Any) {

	def ~>(newValue: Value.Value) = ValueRecord(newValue, recordType)
}

case object ValueRecord {
	def empty = ValueRecord(Value.Unit, Type.Unit)
}
