package com.preprocessor.interpreter.ops

import com.preprocessor.ast.Ast.Value
import com.preprocessor.interpreter.BaseInterpreterSpec

class StringOpsSpec extends BaseInterpreterSpec {

	behavior of "String operations"

	it should "correctly cast values to strings" in {
		val alreadyString = Value.String("already string")
		assert(StringOps.castToString(alreadyString).get == alreadyString)

		val number = Value.Scalar(123.456)
		assert(StringOps.castToString(number).get == Value.String("123.456"))

		assert(StringOps.castToString(Value.Tuple2Value(alreadyString, number)).isEmpty)
	}

	it should "correctly concatenate strings" in {
		val first = Value.String("first")
		val second = Value.String("second")

		assert(StringOps.concatenate(first, second) == Value.String("firstsecond"))
	}
}