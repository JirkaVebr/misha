package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.Scalar
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage}
import com.mishaLang.interpreter.BaseInterpreterSpec
import com.mishaLang.spec.units.Angle.Radian
import com.mishaLang.spec.units.Length.Pixel

class StringOpsSpec extends BaseInterpreterSpec {

	behavior of "String operations"

	it should "correctly cast values to strings" in {
		val alreadyString = Value.String("already string")
		assert(StringOps.castToString(alreadyString).get === alreadyString)

		val number = Value.Scalar(123.456)
		assert(StringOps.castToString(number).get === Value.String("123.456"))
		assert(StringOps.castToString(Value.Dimensioned(50, Percentage)).get === Value.String("50%"))
		assert(StringOps.castToString(Value.Dimensioned(123, Atomic(Pixel))).get === Value.String("123px"))
		assert(StringOps.castToString(Value.Dimensioned(0, Atomic(Pixel))).get === Value.String("0"))
		assert(StringOps.castToString(Value.Dimensioned(0, Atomic(Radian))).get === Value.String("0rad"))

		assert(StringOps.castToString(Value.Tuple2(alreadyString, number)).isEmpty)
	}

	it should "correctly concatenate strings" in {
		val first = Value.String("first")
		val second = Value.String("second")

		assert(StringOps.concatenate(first, second) === Value.String("firstsecond"))
	}

	it should "correctly multiply strings" in {
		assert(StringOps.multiply(Value.String("a"), Scalar(4)) === Value.String("aaaa"))
		assert(StringOps.multiply(Value.String("a"), Scalar(0)) === Value.String(""))
		assert(StringOps.multiply(Value.String("a"), Scalar(-123)) === Value.String(""))
	}
}
