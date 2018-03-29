package com.mishaLang.interpreter.ops

import com.mishaLang.ast.Language.Value.Number
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage}
import com.mishaLang.interpreter.BaseInterpreterSpec
import com.mishaLang.spec.units.Length.Pixel

class NumberOpsSpec extends BaseInterpreterSpec {

	behavior of "NumberOps"

	it should "correctly raise numbers to exponents" in {
		assert(NumberOps.pow(Number(3), Number(3)).get === Number(27))
		assert(NumberOps.pow(Number(2, Percentage), Number(2)).get === Number(4, Map(Percentage -> 2)))
		assert(NumberOps.pow(Number(0, Percentage), Number(0)).isEmpty)
		assert(NumberOps.pow(Number(0, Percentage), Number(5)).get === Number(0, Map(Percentage -> 5)))
		assert(NumberOps.pow(Number(25, Map(
			Percentage -> 2, Atomic(Pixel) -> 6
		)), Number(.5)).get === Number(5, Map(
			Percentage -> 1, Atomic(Pixel) -> 3
		)))
	}
}
