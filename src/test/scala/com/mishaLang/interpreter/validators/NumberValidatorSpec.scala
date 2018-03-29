package com.mishaLang.interpreter.validators

import com.mishaLang.ast.Language.Value.Number
import com.mishaLang.interpreter.BaseInterpreterSpec

class NumberValidatorSpec extends BaseInterpreterSpec {

	behavior of "Number validator"

	it should "correctly detect integers" in {
		assert(NumberValidator.isInteger(Number(123.0d)))
		assert(!NumberValidator.isInteger(Number(123.123d)))
	}
}
