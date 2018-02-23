package com.preprocessor.interpreter.validators

import com.preprocessor.ast.Ast.Value.Scalar
import com.preprocessor.interpreter.BaseInterpreterSpec

class NumberValidatorSpec extends BaseInterpreterSpec {

	behavior of "Number validator"

	it should "correctly detect integers" in {
		assert(NumberValidator.isInteger(Scalar(123.0d)))
		assert(!NumberValidator.isInteger(Scalar(123.123d)))
	}
}
