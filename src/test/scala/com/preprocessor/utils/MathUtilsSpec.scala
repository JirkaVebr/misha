package com.preprocessor.utils

class MathUtilsSpec extends BaseUtilsSpec {

	behavior of "Math utils"

	it should "correctly round with precision" in {
		assert(MathUtils.round(123) === 123)
		assert(MathUtils.round(123.123) === 123)
		assert(MathUtils.round(123.567) === 124)

		assert(MathUtils.round(123.567, 2) === 123.57)
		assert(MathUtils.round(123.567, 10) === 123.567)
	}
}
