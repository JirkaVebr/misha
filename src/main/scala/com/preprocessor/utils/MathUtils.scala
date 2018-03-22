package com.preprocessor.utils

object MathUtils {


	/**
		* @param value The thing to be rounded
		* @param precision Ideally positive… Also small enough that (10 ** precision) doesn't exceed Double.MaxValue.
		*                  Neither of these are actually enforced.
		* @return Rounded with precision
		*/
	def round(value: Double, precision: Int = 0): Double = {
		if (precision == 0) value.round
		else {
			val power = Math.pow(10, precision)
			(value * power).round / power
		}
	}

}
