package com.preprocessor.interpreter.operators

import com.preprocessor.ast.Ast.Value._

object Multiplier {

	def multiply(factor1: Value, factor2: Value): Value = compute(factor1, factor2, 1)

	def divide(dividend: Value, divisor: Value): Value = compute(dividend, divisor, -1)


	private def compute(l: Value, r: Value, sigNum: Byte): Value = (l, r) match {
		case (Number(n1), Number(n2)) => Number(n1 * Math.pow(n2, sigNum))
		case _ => sys.error("Multiplier.multiply: not implemented")
	}
}
