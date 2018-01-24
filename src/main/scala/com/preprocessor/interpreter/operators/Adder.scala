package com.preprocessor.interpreter.operators

import com.preprocessor.ast.Ast.Value._

object Adder {

	def add(addend1: Value, addend2: Value): Value = compute(addend1, addend2, 1)

	def subtract(minuend: Value, subtrahend: Value): Value = compute(minuend, subtrahend, -1)


	private def compute(l: Value, r: Value, sigNum: Byte): Value = (l, r) match {
		case (Number(n1), Number(n2)) => Number(n1 + sigNum * n2)
		case _ => sys.error("Adder.add: not implemented")
	}
}
