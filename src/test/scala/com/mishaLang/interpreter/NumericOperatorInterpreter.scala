package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.{Addition, BinaryOperation, Multiplication, Subtraction}
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Percentage, Rgba, Scalar}
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, StringOps}

class NumericOperatorInterpreter extends BaseInterpreterSpec {

	behavior of "Numeric operator interpreter"

	it should "evaluate Color ± Percentage" in {
		val (l, r) = (Rgba(240, 70, 21), Percentage(25))
		assert(run(BinaryOperation(Addition, l, r)).value === ColorOps.lighten(l, r))
		assert(run(BinaryOperation(Subtraction, l, r)).value === ColorOps.darken(l, r))
	}

	it should "evaluate Color ± Color" in {
		val (l, r) = (Rgba(1, 2, 3, 4), Rgba(4, 3, 2, 1))
		assert(run(BinaryOperation(Addition, l, r)).value === ColorOps.addColors(l, r))
		assert(run(BinaryOperation(Subtraction, l, r)).value === ColorOps.subtractColors(l, r))
	}

	it should "evaluate String + String" in {
		val (l, r) = (Value.String("foo"), Value.String("fighters"))
		assert(run(BinaryOperation(Addition, l, r)).value === StringOps.concatenate(l, r))
		//assert(run(BinaryOperation(Addition, l, Value.Tuple2Value(Scalar(123), Scalar(456)))).value === Value.String(""))
	}

	it should "evaluate String * Scalar" in {
		val (l, r) = (Value.String("a"), Scalar(3))
		assert(run(BinaryOperation(Multiplication, l, r)).value === StringOps.multiply(l, r))
		assertThrows[ProgramError[_]](run(BinaryOperation(Multiplication, l, Scalar(123.456))))
	}

	it should "evaluate List * Scalar" in {
		val (l, r) = (Value.List(Vector(Scalar(1), Scalar(2))), Scalar(3))
		assert(run(BinaryOperation(Multiplication, l, r)).value === ListOps.repeat(l, r.value.toInt))
		assertThrows[ProgramError[_]](run(BinaryOperation(Multiplication, l, Scalar(123.456))))
	}


	protected def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): EnvWithValue =
		super.run[BinaryOperation](BinaryOperationInterpreter.run(_)(state), binaryOperation)

}
