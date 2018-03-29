package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Formula, Number, Rgba}
import com.mishaLang.ast.NumberUnit.Atomic
import com.mishaLang.ast.SimpleExpression
import com.mishaLang.ast.SimpleExpression.Term
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, StringOps}
import com.mishaLang.spec.units.Angle.Turn
import com.mishaLang.spec.units.Length.Pixel

class NumericOperatorInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Numeric operator interpreter"

	it should "evaluate Color ± Color" in {
		val (l, r) = (Rgba(1, 2, 3, 4), Rgba(4, 3, 2, 1))
		assert(run(BinaryOperation(Addition, l, r)).value === ColorOps.addColors(l, r))
		assert(run(BinaryOperation(Subtraction, l, r)).value === ColorOps.subtractColors(l, r))
	}

	it should "evaluate String + String" in {
		val (l, r) = (Value.String("foo"), Value.String("fighters"))
		assert(run(BinaryOperation(Addition, l, r)).value === StringOps.concatenate(l, r))
		//assert(run(BinaryOperation(Addition, l, Value.Tuple2Value(Number(123), Number(456)))).value === Value.String(""))
	}

	it should "evaluate String * Scalar" in {
		val (l, r) = (Value.String("a"), Number(3))
		assert(run(BinaryOperation(Multiplication, l, r)).value === StringOps.multiply(l, r))
		assertThrows[ProgramError[_]](run(BinaryOperation(Multiplication, l, Number(123.456))))
	}

	it should "evaluate List * Scalar" in {
		val (l, r) = (Value.List(Vector(Number(1), Number(2))), Number(3))
		assert(run(BinaryOperation(Multiplication, l, r)).value === ListOps.repeat(l, r.value.toInt))
		assertThrows[ProgramError[_]](run(BinaryOperation(Multiplication, l, Number(123.456))))
	}

	private val TenPx = Number(10, Atomic(Pixel))
	private val FiveTurns = Number(5, Atomic(Turn))

	it should "evaluate Number op Number" in {

		assert(run(BinaryOperation(Division, TenPx, TenPx)).value === Number(1))
		assert(run(BinaryOperation(Division, TenPx, FiveTurns)).value === Number(
			2, Map(Atomic(Pixel) -> 1, Atomic(Turn) -> -1)
		))

		assert(run(BinaryOperation(Multiplication, TenPx, TenPx)).value === Number(
			100, Map(Atomic(Pixel) -> 2)
		))
		assert(run(BinaryOperation(Multiplication, TenPx, FiveTurns)).value === Number(
			50, Map(Atomic(Pixel) -> 1, Atomic(Turn) -> 1)
		))

		assert(run(BinaryOperation(Addition, TenPx, TenPx)).value === Number(20, Atomic(Pixel)))
		assert(run(BinaryOperation(Addition, TenPx, FiveTurns)).value === Formula(
			SimpleExpression.BinaryOperation(Addition, Term(TenPx), Term(FiveTurns))
		))

		assert(run(BinaryOperation(Subtraction, TenPx, TenPx)).value === Number(0, Atomic(Pixel)))
		assert(run(BinaryOperation(Subtraction, TenPx, FiveTurns)).value === Formula(
			SimpleExpression.BinaryOperation(Subtraction, Term(TenPx), Term(FiveTurns))
		))

		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, TenPx)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, FiveTurns)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, TenPx)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, FiveTurns)))
	}


	it should "evaluate Number op Scalar" in {
		assert(run(BinaryOperation(Addition, TenPx, Number(123))).value === Formula(
			SimpleExpression.BinaryOperation(Addition, Term(TenPx), Term(Number(123)))
		))
		assert(run(BinaryOperation(Subtraction, TenPx, Number(123))).value === Formula(
			SimpleExpression.BinaryOperation(Subtraction, Term(TenPx), Term(Number(123)))
		))
		assert(run(BinaryOperation(Multiplication, TenPx, Number(3))).value === Number(
			30, Atomic(Pixel)
		))
		assert(run(BinaryOperation(Division, TenPx, Number(2))).value === Number(
			5, Atomic(Pixel)
		))
		assert(run(BinaryOperation(Exponentiation, TenPx, Number(3))).value === Number(
			1000, Map(Atomic(Pixel) -> 3)
		))
		assert(run(BinaryOperation(Remainder, TenPx, Number(3))).value === Number(
			1, Atomic(Pixel)
		))
		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, Number(123.456))))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, Number(123.456))))
	}


	protected def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): EnvWithValue =
		super.run[BinaryOperation](BinaryOperationInterpreter.run(_)(state), binaryOperation)

}
