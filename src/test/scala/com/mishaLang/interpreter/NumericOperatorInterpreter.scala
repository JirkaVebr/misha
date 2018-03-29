package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{Dimensioned, Formula, Rgba, Scalar}
import com.mishaLang.ast.NumberUnit.{Atomic, RaisedUnit}
import com.mishaLang.ast.SimpleExpression
import com.mishaLang.ast.SimpleExpression.Term
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.ops.{ColorOps, ListOps, StringOps}
import com.mishaLang.spec.units.Angle.Turn
import com.mishaLang.spec.units.Length.Pixel

class NumericOperatorInterpreter extends BaseInterpreterSpec {

	behavior of "Numeric operator interpreter"

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

	private val TenPx = Dimensioned(10, Atomic(Pixel))
	private val FiveTurns = Dimensioned(5, Atomic(Turn))

	it should "evaluate Dimensioned op Dimensioned" in {

		assert(run(BinaryOperation(Division, TenPx, TenPx)).value === Scalar(1))
		assert(run(BinaryOperation(Division, TenPx, FiveTurns)).value === Dimensioned(
			2, RaisedUnit(Map(Atomic(Pixel) -> 1, Atomic(Turn) -> -1))
		))

		assert(run(BinaryOperation(Multiplication, TenPx, TenPx)).value === Dimensioned(
			100, RaisedUnit(Map(Atomic(Pixel) -> 2))
		))
		assert(run(BinaryOperation(Multiplication, TenPx, FiveTurns)).value === Dimensioned(
			50, RaisedUnit(Map(Atomic(Pixel) -> 1, Atomic(Turn) -> 1))
		))

		assert(run(BinaryOperation(Addition, TenPx, TenPx)).value === Dimensioned(20, Atomic(Pixel)))
		assert(run(BinaryOperation(Addition, TenPx, FiveTurns)).value === Formula(
			SimpleExpression.BinaryOperation(Addition, Term(TenPx), Term(FiveTurns))
		))

		assert(run(BinaryOperation(Subtraction, TenPx, TenPx)).value === Dimensioned(0, Atomic(Pixel)))
		assert(run(BinaryOperation(Subtraction, TenPx, FiveTurns)).value === Formula(
			SimpleExpression.BinaryOperation(Subtraction, Term(TenPx), Term(FiveTurns))
		))

		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, TenPx)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, FiveTurns)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, TenPx)))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, FiveTurns)))
	}


	it should "evaluate Dimensioned op Scalar" in {
		assert(run(BinaryOperation(Addition, TenPx, Scalar(123))).value === Formula(
			SimpleExpression.BinaryOperation(Addition, Term(TenPx), Term(Scalar(123)))
		))
		assert(run(BinaryOperation(Subtraction, TenPx, Scalar(123))).value === Formula(
			SimpleExpression.BinaryOperation(Subtraction, Term(TenPx), Term(Scalar(123)))
		))
		assert(run(BinaryOperation(Multiplication, TenPx, Scalar(3))).value === Dimensioned(
			30, Atomic(Pixel)
		))
		assert(run(BinaryOperation(Division, TenPx, Scalar(2))).value === Dimensioned(
			5, Atomic(Pixel)
		))
		assert(run(BinaryOperation(Exponentiation, TenPx, Scalar(3))).value === Dimensioned(
			1000, RaisedUnit(Map(Atomic(Pixel) -> 3))
		))
		assert(run(BinaryOperation(Remainder, TenPx, Scalar(3))).value === Dimensioned(
			1, Atomic(Pixel)
		))
		assertThrows[ProgramError[_]](run(BinaryOperation(Exponentiation, TenPx, Scalar(123.456))))
		assertThrows[ProgramError[_]](run(BinaryOperation(Remainder, TenPx, Scalar(123.456))))
	}


	protected def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): EnvWithValue =
		super.run[BinaryOperation](BinaryOperationInterpreter.run(_)(state), binaryOperation)

}
