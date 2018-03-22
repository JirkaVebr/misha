package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value.{Percentage, Rgba, Scalar}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.Symbol.ValueSymbol
import com.mishaLang.interpreter.ops.{ColorOps, StringOps}

class BinaryOperationInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Binary operation interpreter"

	// TODO test subtyping checks

	it should "correctly perform assignment" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = Scalar(123)
		val targetValue = Scalar(456)

		assertThrows[ProgramError[_]](run(BinaryOperation(Equals, variable, targetValue))(state))

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(initialValue).get

		assert(newState.environment.lookup(symbol).get === initialValue)

		val updatedState = run(BinaryOperation(Equals, variable, targetValue))(newState)
		assert(updatedState.value === targetValue)
		assert(updatedState.environment.lookup(symbol).get === targetValue)
	}

	it should "correctly perform comparison" in {
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Scalar(123))).value === Value.Boolean(true))
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Scalar(456))).value === Value.Boolean(false))
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Value.Boolean(true))).value === Value.Boolean(false))
		assert(run(BinaryOperation(LowerThan, Scalar(123), Scalar(456))).value === Value.Boolean(true))
		assert(run(BinaryOperation(LowerEquals, Scalar(123), Scalar(456))).value === Value.Boolean(true))
		assert(run(BinaryOperation(GreaterEquals, Scalar(456), Scalar(456))).value === Value.Boolean(true))
	}

	it should "reject illegal comparisons" in {
		assertThrows[ProgramError[_]](run(BinaryOperation(GreaterEquals, Scalar(456), Value.Boolean(true))))
		assertThrows[ProgramError[_]](run(BinaryOperation(GreaterEquals, Scalar(456), Percentage(456))))
	}


	private val t = Value.Boolean(true)
	private val f = Value.Boolean(false)
	private val and = LogicalAnd
	private val or = LogicalOr

	it should "correctly perform logical operations" in {
		assert(run(BinaryOperation(and, t, t)).value === t)
		assert(run(BinaryOperation(and, t, f)).value === f)
		assert(run(BinaryOperation(and, f, t)).value === f)
		assert(run(BinaryOperation(and, f, f)).value === f)
		assert(run(BinaryOperation(or, t, t)).value === t)
		assert(run(BinaryOperation(or, t, f)).value === t)
		assert(run(BinaryOperation(or, f, t)).value === t)
		assert(run(BinaryOperation(or, f, f)).value === f)
	}

	it should "reject illegal logical operation operands" in {
		// It should check the numbers despite not technically having to evaluate them
		assertThrows[ProgramError[_]](run(BinaryOperation(and, f, Value.Scalar(123))).value === f)
		assertThrows[ProgramError[_]](run(BinaryOperation(or, t, Value.Scalar(123))).value === t)
	}

	it should "evaluate logical operations lazily" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = t
		val targetValue = f
		val assignment = BinaryOperation(Equals, variable, targetValue)

		val stateWithVar: EnvWithValue =
			state.withNewSymbol(symbol)(initialValue).get

		val stateAfterAnd = run(BinaryOperation(and, f, assignment))(stateWithVar)
		val stateAfterOr = run(BinaryOperation(or, t, assignment))(stateWithVar)

		assert(TermInterpreter.run(variable)(stateAfterAnd).get.value === initialValue)
		assert(TermInterpreter.run(variable)(stateAfterOr).get.value === initialValue)
	}

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


	protected def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): EnvWithValue =
		super.run[BinaryOperation](BinaryOperationInterpreter.run(_)(state), binaryOperation)
}
