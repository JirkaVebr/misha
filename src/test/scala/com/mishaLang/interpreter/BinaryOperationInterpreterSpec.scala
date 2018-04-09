package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Value.{Rgba, Number}
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.ast.NumberUnit.Percentage
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.Symbol.ValueSymbol

class BinaryOperationInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Binary operation interpreter"

	// TODO test subtyping checks

	it should "correctly perform assignment" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = Number(123)
		val targetValue = Number(456)

		assertThrows[ProgramError[_]](run(BinaryOperation(Equals, variable, targetValue))(state))

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(initialValue).get

		assert(newState.environment.lookup(symbol).get === initialValue)

		val updatedState = run(BinaryOperation(Equals, variable, targetValue))(newState)
		assert(updatedState.value === targetValue)
		assert(updatedState.environment.lookup(symbol).get === targetValue)
	}

	it should "correctly perform comparison" in {
		assert(run(BinaryOperation(IsEqualTo, Number(123), Number(123))).value === Value.Boolean(true))
		assert(run(BinaryOperation(IsEqualTo, Number(123), Number(456))).value === Value.Boolean(false))
		assert(run(BinaryOperation(IsEqualTo, Number(123), Value.Boolean(true))).value === Value.Boolean(false))
		assert(run(BinaryOperation(LowerThan, Number(123), Number(456))).value === Value.Boolean(true))
		assert(run(BinaryOperation(LowerEquals, Number(123), Number(456))).value === Value.Boolean(true))
		assert(run(BinaryOperation(GreaterEquals, Number(456), Number(456))).value === Value.Boolean(true))
	}

	it should "reject illegal comparisons" in {
		assertThrows[ProgramError[_]](run(BinaryOperation(GreaterEquals, Number(456), Value.Boolean(true))))
		assertThrows[ProgramError[_]](run(BinaryOperation(GreaterEquals, Number(456), Value.Number(456, Percentage))))
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

	it should "sometimes ignore illegal logical operation operands due to laziness" in {
		assert(run(BinaryOperation(and, f, Value.Number(123))).value === f)
		assert(run(BinaryOperation(or, t, Value.Number(123))).value === t)
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


	protected def run(binaryOperation: BinaryOperation)(implicit state: EnvWithValue): EnvWithValue =
		super.run[BinaryOperation](BinaryOperationInterpreter.run(_)(state), binaryOperation)
}
