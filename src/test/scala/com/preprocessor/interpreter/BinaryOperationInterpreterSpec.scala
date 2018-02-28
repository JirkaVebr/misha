package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Value.{Percentage, Rgba, Scalar}
import com.preprocessor.ast.Ast.{Term, Type, Value}
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError
import com.preprocessor.interpreter.ops.{ColorOps, StringOps}

import scala.util.{Failure, Success}

class BinaryOperationInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Binary operation interpreter"

	// TODO test subtyping checks

	it should "correctly perform assignment" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = Scalar(123)
		val targetValue = Scalar(456)

		assertThrows[ProgramError](run(BinaryOperation(Equals, variable, targetValue))(state))

		val newState: EvalState =
			state.withNewSymbol(symbol)(ValueRecord(initialValue, Type.Scalar)).get

		assert(newState.environment.lookup(symbol).get.value == initialValue)

		val updatedState = run(BinaryOperation(Equals, variable, targetValue))(newState)
		assert(updatedState.valueRecord.value == targetValue)
		assert(updatedState.environment.lookup(symbol).get.value == targetValue)
	}

	it should "correctly perform comparison" in {
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Scalar(123))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Scalar(456))).valueRecord.value == Value.Boolean(false))
		assert(run(BinaryOperation(IsEqualTo, Scalar(123), Value.Boolean(true))).valueRecord.value == Value.Boolean(false))
		assert(run(BinaryOperation(LowerThan, Scalar(123), Scalar(456))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(LowerEquals, Scalar(123), Scalar(456))).valueRecord.value == Value.Boolean(true))
		assert(run(BinaryOperation(GreaterEquals, Scalar(456), Scalar(456))).valueRecord.value == Value.Boolean(true))
	}

	it should "reject illegal comparisons" in {
		assertThrows[ProgramError](run(BinaryOperation(GreaterEquals, Scalar(456), Value.Boolean(true))))
		assertThrows[ProgramError](run(BinaryOperation(GreaterEquals, Scalar(456), Percentage(456))))
	}


	private val t = Value.Boolean(true)
	private val f = Value.Boolean(false)
	private val and = LogicalAnd
	private val or = LogicalOr

	it should "correctly perform logical operations" in {
		assert(run(BinaryOperation(and, t, t)).valueRecord.value == t)
		assert(run(BinaryOperation(and, t, f)).valueRecord.value == f)
		assert(run(BinaryOperation(and, f, t)).valueRecord.value == f)
		assert(run(BinaryOperation(and, f, f)).valueRecord.value == f)
		assert(run(BinaryOperation(or, t, t)).valueRecord.value == t)
		assert(run(BinaryOperation(or, t, f)).valueRecord.value == t)
		assert(run(BinaryOperation(or, f, t)).valueRecord.value == t)
		assert(run(BinaryOperation(or, f, f)).valueRecord.value == f)
	}

	it should "reject illegal logical operation operands" in {
		// It should check the numbers despite not technically having to evaluate them
		assertThrows[ProgramError](run(BinaryOperation(and, f, Value.Scalar(123))).valueRecord.value == f)
		assertThrows[ProgramError](run(BinaryOperation(or, t, Value.Scalar(123))).valueRecord.value == t)
	}

	it should "evaluate logical operations lazily" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val initialValue = t
		val targetValue = f
		val assignment = BinaryOperation(Equals, variable, targetValue)

		val stateWithVar: EvalState =
			state.withNewSymbol(symbol)(ValueRecord(initialValue, Type.Boolean)).get

		val stateAfterAnd = run(BinaryOperation(and, f, assignment))(stateWithVar)
		val stateAfterOr = run(BinaryOperation(or, t, assignment))(stateWithVar)

		assert(TermInterpreter.run(variable)(stateAfterAnd).get.valueRecord.value == initialValue)
		assert(TermInterpreter.run(variable)(stateAfterOr).get.valueRecord.value == initialValue)
	}

	it should "evaluate Color ± Percentage" in {
		val (l, r) = (Rgba(240, 70, 21), Percentage(25))
		assert(run(BinaryOperation(Addition, l, r)).valueRecord.value == ColorOps.lighten(l, r))
		assert(run(BinaryOperation(Subtraction, l, r)).valueRecord.value == ColorOps.darken(l, r))
	}

	it should "evaluate Color ± Color" in {
		val (l, r) = (Rgba(1, 2, 3, 4), Rgba(4, 3, 2, 1))
		assert(run(BinaryOperation(Addition, l, r)).valueRecord.value == ColorOps.addColors(l, r))
		assert(run(BinaryOperation(Subtraction, l, r)).valueRecord.value == ColorOps.subtractColors(l, r))
	}

	it should "evaluate String + String" in {
		val (l, r) = (Value.String("foo"), Value.String("fighters"))
		assert(run(BinaryOperation(Addition, l, r)).valueRecord.value == StringOps.concatenate(l, r))
		//assert(run(BinaryOperation(Addition, l, Value.Tuple2Value(Scalar(123), Scalar(456)))).valueRecord.value == Value.String(""))
	}

	it should "evaluate String * Scalar" in {
		val (l, r) = (Value.String("a"), Scalar(3))
		assert(run(BinaryOperation(Multiplication, l, r)).valueRecord.value == StringOps.multiply(l, r))
		assertThrows[ProgramError](run(BinaryOperation(Multiplication, l, Scalar(123.456))))
	}


	protected def run(expression: BinaryOperation)(implicit state: EvalState): EvalState = {
		val result = BinaryOperationInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
