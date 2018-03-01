package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression.{LogicalNegation, UnaryOperation}
import com.preprocessor.ast.Ast.Term.{Term, Variable}
import com.preprocessor.ast.Ast.{Term, Type, Value}
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class TermInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Term interpreter"

	it should "correctly read existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varType = Type.Number
		val varValue = Value.Scalar(123)
		val variable = Variable(symbol)
		val newState = state.withNewSymbol(symbol)(ValueRecord(varValue, varType)).get

		assert(run(variable)(newState).valueRecord.value == varValue)
	}

	it should "reject undefined variable reads" in {
		assertThrows[ProgramError](run(Variable(ValueSymbol("absentVar"))))
	}

	it should "correctly interpret tuples" in {
		assert(run(Term.Tuple2(
			UnaryOperation(LogicalNegation, Value.Boolean(true)), Value.String("foo")
		)).valueRecord.value == Value.Tuple2(
			Value.Boolean(false), Value.String("foo")
		))
	}

	protected def run(expression: Term)(implicit state: EvalState): EvalState = {
		val result = TermInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
