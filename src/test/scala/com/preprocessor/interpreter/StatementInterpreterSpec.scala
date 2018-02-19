package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement.{Sequence, Statement, VariableDeclaration}
import com.preprocessor.ast.Ast.Term.Variable
import com.preprocessor.ast.Ast.{Type, Value}
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class StatementInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Statement interpreter"

	it should "correctly declare variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Number(123)

		val newState = run(VariableDeclaration(symbol, None, varValue))
		assert(newState.environment.isInCurrentScope(symbol))
		assert(newState.environment.lookup(symbol).get.value == varValue)
	}

	it should "reject declaration of existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varType = Type.Number
		val varValue = Value.Number(123)
		val newState = (state withUpdatedValue(symbol, ValueRecord(varValue, varType))).get

		assertThrows[ProgramError](run(VariableDeclaration(symbol, None, varValue))(newState))
	}

	it should "reject declarations with wrong type annotations" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Number(123)
		val varType = Type.Boolean // Deliberately wrong type … that's the point here

		assertThrows[ProgramError](run(VariableDeclaration(symbol, Some(varType), varValue)))
	}

	it should "correctly run a sequence" in {
		val symbol1 = ValueSymbol("myVar1")
		val symbol2 = ValueSymbol("myVar2")
		val varValue1 = Value.Number(123)
		val consequent = Value.Number(111)
		val alternative = Value.Number(222)

		assert(run(
			Sequence(
				Sequence(
					VariableDeclaration(symbol1, None, varValue1),
					VariableDeclaration(symbol2, None,
						Conditional(BinaryOperation(IsEqualTo, Variable(symbol1), varValue1), consequent, Some(alternative))
					)
				),
				Variable(symbol2)
			)
		).valueRecord.value == consequent)
	}


	protected def run(expression: Statement)(implicit state: EvalState): EvalState = {
		val result = StatementInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
