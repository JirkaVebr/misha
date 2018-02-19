package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Statement.{Statement, VariableDeclaration}
import com.preprocessor.ast.Ast.Term.{Term, Variable}
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


	protected def run(expression: Statement)(implicit state: EvalState): EvalState = {
		val result = StatementInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
