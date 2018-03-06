package com.preprocessor.interpreter

import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement.VariableDeclaration
import com.preprocessor.ast.Ast.Value.Scalar
import com.preprocessor.ast.Ast.{Term, Type, Value, ValueSymbolDeclaration}
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

import scala.util.{Failure, Success}

class ExpressionInterpreterSpec extends BaseInterpreterSpec {


	behavior of "Expression interpreter"

	it should "correctly negate booleans" in {
		assert(run(UnaryOperation(LogicalNegation, Value.Boolean(true))).valueRecord.value === Value.Boolean(false))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly negate numbers" in {
		assert(run(UnaryOperation(ArithmeticNegation, Value.Scalar(123))).valueRecord.value === Value.Scalar(-123))
		assertThrows[ProgramError](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly execute conditional expressions" in {
		val consequent = Value.Scalar(123)
		val alternative = Value.Scalar(456)

		assert(run(Conditional(Value.Boolean(true), consequent, None)).valueRecord.value === consequent)
		assert(run(Conditional(Value.Boolean(true), consequent, Some(alternative))).valueRecord.value === consequent)
		assert(run(Conditional(Value.Boolean(false), consequent, Some(alternative))).valueRecord.value === alternative)
		assertThrows[ProgramError](run(Conditional(consequent, consequent, None)))
		assertThrows[ProgramError](run(Conditional(Value.Boolean(true), consequent, Some(Value.Boolean(true)))))
	}

	it should "correctly execute assignments within blocks" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val outerValue = Scalar(123)
		val innerValue = Scalar(456)

		val newState: EvalState =
			state.withNewSymbol(symbol)(ValueRecord(outerValue, Type.Scalar)).get

		val updatedState = run(Block(BinaryOperation(Equals, variable, innerValue)))(newState)

		assert(updatedState.valueRecord.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get.value === innerValue)
	}

	it should "correctly execute declarations within blocks" in {
		val symbol = ValueSymbol("myVar")
		val outerValue = Scalar(123)
		val innerValue = Scalar(456)

		val newState: EvalState =
			state.withNewSymbol(symbol)(ValueRecord(outerValue, Type.Scalar)).get

		val updatedState = run(Block(VariableDeclaration(ValueSymbolDeclaration(symbol, None, innerValue))))(newState)

		assert(updatedState.valueRecord.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get.value === outerValue)
	}


	protected def run(expression: Expression)(implicit state: EvalState): EvalState = {
		val result = ExpressionInterpreter.run(expression)

		result match {
			case Failure(exception) => throw exception
			case Success(value) => value
		}
	}
}
