package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Statement.VariableDeclaration
import com.mishaLang.ast.Language.Value.Scalar
import com.mishaLang.ast.Language.{Term, Value, ValueSymbolDeclaration}
import com.mishaLang.error.ProgramError
import com.mishaLang.interpreter.Symbol.ValueSymbol

class ExpressionInterpreterSpec extends BaseInterpreterSpec {


	behavior of "Expression interpreter"

	it should "correctly negate booleans" in {
		assert(run(UnaryOperation(LogicalNegation, Value.Boolean(true))).value === Value.Boolean(false))
		assertThrows[ProgramError[_]](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly negate numbers" in {
		assert(run(UnaryOperation(ArithmeticNegation, Value.Scalar(123))).value === Value.Scalar(-123))
		assertThrows[ProgramError[_]](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly execute conditional expressions" in {
		val consequent = Value.Scalar(123)
		val alternative = Value.Scalar(456)

		assert(run(Conditional(Value.Boolean(true), consequent, None)).value === consequent)
		assert(run(Conditional(Value.Boolean(true), consequent, Some(alternative))).value === consequent)
		assert(run(Conditional(Value.Boolean(false), consequent, Some(alternative))).value === alternative)
		assertThrows[ProgramError[_]](run(Conditional(consequent, consequent, None)))
	}

	it should "correctly execute assignments within blocks" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val outerValue = Scalar(123)
		val innerValue = Scalar(456)

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(outerValue).get

		val updatedState = run(Block(BinaryOperation(Equals, variable, innerValue)))(newState)

		assert(updatedState.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get === innerValue)
	}

	it should "correctly execute declarations within blocks" in {
		val symbol = ValueSymbol("myVar")
		val outerValue = Scalar(123)
		val innerValue = Scalar(456)

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(outerValue).get

		val updatedState = run(Block(VariableDeclaration(ValueSymbolDeclaration(symbol, None, innerValue))))(newState)

		assert(updatedState.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get === outerValue)
	}


	protected def run(expression: Expression)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Expression](ExpressionInterpreter.run(_), expression)
}
