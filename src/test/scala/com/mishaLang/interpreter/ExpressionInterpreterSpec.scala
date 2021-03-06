package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression._
import com.mishaLang.ast.Language.Statement.VariableDeclaration
import com.mishaLang.ast.Language.Value.Number
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
		assert(run(UnaryOperation(ArithmeticNegation, Value.Number(123))).value === Value.Number(-123))
		assertThrows[ProgramError[_]](run(UnaryOperation(LogicalNegation, Value.String("I shall not be negated"))))
	}

	it should "correctly execute conditional expressions" in {
		val consequent = Value.Number(123)
		val alternative = Value.Number(456)

		assert(run(Conditional(Value.Boolean(true), consequent, None)).value === consequent)
		assert(run(Conditional(Value.Boolean(true), consequent, Some(alternative))).value === consequent)
		assert(run(Conditional(Value.Boolean(false), consequent, Some(alternative))).value === alternative)
		assertThrows[ProgramError[_]](run(Conditional(consequent, consequent, None)))
	}

	it should "correctly execute assignments within blocks" in {
		val symbol = ValueSymbol("myVar")
		val variable = Term.Variable(symbol)
		val outerValue = Number(123)
		val innerValue = Number(456)

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(outerValue).get

		val updatedState = run(Block(BinaryOperation(Equals, variable, innerValue)))(newState)

		assert(updatedState.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get === innerValue)
	}

	it should "correctly execute declarations within blocks" in {
		val symbol = ValueSymbol("myVar")
		val outerValue = Number(123)
		val innerValue = Number(456)

		val newState: EnvWithValue =
			state.withNewSymbol(symbol)(outerValue).get

		val updatedState = run(Block(VariableDeclaration(ValueSymbolDeclaration(symbol, None, innerValue))))(newState)

		assert(updatedState.value === innerValue)
		assert(updatedState.environment.lookup(symbol).get === outerValue)
	}

	it should "evaluate functions with static scope" in {
		val testVariable = ValueSymbol("testVariable")
		val testLambda = ValueSymbol("testLambda")
		val testValue1 = Number(123)
		val testValue2 = Number(456)

		val root = testEnvironment.putNew(testVariable)(testValue1)
		var sub0 = root.pushSubScope().get
		sub0 = sub0.putNew(testLambda)(Value.Lambda(
			None, Vector(), Vector(), None, Term.Variable(testVariable), sub0.meta.id
		))
		sub0 = sub0.updated(testVariable)(testValue2)

		assert(run(Term.FunctionCall(Term.Variable(testLambda), Vector()))(EnvironmentWithValue(sub0)).value === testValue2)
		/*
		{ // root
			@let $testVariable = 123
			{ // sub0
				@let testLambda = () => $testVariable
				$testVariable = 456
				$testLambda()
			}
		}
		*/
	}


	it should "enforce stack overflow" in {
		def wrap(expression: Expression, n: Int): Expression =
			if (n == 0) expression
			else Block(wrap(expression, n - 1))

		assertThrows[ProgramError[_]](run(wrap(Value.Number(1), 100)))
	}


	protected def run(expression: Expression)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Expression](ExpressionInterpreter.run(_)(state), expression)
}
