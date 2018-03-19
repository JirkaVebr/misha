package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression._
import com.preprocessor.ast.Language.Term.{ParentSelector, Term, Variable}
import com.preprocessor.ast.Language.{Term, Value}
import com.preprocessor.ast.Selector.Class
import com.preprocessor.error.ProgramError
import com.preprocessor.interpreter.RuleContext.RuleSelector
import com.preprocessor.interpreter.Symbol.ValueSymbol

class TermInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Term interpreter"

	it should "correctly read existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varValue = Value.Scalar(123)
		val variable = Variable(symbol)
		val newState = state.withNewSymbol(symbol)(varValue).get

		assert(run(variable)(newState).value === varValue)
	}

	it should "reject undefined variable reads" in {
		assertThrows[ProgramError[_]](run(Variable(ValueSymbol("absentVar"))))
	}

	it should "correctly interpret tuples" in {
		assert(run(Term.Tuple2(
			UnaryOperation(LogicalNegation, Value.Boolean(true)), Value.String("foo")
		)).value === Value.Tuple2(
			Value.Boolean(false), Value.String("foo")
		))
	}

	it should "correctly interpret lists" in {
		assert(run(Term.List(List(
			BinaryOperation(Addition, Value.String("123"), Value.String("456")),
			BinaryOperation(IsEqualTo, Value.Scalar(10), Value.Scalar(10))
		))).value === Value.List(List(
			Value.String("123456"),
			Value.Boolean(true)
		)))
	}

	it should "correctly interpret the parent selector magic symbol" in {
		assert(run(ParentSelector).value === Value.String(""))

		val originalRuleHead = ".myClass"
		val newState = EnvironmentWithValue(testEnvironment.pushSubScope(RuleSelector(Class("myClass"))), Value.Unit)
		assert(run(ParentSelector)(newState).value === Value.String(originalRuleHead))
	}

	protected def run(term: Term)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Term](TermInterpreter.run(_), term)
}
