package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.{LogicalNegation, UnaryOperation}
import com.preprocessor.ast.Language.Term.{ParentSelector, Term, Variable}
import com.preprocessor.ast.Language.{Term, Type, Value}
import com.preprocessor.ast.RuleContext.RuleSelector
import com.preprocessor.ast.Selector.Class
import com.preprocessor.ast.Symbol.ValueSymbol
import com.preprocessor.ast.ValueRecord
import com.preprocessor.error.ProgramError

class TermInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Term interpreter"

	it should "correctly read existing variables" in {
		val symbol = ValueSymbol("myVar")
		val varType = Type.Scalar
		val varValue = Value.Scalar(123)
		val variable = Variable(symbol)
		val newState = state.withNewSymbol(symbol)(ValueRecord(varValue, varType)).get

		assert(run(variable)(newState).valueRecord.value === varValue)
	}

	it should "reject undefined variable reads" in {
		assertThrows[ProgramError](run(Variable(ValueSymbol("absentVar"))))
	}

	it should "correctly interpret tuples" in {
		assert(run(Term.Tuple2(
			UnaryOperation(LogicalNegation, Value.Boolean(true)), Value.String("foo")
		)).valueRecord.value === Value.Tuple2(
			Value.Boolean(false), Value.String("foo")
		))
	}

	it should "correctly interpret the parent selector magic symbol" in {
		assert(run(ParentSelector).valueRecord.value === Value.String(""))

		val originalRuleHead = ".myClass"
		val newState = EvalState(testEnvironment.pushSubScope(RuleSelector(Class("myClass"), originalRuleHead)))
		assert(run(ParentSelector)(newState).valueRecord.value === Value.String(originalRuleHead))
	}

	protected def run(term: Term)(implicit state: EvalState): EvalState =
		super.run[Term](TermInterpreter.run(_), term)
}
