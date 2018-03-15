package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Block
import com.preprocessor.ast.Language.Statement.{Property, Rule, Sequence}
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.PropertyRecord
import com.preprocessor.ast.RuleContext.RuleSelector
import com.preprocessor.ast.Selector.Class
import com.preprocessor.ast.Symbol.{PropertySymbol, RuleContextSymbol}

class RuleInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Rule interpreter"

	it should "correctly interpret rules" in {
		val newState = run(Rule(Seq(Left(".class")), Block(Sequence(
			Property(Value.String("line-height"), Value.Scalar(1.6)),
			Property(Value.String("width"), Value.Percentage(80))
		))))
		val ruleEnvironment = newState.environment.subEnvironments.head

		// TODO
		assert(ruleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(Class("class")))
		assert(ruleEnvironment.lookupCurrent(PropertySymbol).get === List(
			PropertyRecord("width", "80%"),
			PropertyRecord("line-height", "1.6")
		))
	}


	protected def run(rule: Rule)(implicit state: EvalState): EvalState =
		super.run[Rule](RuleInterpreter.run(_), rule)

}
