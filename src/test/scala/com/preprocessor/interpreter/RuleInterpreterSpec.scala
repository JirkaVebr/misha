package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Expression.Block
import com.preprocessor.ast.Language.Statement.{Property, Rule, Sequence}
import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.PropertyRecord
import RuleContext.RuleSelector
import com.preprocessor.ast.Selector.{Class, Complex, SelectorList}
import Symbol.{PropertySymbol, RuleContextSymbol}
import com.preprocessor.spec.SelectorCombinator.Descendant

class RuleInterpreterSpec extends BaseInterpreterSpec {

	behavior of "Rule interpreter"

	it should "correctly interpret simple rules" in {
		val newState = run(Rule(Vector(Left(".class")), Block(Sequence(
			Property(Value.String("line-height"), Value.Scalar(1.6)),
			Property(Value.String("width"), Value.Percentage(80))
		))))
		val ruleEnvironment = newState.environment.subEnvironments.head

		assert(ruleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(Class("class")))
		assert(ruleEnvironment.lookupCurrent(PropertySymbol).get === List(
			PropertyRecord("width", "80%"),
			PropertyRecord("line-height", "1.6")
		))
	}

	it should "correctly interpret rulex with structured selectors" in {
		/*
		.myClass1 .mySubclass,
		.myClass1 .mySubclass
		 */
		val newState = run(Rule(Vector(
			Left(".m"),
			Left("y"),
			Right(Vector(
				Value.String("Class1"),
				Value.String("Class2")
			)),
			Left(" .mySubclass")
		), Block(
			Property(Value.String("width"), Value.Percentage(80))
		)))
		val ruleEnvironment = newState.environment.subEnvironments.head

		assert(ruleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(
			SelectorList(Set(
				Complex(Descendant, Class("myClass1"), Class("mySubclass")),
				Complex(Descendant, Class("myClass2"), Class("mySubclass"))
			))
		))
		assert(ruleEnvironment.lookupCurrent(PropertySymbol).get === List(
			PropertyRecord("width", "80%")
		))
	}


	protected def run(rule: Rule)(implicit state: EvalState): EvalState =
		super.run[Rule](RuleInterpreter.run(_), rule)

}
