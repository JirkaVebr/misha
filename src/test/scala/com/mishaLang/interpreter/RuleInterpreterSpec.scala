package com.mishaLang.interpreter

import com.mishaLang.ast.Language.Expression.Block
import com.mishaLang.ast.Language.Statement.{Property, Rule, Sequence}
import com.mishaLang.ast.Language.Term.ParentSelector
import com.mishaLang.ast.Language.{Term, Value}
import com.mishaLang.ast.PropertyRecord
import com.mishaLang.ast.Selector.{Class, Complex, Id, SelectorList}
import com.mishaLang.interpreter.RuleContext.RuleSelector
import com.mishaLang.interpreter.Symbol.{PropertySymbol, RuleContextSymbol}
import com.mishaLang.spec.SelectorSeparator.Descendant

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

	it should "correctly interpret rules with structured selectors" in {
		/*
		.myClass1 .mySubclass,
		.myClass1 .mySubclass
		 */
		val newState = run(Rule(Vector(
			Left(".m"),
			Left("y"),
			Right(Term.List(List(
				Value.String("Class1"),
				Value.String("Class2")
			))),
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

	it should "correctly interpret rules with implicit parent selectors" in {
		val newState = run(Rule(Vector(Left(".myClass")), Block(Rule(
			Vector(Left("#myId, .anotherClass")), Block(
				Property(Value.String("width"), Value.Percentage(80))
			))
		)))
		val outerRuleEnvironment = newState.environment.subEnvironments.head
		val innerRuleEnvironment = outerRuleEnvironment.subEnvironments.head

		assert(outerRuleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(Class("myClass")))
		assert(innerRuleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(SelectorList(Set(
			Complex(Descendant, Class("myClass"), Id("myId")),
			Complex(Descendant, Class("myClass"), Class("anotherClass"))
		))))
	}

	it should "correctly interpret rules with explicit parent selectors" in {
		val newState = run(Rule(Vector(Left(".myClass1, .myClass2")), Block(Rule(
			Vector(Right(ParentSelector), Left(" .anotherClass")), Block(
				Property(Value.String("width"), Value.Percentage(80))
			))
		)))
		val outerRuleEnvironment = newState.environment.subEnvironments.head
		val innerRuleEnvironment = outerRuleEnvironment.subEnvironments.head

		assert(innerRuleEnvironment.lookupCurrent(RuleContextSymbol).get === RuleSelector(SelectorList(Set(
			Complex(Descendant, Class("myClass1"), Class("anotherClass")),
			Complex(Descendant, Class("myClass2"), Class("anotherClass"))
		))))
	}


	protected def run(rule: Rule)(implicit state: EnvWithValue): EnvWithValue =
		super.run[Rule](RuleInterpreter.run(_), rule)

}
