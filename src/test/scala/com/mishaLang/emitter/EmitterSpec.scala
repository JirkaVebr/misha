package com.mishaLang.emitter

import com.mishaLang.ast.PropertyRecord
import com.mishaLang.interpreter.Symbol.{PropertySymbol, RuleContextSymbol}
import com.mishaLang.interpreter.Environment

class EmitterSpec extends BaseEmitterSpec {

	behavior of "The emitter"

	it should "emit an environment with just properties" in {
		val environment = new Environment
		val withProperties = environment.putNew(PropertySymbol)(List(
			PropertyRecord("line-height", "1.6"),
			PropertyRecord("color", "blue")
		))

		assert(emit(withProperties) ===
			"""	color: blue;
				|	line-height: 1.6;
				|""".stripMargin)
	}

	it should "emit an environment with a rule-less sub-environment" in {
		val baseEnvironment = new Environment().putNew(PropertySymbol)(List(PropertyRecord("line-height", "1.6")))
		val childEnvironment = baseEnvironment.pushSubScope().get
		val childWithProperty = childEnvironment.putNew(PropertySymbol)(List(PropertyRecord("color", "blue")))
		val baseWithChild = childWithProperty.popSubScope().get

		assert(emit(baseWithChild) ===
			"""	line-height: 1.6;
				|	color: blue;
				|""".stripMargin)
	}

	// TODO
	/*it should "emit an environment with a rule" in {
		val environment = new Environment().putNew(RuleContextSymbol)(RawRuleHead(List(Right(".myClass"))))
		val withProperties = environment.putNew(PropertySymbol)(List(
			PropertyRecord("line-height", "1.6"),
			PropertyRecord("color", "blue")
		))

		assert(emit(withProperties) ===
			""".myClass {
				|	color: blue;
				|	line-height: 1.6;
				|}
				|""".stripMargin)
	}*/

	it should "emit an environment with a rule and a sub-environment" in {
		// TODO
	}

	def emit(environment: Environment): String =
		new Emitter(environment).emit().toString()
}
