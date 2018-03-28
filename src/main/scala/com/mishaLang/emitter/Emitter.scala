package com.mishaLang.emitter

import com.mishaLang.ast.PropertyRecord
import com.mishaLang.interpreter.Symbol.{RuleStoreSymbol, RuleContextSymbol}
import com.mishaLang.interpreter.Environment
import com.mishaLang.interpreter.EnvironmentType.FunctionEnvironment

class Emitter(val finalEnvironment: Environment) {


	def emit(): StringBuilder =
		emitFinalEnvironment(finalEnvironment)(StringBuilder.newBuilder)

	private def emitFinalEnvironment(environment: Environment)(implicit builder: StringBuilder): StringBuilder = {
		val ruleStore = environment.lookupCurrent(RuleStoreSymbol).get

		for ((context, propertyStore) <- ruleStore) {
			emitRuleStart(context)

			for ((_, propertyRecords) <- propertyStore) {
				emitProperties(propertyRecords)
			}

			emitRuleEnd(context)
		}

		builder
	}

	private def emitRuleStart(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder =
		RuleHeadEmitter.emit(head).append(" {\n")

	private def emitRuleEnd(head: RuleContextSymbol.Value)(implicit builder: StringBuilder): StringBuilder =
		builder.append("}\n")

	private def emitProperties(properties: List[PropertyRecord])(implicit builder: StringBuilder): StringBuilder =
		properties.reverse.map((property: PropertyRecord) =>
			'\t' + property.name + ": " + property.value + ";\n" // TODO handle flags
		).foldLeft(builder)(_.append(_))

	//private def

}
