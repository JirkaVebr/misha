package com.preprocessor.emitter

import com.preprocessor.ast.PropertyRecord
import com.preprocessor.interpreter.Symbol.{PropertySymbol, RuleContextSymbol}
import com.preprocessor.interpreter.Environment

class Emitter(val finalEnvironment: Environment) {


	def emit(): StringBuilder =
		emitEnvironment(finalEnvironment)(StringBuilder.newBuilder)

	private def emitEnvironment(environment: Environment)(implicit builder: StringBuilder): StringBuilder = {
		val properties = environment.lookupCurrent(PropertySymbol).getOrElse(List.empty)
		val ruleContextSymbol = environment.lookupCurrent(RuleContextSymbol)

		if (ruleContextSymbol.nonEmpty)
			emitRuleStart(ruleContextSymbol.get)

		emitProperties(properties)

		if (environment.subEnvironments.nonEmpty)
			emitSubEnvironments(environment)

		if (ruleContextSymbol.nonEmpty)
			emitRuleEnd(ruleContextSymbol.get)
		else
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

	private def emitSubEnvironments(environment: Environment)(implicit builder: StringBuilder): StringBuilder =
		environment.subEnvironments.foldLeft(builder)((builder, environment: Environment) =>
			emitEnvironment(environment)(builder)
		)

	//private def

}
