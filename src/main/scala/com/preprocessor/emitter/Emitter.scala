package com.preprocessor.emitter

import com.preprocessor.ast.PropertyRecord
import com.preprocessor.ast.Symbol.{PropertySymbol, RuleContextSymbol}
import com.preprocessor.interpreter.Environment

class Emitter(val finalEnvironment: Environment) {


	def emit(): StringBuilder =
		emitEnvironment(StringBuilder.newBuilder, finalEnvironment)

	private def emitEnvironment(builder: StringBuilder, environment: Environment): StringBuilder = {
		val properties = environment.lookupCurrent(PropertySymbol).getOrElse(List.empty)
		val ruleContextSymbol = environment.lookupCurrent(RuleContextSymbol)

		if (ruleContextSymbol.nonEmpty)
			emitRuleStart(builder, ruleContextSymbol.get)

		emitProperties(builder, properties)

		if (environment.subEnvironments.nonEmpty)
			emitSubEnvironments(builder, environment)

		if (ruleContextSymbol.nonEmpty)
			emitRuleEnd(builder, ruleContextSymbol.get)
		else
			builder
	}

	private def emitRuleStart(builder: StringBuilder, head: RuleContextSymbol.Value): StringBuilder =
		RuleHeadEmitter.emit(builder, head).append(" {\n")

	private def emitRuleEnd(builder: StringBuilder, head: RuleContextSymbol.Value): StringBuilder =
		builder.append("}\n")

	private def emitProperties(builder: StringBuilder, properties: List[PropertyRecord]): StringBuilder =
		properties.reverse.map((property: PropertyRecord) =>
			'\t' + property.name + ": " + property.value + ";\n" // TODO handle flags
		).foldLeft(builder)(_.append(_))

	private def emitSubEnvironments(builder: StringBuilder, environment: Environment): StringBuilder =
		environment.subEnvironments.foldLeft(builder)(emitEnvironment)

	//private def

}
