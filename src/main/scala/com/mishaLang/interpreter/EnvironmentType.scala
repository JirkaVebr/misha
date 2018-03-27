package com.mishaLang.interpreter

object EnvironmentType {
	sealed trait EnvironmentType

	case object FunctionEnvironment extends EnvironmentType
	case object RuleEnvironment extends EnvironmentType
	case object ScopeEnvironment extends EnvironmentType
}
