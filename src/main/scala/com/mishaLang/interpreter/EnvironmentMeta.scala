package com.mishaLang.interpreter

import com.mishaLang.interpreter.EnvironmentType.EnvironmentType

case class EnvironmentMeta(id: ScopeId, environmentType: EnvironmentType) {

	def ~>(newId: ScopeId): EnvironmentMeta =
		EnvironmentMeta(newId, environmentType)
}
