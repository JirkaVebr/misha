package com.mishaLang

import com.mishaLang.ast.Language.Value

package object interpreter {

	type ScopeId = Scope.Id

	type EnvWithValue = EnvironmentWithValue.Env

	type RawRuleHeadComponent = Either[String, Vector[Value.String]]
	type RawRuleHead = Vector[RawRuleHeadComponent]

}
