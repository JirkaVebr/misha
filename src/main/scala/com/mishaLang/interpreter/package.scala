package com.mishaLang

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.PropertyRecord
import com.mishaLang.utils.LinkedMap

package object interpreter {

	type ScopeId = Scope.Id

	type EnvWithValue = EnvironmentWithValue.Env

	type RawRuleHeadComponent = Either[String, Vector[Value.String]]
	type RawRuleHead = Vector[RawRuleHeadComponent]

	type PropertyStore = LinkedMap[String, List[PropertyRecord]]
	type RuleStore = LinkedMap[Symbol.RuleContextSymbol.Value, PropertyStore]

}
