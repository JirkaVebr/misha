package com.preprocessor

import com.preprocessor.ast.Language.Expression.Expression

package object ast {

	@inline implicit def string2CssIdentifier(string: String): CssIdentifier = CssIdentifier(string)
	@inline implicit def cssIdentifier2String(identifier: CssIdentifier): String = identifier.value


	type RuleHeadComponent = Either[String, Seq[Expression]]
	type RuleHead = Seq[RuleHeadComponent]


}
