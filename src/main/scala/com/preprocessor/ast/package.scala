package com.preprocessor

import com.preprocessor.ast.Language.Expression.Expression
import com.preprocessor.ast.Language.Value

package object ast {

	@inline implicit def string2CssIdentifier(string: String): CssIdentifier = CssIdentifier(string)
	@inline implicit def cssIdentifier2String(identifier: CssIdentifier): String = identifier.value

	@inline implicit def string2Value(string: String): Value.String = Value.String(string)


	type RuleHeadComponent = Either[String, Expression]
	type RuleHead = Vector[RuleHeadComponent]


}
