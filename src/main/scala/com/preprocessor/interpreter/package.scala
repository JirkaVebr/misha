package com.preprocessor

import com.preprocessor.ast.Language.Value

package object interpreter {

	type RawRuleHeadComponent = Either[String, Vector[Value.String]]
	type RawRuleHead = Vector[RawRuleHeadComponent]

}
