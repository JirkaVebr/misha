package com.mishaLang.error

import com.mishaLang.interpreter.Environment
import org.parboiled2

case class RuleHeadParseError(override val parseError: parboiled2.ParseError, environment: Environment)
	extends ParseError(parseError) {

}
