package com.preprocessor.error

import com.preprocessor.interpreter.Environment
import org.parboiled2

case class RuleHeadParseError(override val parseError: parboiled2.ParseError, environment: Environment)
	extends ParseError(parseError) {

}
