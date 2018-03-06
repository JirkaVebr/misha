package com.preprocessor.parser.common

import com.preprocessor.ast.Ast.Value
import com.preprocessor.parser.BaseParserSpec
import com.preprocessor.parser.language.LanguageParser

class StringsParserSpec extends BaseParserSpec {

	behavior of "The strings parser"

	it should "parse quoted strings" in {
		assert(parse("\"abc\"") == Value.String("abc"))
		assert(parse("'def'") == Value.String("def"))
		assert(parse("\"g'h'i\"") == Value.String("g'h'i"))
		assert(parse("'j\"k\"l'") == Value.String("j\"k\"l"))
		assert(parse("\"m\\'n\\'o\"") == Value.String("m'n'o")) // Needlessly escaped
		assert(parse("'p\\\"q\\\"r'") == Value.String("p\"q\"r"))
	}

	it should "parse complex escape sequences" in {
		assert(parse(""""\"\'\\\n\r\t"""") == Value.String("\"\'\\\n\r\t"))
		assert(parse("\"\\2026\\B1\"") == Value.String("…±"))
	}

	it should "parse unquoted strings" in {
		assert(parse("abc") == Value.String("abc"))
		assert(parse("d_-_e-_-f") == Value.String("d_-_e-_-f"))
	}

	protected def parse(input: String): Value.Value = parseLanguageRule(input, _.String)

}
