package com.mishaLang.parser.language

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.Language.Value.{CurrentColor, Rgba, Transparent}
import com.mishaLang.ast.NodePosition
import com.mishaLang.parser.BaseParserSpec

class LiteralsSpec extends BaseParserSpec {

	behavior of "The literal parser"

	it should "parse a flag keyword" in {
		assert(parse("!important") === Value.Important)
		assert(parse("!duplicate") === Value.Duplicate)
	}

	it should "parse a boolean literal" in {
		assert(parse("true") === Value.Boolean(true))
		assert(parse("false") === Value.Boolean(false))
	}

	it should "parse a hex color" in {
		assert(parse("#123") === Rgba(17, 34, 51))
		assert(parse("#1234") === Rgba(17, 34, 51, 68))
		assert(parse("#123123") === Rgba(18, 49, 35))
		assert(parse("#12341234") === Rgba(18, 52, 18, 52))
	}

	it should "parse a keyword rgba color" in {
		assert(parse("black") === Rgba(0, 0, 0))
		assert(parse("navy") === Rgba(0, 0, 128))
		assert(parse("white") === Rgba(255, 255, 255))
	}

	it should "parse non-rgba color keywords" in {
		assert(parse("currentColor") === CurrentColor)
		assert(parse("transparent") === Transparent)
	}

	it should "ignore case while parsing color keywords" in {
		assert(parse("cUrReNtCoLor") === CurrentColor)
		assert(parse("transPARent") === Transparent)
		assert(parse("RED") === Rgba(255, 0, 0))
	}

	it should "track positioning of flags" in {
		assert(parse("!important").position === Some(NodePosition(0, 10)))
	}

	it should "track positioning of booleans" in {
		assert(parse("false").position === Some(NodePosition(0, 5)))
	}

	it should "track positioning of colors" in {
		assert(parse("#fff").position === Some(NodePosition(0, 4)))
		assert(parse("#abcdef").position === Some(NodePosition(0, 7)))
		assert(parse("currentColor").position === Some(NodePosition(0, 12)))
		assert(parse("red").position === Some(NodePosition(0, 3)))
	}


	protected def parse(input: String): Value.Value = parseLanguageRule(input, _.Literal)
}
