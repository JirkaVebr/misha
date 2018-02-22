package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{CurrentColor, Important, Rgba, Transparent}
import com.preprocessor.ast.NumberUnit.{UnitOfMeasure, Percentage}

class LiteralsSpec extends BaseParserSpec {

	behavior of "The literal parser"

	it should "parse a flag keyword" in {
		assert(parse("!important") == Important)
	}

	it should "parse a boolean literal" in {
		assert(parse("true") == Value.Boolean(true))
		assert(parse("false") == Value.Boolean(false))
	}

	it should "parse an integer literal" in {
		assert(parse("+0") == Value.Scalar(0))
		assert(parse("0") == Value.Scalar(0))
		assert(parse("0000") == Value.Scalar(0))
		assert(parse("-0") == Value.Scalar(0))
		assert(parse("+123") == Value.Scalar(123))
		assert(parse("123") == Value.Scalar(123))
		assert(parse("-123") == Value.Scalar(-123))
	}

	it should "parse a fractional literal" in {
		assert(parse("+0.00") == Value.Scalar(0))
		assert(parse("0.00") == Value.Scalar(0))
		assert(parse("-0.00") == Value.Scalar(0))
		assert(parse("+123.456") == Value.Scalar(123.456))
		assert(parse("123.456") == Value.Scalar(123.456))
		assert(parse("-123.456") == Value.Scalar(-123.456))
	}

	it should "parse a number literal without an integral" in {
		assert(parse("+.0") == Value.Scalar(0))
		assert(parse("-.0") == Value.Scalar(0))
		assert(parse("+.123") == Value.Scalar(.123))
		assert(parse(".456") == Value.Scalar(.456))
		assert(parse("-.456") == Value.Scalar(-.456))
	}

	it should "parse a number with an exponent" in {
		assert(parse("10e2") == Value.Scalar(1000))
		assert(parse("2E2") == Value.Scalar(200))
		assert(parse("1e0") == Value.Scalar(1))
		assert(parse("1.2e3") == Value.Scalar(1200))
		assert(parse("-8.0e2") == Value.Scalar(-800))
	}

	it should "parse a number with a simple unit" in {
		assert(parse("10e2s") == Value.Dimensioned(1000, UnitOfMeasure(Map("s" -> 1))))
		assert(parse("0km") == Value.Dimensioned(0, UnitOfMeasure(Map("km" -> 1))))
		assert(parse("-7e2ms") == Value.Dimensioned(-700, UnitOfMeasure(Map("ms" -> 1))))
	}

	it should "parse a percentage" in {
		assert(parse("+0%") == Value.Percentage(0))
		assert(parse("0%") == Value.Percentage(0))
		assert(parse("123%") == Value.Percentage(123))
		assert(parse("11.22%") == Value.Percentage(11.22))
	}

	it should "parse a hex color" in {
		assert(parse("#123") == Rgba(17, 34, 51))
		assert(parse("#1234") == Rgba(17, 34, 51, 68))
		assert(parse("#123123") == Rgba(18, 49, 35))
		assert(parse("#12341234") == Rgba(18, 52, 18, 52))
	}

	it should "parse a keyword rgba color" in {
		assert(parse("black") == Rgba(0, 0, 0))
		assert(parse("navy") == Rgba(0, 0, 128))
		assert(parse("white") == Rgba(255, 255, 255))
	}

	it should "parse non-rgba color keywords" in {
		assert(parse("currentColor") == CurrentColor)
		assert(parse("transparent") == Transparent)
	}

	it should "ignore case while parsing color keywords" in {
		assert(parse("cUrReNtCoLor") == CurrentColor)
		assert(parse("transPARent") == Transparent)
		assert(parse("RED") == Rgba(255, 0, 0))
	}

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


	protected def parse(input: String): Value.Value = parseRule(input, _.Literal)
}
