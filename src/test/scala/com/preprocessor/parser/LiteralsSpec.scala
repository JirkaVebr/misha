package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{CurrentColor, Duplicate, Important, Rgba, Transparent}
import com.preprocessor.ast.NumberUnit.UnitOfMeasure

class LiteralsSpec extends BaseParserSpec {

	behavior of "The literal parser"

	it should "parse a flag keyword" in {
		assert(parse("!important") == Important)
		assert(parse("!duplicate") == Duplicate)
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
		assert(parse(".3s") == Value.Dimensioned(.3, UnitOfMeasure(Map("s" -> 1))))
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


	protected def parse(input: String): Value.Value = parseRule(input, _.Literal)
}
