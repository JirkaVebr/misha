package com.preprocessor.parser.common

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.NumberUnit.UnitOfMeasure
import com.preprocessor.parser.BaseParserSpec

class NumbersParserSpec extends BaseParserSpec {

	behavior of "The number parser"

	it should "parse an integer literal" in {
		assert(parse("+0") == Value.Scalar(0))
		assert(parse("0") == Value.Scalar(0))
		assert(parse("0000") == Value.Scalar(0))
		assert(parse("-0") == Value.Scalar(0))
		assert(parse("+123") == Value.Scalar(123))
		assert(parse("123") == Value.Scalar(123))
		assert(parse("-123") == Value.Scalar(-123))
	}

	it should "parse an unsigned integer" in {
		assert(parseUnsignedInt("0") == 0)
		assert(parseUnsignedInt("0000") == 0)
		assert(parseUnsignedInt("123") == 123)
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

	protected def parse(input: String): Value.Value = parseRule(input, _.Number)

	protected def parseUnsignedInt(input: String): Int = parseRule(input, _.UnsignedInteger)

}
