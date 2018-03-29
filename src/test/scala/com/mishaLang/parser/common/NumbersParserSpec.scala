package com.mishaLang.parser.common

import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.NodePosition
import com.mishaLang.ast.NumberUnit.{Atomic, Percentage}
import com.mishaLang.parser.BaseParserSpec
import com.mishaLang.spec.units.Time.{MiliSecond, Second}

class NumbersParserSpec extends BaseParserSpec {

	behavior of "The number parser"

	it should "parse an integer literal" in {
		assert(parse("+0") === Value.Number(0))
		assert(parse("0") === Value.Number(0))
		assert(parse("0000") === Value.Number(0))
		assert(parse("-0") === Value.Number(0))
		assert(parse("+123") === Value.Number(123))
		assert(parse("123") === Value.Number(123))
		assert(parse("-123") === Value.Number(-123))
	}

	it should "parse an unsigned integer" in {
		assert(parseUnsignedInt("0") === 0)
		assert(parseUnsignedInt("0000") === 0)
		assert(parseUnsignedInt("123") === 123)
	}

	it should "parse a fractional literal" in {
		assert(parse("+0.00") === Value.Number(0))
		assert(parse("0.00") === Value.Number(0))
		assert(parse("-0.00") === Value.Number(0))
		assert(parse("+123.456") === Value.Number(123.456))
		assert(parse("123.456") === Value.Number(123.456))
		assert(parse("-123.456") === Value.Number(-123.456))
	}

	it should "parse a number literal without an integral" in {
		assert(parse("+.0") === Value.Number(0))
		assert(parse("-.0") === Value.Number(0))
		assert(parse("+.123") === Value.Number(.123))
		assert(parse(".456") === Value.Number(.456))
		assert(parse("-.456") === Value.Number(-.456))
	}

	it should "parse a number with an exponent" in {
		assert(parse("10e2") === Value.Number(1000))
		assert(parse("2E2") === Value.Number(200))
		assert(parse("1e0") === Value.Number(1))
		assert(parse("1.2e3") === Value.Number(1200))
		assert(parse("-8.0e2") === Value.Number(-800))
	}

	it should "parse a number with a simple unit" in {
		assert(parse("10e2s") === Value.Number(1000, Atomic(Second)))
		assert(parse("0ms") === Value.Number(0, Atomic(MiliSecond)))
		assert(parse(".3s") === Value.Number(.3, Atomic(Second)))
		assert(parse("-7e2ms") === Value.Number(-700, Atomic(MiliSecond)))
	}

	it should "parse percentage as a number" in {
		assert(parse("+0%") === Value.Number(0, Percentage))
		assert(parse("0%") === Value.Number(0, Percentage))
		assert(parse("123%") === Value.Number(123, Percentage))
		assert(parse("11.22%") === Value.Number(11.22, Percentage))
	}

	it should "parse percentage as a dedicated rule" in {
		assert(parsePercentage("+0%") === Value.Number(0, Percentage))
		assert(parsePercentage("0%") === Value.Number(0, Percentage))
		assert(parsePercentage("123%") === Value.Number(123, Percentage))
		assert(parsePercentage("11.22%") === Value.Number(11.22, Percentage))
	}

	it should "parse number positioning" in {
		assert(parse("1").position === Some(NodePosition(0, 1)))
		assert(parse("123%").position === Some(NodePosition(0, 4)))
		assert(parse("-123.456").position === Some(NodePosition(0, 8)))
		assert(parse("3ms").position === Some(NodePosition(0, 3)))
	}

	protected def parse(input: String): Value.Value = parseLanguageRule(input, _.Number)

	protected def parsePercentage(input: String): Value.Value = parseLanguageRule(input, _.Percentage)

	protected def parseUnsignedInt(input: String): Int = parseLanguageRule(input, _.UnsignedInteger)

}
