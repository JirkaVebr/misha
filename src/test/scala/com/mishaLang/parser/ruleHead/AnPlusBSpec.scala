package com.mishaLang.parser.ruleHead

import com.mishaLang.parser.BaseParserSpec
import com.mishaLang.spec.PseudoClasses.AnPlusB

class AnPlusBSpec extends BaseParserSpec {

	behavior of "An + B parser"

	it should "correctly parse keywords" in {
		assert(parse("even") === AnPlusB(2, 0))
		assert(parse("odd") === AnPlusB(2, 1))
	}

	it should "correctly parse standalone B constants" in {
		assert(parse("2") === AnPlusB(0, 2))
		assert(parse("123") === AnPlusB(0, 123))
	}

	it should "correctly parse standalone A constants" in {
		assert(parse("5n") === AnPlusB(5, 0))
		assert(parse("-n") === AnPlusB(-1, 0))
		assert(parse("-4n") === AnPlusB(-4, 0))
		assert(parse("n") === AnPlusB(1, 0))
		assert(parse("+n") === AnPlusB(1, 0))
		assert(parse("+6n") === AnPlusB(6, 0))
		assert(parse("+0n") === AnPlusB(0, 0))
		assert(parse("0n") === AnPlusB(0, 0))
	}

	it should "correctly parse full An+B expressions" in {
		assert(parse("3n+4") === AnPlusB(3, 4))
		assert(parse("n-2") === AnPlusB(1, -2))
		assert(parse("-4n+5") === AnPlusB(-4, 5))
	}

	it should "allow whitespace at appropriate places" in {
		assert(parse("2n + 4") === AnPlusB(2, 4))
		assert(parse("-2n+ 1") === AnPlusB(-2, 1))
		assert(parse("5n +1") === AnPlusB(5, 1))
	}


	protected def parse(input: String): AnPlusB = parseSelectorRule(input, _.AnPlusB)

}
