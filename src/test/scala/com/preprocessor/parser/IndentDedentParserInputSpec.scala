package com.preprocessor.parser

import com.preprocessor.error.InputError

class IndentDedentParserInputSpec extends BaseParserSpec {

	import Characters._

	behavior of "The indent/dedent parser input"

	it should "strip empty lines" in {
		assert(process(
			"""
				|a
				|
				|ab
				|
				|""".stripMargin) ==
			"""a
				|ab
				|""".stripMargin)
	}

	it should "close unclosed indents" in {
		assert(process("a\n	abc") == s"a\n${INDENT}abc\n$DEDENT\n")
	}

	it should "convert several initial tabs to one indent" in {
		assert(process("a\n			abc") == s"a\n${INDENT}abc\n$DEDENT\n")
	}

	it should "convert several initial spaces to one indent" in {
		assert(process("a\n            abc") == s"a\n${INDENT}abc\n$DEDENT\n")
	}

	it should "correctly add indents & dedents" in {
		assert(process(
			"""
				|a
				|	b
				|	c
				|d
				|""".stripMargin) ==
			s"""a
				|${INDENT}b
				|c
				|${DEDENT}d
				|""".stripMargin)
	}

	it should "treat a tab as four spaces" in {
		assert(
			process(
				"""a
					|	b
					|    c
					|""".stripMargin) ==
				s"""a
					|${INDENT}b
					|c
					|$DEDENT
					|""".stripMargin)
	}

	it should "handle nested blocks" in {
		assert(process(
			"""a
				|	b
				|		c
				|	d
				|e
				|""".stripMargin) ==
			s"""a
				|${INDENT}b
				|${INDENT}c
				|${DEDENT}d
				|${DEDENT}e
				|""".stripMargin)
	}

	it should "throw an error upon encountering illegal indentation" in {
		assertThrows[InputError](process(
			"""a
				|	b
				|   c
				|""".stripMargin))
	}

	it should "strip comments at the beginning of a line" in {
		assert(process("// foo bar baz") == "\n")
		assert(process(
			"""a
				|// whatever
				|b
				|""".stripMargin) == "a\nb\n")
	}

	it should "strip comments at the end of a line" in {
		assert(process("content// comment") == "content\n")
	}

	// TODO This is not implemented yet
	/*it should "preserve comment within delimited strings" in {
		assert(process("'string//string'") == "'string//string'\n")
		assert(process("\"string//string\"") == "\"string//string\"\n")
	}

	it should "not get fooled by escape sequences within strings" in {
		assert(process("\"string\\\"//string\"") == "\"string\\\"//string\"\n")
		assert(process("'string\\'//string'") == "'string\\'//string'\n")
	}*/

	it should "strip block comments" in {
		assert(process(
			"""a
				|	// block
				|		comment
				|		comment
				|	b
				|""".stripMargin) == s"a\n${INDENT}b\n$DEDENT\n")
	}

	it should "not get fooled by nested block comments" in {
		assert(process(
			"""a
				|	// block
				|		comment
				|		comment
				|		// nested //
				|		foo
				|	b
				|""".stripMargin) == s"a\n${INDENT}b\n$DEDENT\n")
	}

	def process(input: String): String = {
		val parserInput = new IndentDedentParserInput(input)

		parserInput.sliceCharArray(0, parserInput.length).mkString
	}

}
