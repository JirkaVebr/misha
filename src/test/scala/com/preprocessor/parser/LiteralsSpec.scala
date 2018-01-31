package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{CurrentColor, Important, Rgba, Transparent}
import org.parboiled2.ParseError

import scala.util.{Failure, Success}

class LiteralsSpec extends BaseParserSpec {

	//import org.parboiled2.Parser.DeliveryScheme.Throw

	behavior of "The literal parser"

	it should "parse a flag keyword" in {
		assert(parse("!important") == Important)
	}

	it should "parse a boolean literal" in {
		assert(parse("true") == Value.Boolean(true))
		assert(parse("false") == Value.Boolean(false))
	}

	it should "parse an integer literal" in {
		assert(parse("+0") == Value.Number(0))
		assert(parse("0") == Value.Number(0))
		assert(parse("0000") == Value.Number(0))
		assert(parse("-0") == Value.Number(0))
		assert(parse("+123") == Value.Number(123))
		assert(parse("123") == Value.Number(123))
		assert(parse("-123") == Value.Number(-123))
	}

	it should "parse a fractional literal" in {
		assert(parse("+0.00") == Value.Number(0))
		assert(parse("0.00") == Value.Number(0))
		assert(parse("-0.00") == Value.Number(0))
		assert(parse("+123.456") == Value.Number(123.456))
		assert(parse("123.456") == Value.Number(123.456))
		assert(parse("-123.456") == Value.Number(-123.456))
	}

	it should "parse a literal without an integral" in {
		assert(parse("+.0") == Value.Number(0))
		assert(parse("-.0") == Value.Number(0))
		assert(parse("+.123") == Value.Number(.123))
		assert(parse(".456") == Value.Number(.456))
		assert(parse("-.456") == Value.Number(-.456))
	}

	it should "parse a number with an exponent" in {
		assert(parse("10e2") == Value.Number(100))
		assert(parse("2E2") == Value.Number(4))
		assert(parse("1e0") == Value.Number(1))
		assert(Math.round(parse("1.2e3").asInstanceOf[Value.Number].value * 1000.0) == 1728)
		assert(parse("-8.0e2") == Value.Number(64))
	}

	it should "parse a keyword rgba color" in {
		assert(parse("black") == Rgba(0, 0, 0, 0))
		assert(parse("navy") == Rgba(0, 0, 128, 0))
		assert(parse("white") == Rgba(255, 255, 255, 0))
	}

	it should "parse non-rgba color keywords" in {
		assert(parse("currentColor") == CurrentColor)
		assert(parse("transparent") == Transparent)
	}

	it should "ignore case while parsing color keywords" in {
		assert(parse("cUrReNtCoLor") == CurrentColor)
		assert(parse("transPARent") == Transparent)
		assert(parse("RED") == Rgba(255, 0, 0, 0))
	}


	protected def parse(input: String, failMessage: => String = ""): Value.Value = {
		val parser = this.getParser(input)

		parser.Literal.run() match {
			case Success(result) => result
			case Failure(failure: ParseError) =>
				println(failure.traces)
				fail()
			case a =>
				fail(a.asInstanceOf[String])
		}
	}
}
