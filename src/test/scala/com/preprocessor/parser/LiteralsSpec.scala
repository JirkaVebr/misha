package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{CurrentColor, Important, Rgba, Transparent}
import com.preprocessor.ast.UnitOfMeasure
import org.parboiled2.ParseError

import scala.util.{Failure, Success}

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

	it should "parse a number literal without an integral" in {
		assert(parse("+.0") == Value.Number(0))
		assert(parse("-.0") == Value.Number(0))
		assert(parse("+.123") == Value.Number(.123))
		assert(parse(".456") == Value.Number(.456))
		assert(parse("-.456") == Value.Number(-.456))
	}

	it should "parse a number with an exponent" in {
		assert(parse("10e2") == Value.Number(1000))
		assert(parse("2E2") == Value.Number(200))
		assert(parse("1e0") == Value.Number(1))
		assert(parse("1.2e3") == Value.Number(1200))
		assert(parse("-8.0e2") == Value.Number(-800))
	}

	it should "parse a number with a simple unit" in {
		assert(parse("10e2s") == Value.Number(1000, UnitOfMeasure(Map("s" -> 1))))
		assert(parse("0km") == Value.Number(0, UnitOfMeasure(Map("km" -> 1))))
		assert(parse("-7e2ms") == Value.Number(-700, UnitOfMeasure(Map("ms" -> 1))))
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

	it should "parse quoted strings" in {
		assert(parse("\"abc\"") == Value.String("abc"))
		assert(parse("'def'") == Value.String("def"))
		assert(parse("\"g'h'i\"") == Value.String("g'h'i"))
		assert(parse("'j\"k\"l'") == Value.String("j\"k\"l"))
		assert(parse("\"m\\'n\\'o\"") == Value.String("m'n'o")) // Needlessly escaped
		assert(parse("'p\\\"q\\\"r'") == Value.String("p\"q\"r"))
		assert(parse(""""\"\'\\\n\r\t"""") == Value.String("\"\'\\\n\r\t"))
	}

	it should "parse unquoted strings" in {
		assert(parse("abc") == Value.String("abc"))
		assert(parse("d_-_e-_-f") == Value.String("d_-_e-_-f"))
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
