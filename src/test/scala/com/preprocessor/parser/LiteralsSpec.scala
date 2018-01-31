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
				fail(failure.traces.asInstanceOf[String])
			case a =>
				fail(a.asInstanceOf[String])
		}
	}
}
