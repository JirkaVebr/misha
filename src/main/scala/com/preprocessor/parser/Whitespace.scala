package com.preprocessor.parser

import org.parboiled2._

trait Whitespace extends org.parboiled2.Parser {
	import Whitespace._

	def AnyWhitespace: Rule0 = rule {
		quiet(zeroOrMore(WhiteSpaceChar))
	}

	def MandatoryAnyWhitespace: Rule0 = rule {
		oneOrMore(WhiteSpaceChar)
	}

	def StartOfLine: Rule0 = rule {
		zeroOrMore(SingleLineWhitespace)
	}

	def EndOfLine: Rule0 = rule {
		StartOfLine ~ '\n'
	}

	implicit def whitespaceAfterString(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)) ~ AnyWhitespace
	}

}

object Whitespace {
	val SingleLineWhitespace = CharPredicate(" \f\r\t")
	val WhiteSpaceChar: CharPredicate = SingleLineWhitespace ++ '\n'
}
