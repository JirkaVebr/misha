package com.preprocessor.parser

import org.parboiled2._

trait L0_Whitespace extends org.parboiled2.Parser {
	import L0_Whitespace._

	def AnyWhitespace: Rule0 = rule {
		quiet(zeroOrMore(WhiteSpaceChar))
	}

	def MandatoryAnyWhitespace: Rule0 = rule {
		oneOrMore(WhiteSpaceChar)
	}

	def SingleLineWhitespace: Rule0 = rule {
		quiet(zeroOrMore(SingleLineWhitespaceChar))
	}

	def MandatorySingleLineWhitespace: Rule0 = rule {
		oneOrMore(SingleLineWhitespace)
	}

	def EndOfLine: Rule0 = rule {
		SingleLineWhitespace ~ End
	}

	def SingleLineString(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)) ~ SingleLineWhitespace
	}

	implicit def whitespaceAfterString(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)) ~ AnyWhitespace
	}

}

object L0_Whitespace {
	private val End: CharPredicate = CharPredicate('\n') ++ Characters.EOI
	private val SingleLineWhitespaceChar = CharPredicate(" \f\r\t")
	private val WhiteSpaceChar: CharPredicate = SingleLineWhitespaceChar ++ '\n'
}
