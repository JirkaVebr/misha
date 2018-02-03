package com.preprocessor.parser

import org.parboiled2._

trait Whitespace extends org.parboiled2.Parser {
	import Whitespace._

	def whitespace: Rule0 = rule {
		quiet(zeroOrMore(WhiteSpaceChar))
	}

	implicit def whitespaceAfterString(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)) ~ whitespace
	}

}

object Whitespace {
	val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
}
