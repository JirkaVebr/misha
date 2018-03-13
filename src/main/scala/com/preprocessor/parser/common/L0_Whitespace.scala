package com.preprocessor.parser.common

import com.preprocessor.parser.Characters
import org.parboiled2._

trait L0_Whitespace extends org.parboiled2.Parser {
	import L0_Whitespace._

	def AnyWhitespace: Rule0 = rule {
		quiet(zeroOrMore(AnyWhiteSpaceChar))
	}

	def MandatoryAnyWhitespace: Rule0 = rule {
		oneOrMore(AnyWhiteSpaceChar)
	}

	def SingleLineWhitespace: Rule0 = rule {
		quiet(zeroOrMore(SingleLineWhitespaceChar))
	}

	def MandatorySingleLineWhitespace: Rule0 = rule {
		oneOrMore(SingleLineWhitespaceChar)
	}

	def MultiLineWhitespace: Rule0 = rule {
		quiet(zeroOrMore(MultiLineWhitespaceChar))
	}

	def MandatoryMultiLineWhitespace: Rule0 = rule {
		oneOrMore(MultiLineWhitespaceChar)
	}

	def EndOfLine: Rule0 = rule {
		SingleLineWhitespace ~ End
	}

	def SingleLineString(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)) ~ SingleLineWhitespace
	}

	def Token(stringToken: String): Rule0 = rule {
		atomic(str(stringToken)).named(stringToken)
	}

	def IgnoreCaseToken(stringToken: String): Rule0 = rule {
		atomic(ignoreCase(stringToken)).named(stringToken)
	}

	def WhitespaceAround(operator: String): Rule0 = rule {
		SingleLineWhitespace ~ Token(operator) ~ SingleLineWhitespace
	}

	def AnyWhitespaceAround(operator: String): Rule0 = rule {
		AnyWhitespace ~ Token(operator) ~ AnyWhitespace
	}

	def NotAfterWhitespace: Rule0 = rule {
		test(!AnyWhiteSpaceChar.matchesAny(lastChar.toString))
	}

	implicit def whitespaceAfterString(stringToken: String): Rule0 = rule {
		(Token(stringToken) ~ SingleLineWhitespace).named(stringToken)
	}

}

object L0_Whitespace {
	import Characters._

	private val End: CharPredicate = CharPredicate('\n') ++ Characters.EOI
	private val SingleLineWhitespaceChar = CharPredicate(" \f\r\t")
	private val MultiLineWhitespaceChar: CharPredicate = SingleLineWhitespaceChar ++ "\n"
	private val AnyWhiteSpaceChar: CharPredicate = MultiLineWhitespaceChar ++ CharPredicate(s"$INDENT$DEDENT")
}
