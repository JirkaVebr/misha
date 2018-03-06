package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.parser.L1_Strings._
import org.parboiled2.CharPredicate.HexDigit
import org.parboiled2._

/**
	* The string parsing is heavily based on the parboiled2 example Json parser
	*
	* @see https://github.com/sirthias/parboiled2/blob/release-2.1/examples/src/main/scala/org/parboiled2/examples/JsonParser.scala
	*/
trait L1_Strings { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace =>

	def String: Rule1[Value.String] = rule {
		QuotedString | UnquotedString
	}

	def QuotedString: Rule1[Value.String] = rule {
		quotedStringInner(quotedStringBody)
	}

	private val quotedStringBody: CharPredicate => Rule1[String] = (charPredicate: CharPredicate) => rule {
		clearSB() ~ zeroOrMore((!charPredicate ~ ANY ~ appendSB()) | '\\' ~ escapedChar) ~ push(sb.toString)
	}

	private def quotedStringInner(body: (CharPredicate) => Rule1[String]): Rule1[Value.String] = rule {
		(('\'' ~ body(SingleQuoteOrBackslash) ~ '\'') |
			('"' ~ body(DoubleQuoteOrBackslash) ~ '"')
			) ~> Value.String
	}

	private def escapedChar: Rule0 = rule (
		StringDelimiterOrBackslash ~ appendSB()
			| 'n' ~ appendSB('\n')
			| 'r' ~ appendSB('\r')
			| 't' ~ appendSB('\t')
			| unicodeSequence ~> { (code: Int) => sb.append(code.asInstanceOf[Char]); () }
	)

	private def unicodeSequence: Rule1[Int] = rule {
		capture((1 to 4).times(HexDigit)) ~> ((hexDigits: String) => java.lang.Integer.parseInt(hexDigits, 16))
	}


	def UnquotedString: Rule1[Value.String] = rule {
		Identifier ~> Value.String
	}

	def Identifier: Rule1[String] = rule {
		capture(optional("--") ~ oneOrMore(AlphaNumUnderscore) ~ zeroOrMore(AlphaNumDashUnderscore))
	}

}

object L1_Strings {
	val AlphaNumUnderscore: CharPredicate = CharPredicate.AlphaNum ++ '_'
	val AlphaNumDashUnderscore: CharPredicate = AlphaNumUnderscore ++ '-'
	val SingleQuoteOrBackslash = CharPredicate("'\\")
	val DoubleQuoteOrBackslash = CharPredicate("\"\\")
	val StringDelimiterOrBackslash = CharPredicate("\"\\'")
}
