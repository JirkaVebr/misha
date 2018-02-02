package com.preprocessor.parser

import com.preprocessor.ast.Ast.Value
import com.preprocessor.ast.Ast.Value.{Color, Flag, Important, Primitive}
import com.preprocessor.ast.UnitOfMeasure
import com.preprocessor.spec.ColorKeywords
import org.parboiled2._


trait L1_Literals { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics =>

	import CharPredicate.HexDigit
	import L1_Literals._


	def Literal: Rule1[Primitive] = rule {
		flag | boolean | number | quotedString | colorKeyword | unquotedString
	}

	private def flag: Rule1[Flag] = rule {
		atomic("!important") ~> (() => Important)
	}

	private def boolean: Rule1[Value.Boolean] = rule {
		(capture(atomic("true")) | capture(atomic("false"))) ~> ((literal: String) => Value.Boolean(literal == "true"))
	}


	/* NUMBERS */

	private def number: Rule1[Value.Number] = rule {
		(base ~ optional(exponent) ~ optional(unitOfMeasure) ~ whitespace) ~> (createNumber(_, _, _))
	}

	private def integral: Rule1[Double] = rule {
		digits ~> ((digits: String) => digits.toDouble)
	}

	private def fractional: Rule1[Double] = rule {
		'.' ~ digits ~> (digitsToFractional(_))
	}

	private def base: Rule1[Double] = rule {
		((sign ~ integral ~ optional(fractional)) ~> (computeBase(_, _, _))) |
		(sign ~ fractional ~> ((sign: Int, fractional: Double) => computeBase(sign, 0, Some(fractional))))
	}

	private def sign: Rule1[Int] = rule {
		capture(optional(Signs)) ~> (signToFactor(_))
	}

	private def digits: Rule1[String] = rule {
		capture(oneOrMore(CharPredicate.Digit))
	}

	private def exponent: Rule1[Double] = rule {
		(Exponent ~ sign ~ digits) ~> ((sign: Int, digits: String) => sign * digits.toDouble)
	}

	private def unitOfMeasure: Rule1[UnitOfMeasure] = rule {
		capture(oneOrMore(CharPredicate.Alpha)) ~> ((unitOfMeasure: String) => UnitOfMeasure(Map(unitOfMeasure -> 1)))
	}



	/* COLORS */

	private def colorKeyword: Rule1[Color] = rule {
		valueMap(ColorKeywords.map, ignoreCase = true) ~ whitespace
	}


	/* STRINGS */
	// The string parsing is heavily based on the parboiled2 example Json parser

	private def quotedString: Rule1[Value.String] = rule {
		(('\'' ~ clearSB() ~ singleQuotedStrChars ~ '\'' ~ whitespace ~ push(sb.toString)) |
		('"' ~ clearSB() ~ doubleQuotedStrChars ~ '"' ~ whitespace ~ push(sb.toString))) ~> Value.String
	}

	private def singleQuotedStrChars: Rule0 = rule {
		zeroOrMore(normalSingleQuotedStrChar | '\\' ~ escapedChar)
	}

	private def normalSingleQuotedStrChar: Rule0 = rule {
		!SingleQuoteOrBackslash ~ ANY ~ appendSB()
	}

	private def doubleQuotedStrChars: Rule0 = rule {
		zeroOrMore(normalDoubleQuotedStrChar | '\\' ~ escapedChar)
	}

	private def normalDoubleQuotedStrChar: Rule0 = rule {
		!DoubleQuoteOrBackslash ~ ANY ~ appendSB()
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

	private def unquotedString: Rule1[Value.String] = rule {
		capture(oneOrMore(AlphaNumUnderscore) ~ zeroOrMore(AlphaNumDashUnderscore)) ~> Value.String
	}

}

object L1_Literals {
	val Signs = CharPredicate("+-")
	val Exponent = CharPredicate("eE")
	val AlphaNumUnderscore: CharPredicate = CharPredicate.AlphaNum ++ '_'
	val AlphaNumDashUnderscore: CharPredicate = AlphaNumUnderscore ++ '-'
	val SingleQuoteOrBackslash = CharPredicate("'\\")
	val DoubleQuoteOrBackslash = CharPredicate("\"\\")
	val StringDelimiterOrBackslash = CharPredicate("\"\\'")


	private def signToFactor(sign: String): Int = if (sign == "-") -1 else 1

	private def digitsToFractional(digits: String): Double = {
		val digitsCount = digits.length
		if (digitsCount == 0) 0
		else digits.toDouble / Math.pow(10, digitsCount)
	}

	private def computeBase(sign: Int, integral: Double, fractional: Option[Double]): Double =
		sign * (integral + fractional.getOrElse(0.0d))

	private def createNumber(base: Double, exponent: Option[Double], unitOfMeasure: Option[UnitOfMeasure]): Value.Number =
		Value.Number(
			if (exponent.isEmpty) base else base * Math.pow(10, exponent.get),
			unitOfMeasure.getOrElse(UnitOfMeasure())
		)
}
