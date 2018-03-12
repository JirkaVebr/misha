package com.preprocessor.parser.language

import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Language.Value.{Color, Duplicate, Flag, Important, Primitive, Rgba}
import com.preprocessor.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import com.preprocessor.spec.ColorKeywords
import org.parboiled2._


trait L3_Literals { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers =>

	import CharPredicate.HexDigit
	import L3_Literals._


	def Literal: Rule1[Primitive] = rule {
		Flag | boolean | Number | QuotedString | color | UnquotedString
	}

	def Flag: Rule1[Flag] = rule {
		'!' ~ ((Token("important") ~ push(Important)) |
			(Token("duplicate") ~ push(Duplicate)))
	}

	private def boolean: Rule1[Value.Boolean] = rule {
		(capture(atomic("true")) | capture(atomic("false"))) ~>
			((literal: String) => Value.Boolean(literal.charAt(0) == 't'))
	}


	/* COLORS */

	private def color: Rule1[Color] = rule {
		hexColor | colorKeyword
	}

	private def hexColor: Rule1[Color] = rule {
		'#' ~ capture(
			8.times(HexDigit) |
			6.times(HexDigit) |
			(3 to 4).times(HexDigit)
		) ~> (convertHexToColor(_))
	}

	private def colorKeyword: Rule1[Color] = rule {
		valueMap(ColorKeywords.map, ignoreCase = true)
	}


}

object L3_Literals {
	private def convertHexToColor(hex: String): Rgba = {
		val isShort = hex.length == 3 || hex.length == 4
		val hasAlpha = hex.length == 4 || hex.length == 8
		val h2d = (c1: Char, c2: Char) => java.lang.Integer.parseInt(s"$c1$c2", 16)
		val norm: Vector[Int] =
			if (isShort) hex.toVector.map((c: Char) => h2d(c, c))
			else hex.toVector.grouped(2).map((pxs: Vector[Char]) => h2d(pxs(0), pxs(1))).toVector

		Rgba(norm(0), norm(1), norm(2), if (hasAlpha) norm(3) else 255)
	}
}
