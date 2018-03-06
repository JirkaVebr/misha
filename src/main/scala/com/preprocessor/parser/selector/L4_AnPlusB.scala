package com.preprocessor.parser.selector

import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import com.preprocessor.spec.PseudoClasses.{AnPlusB => AnB}
import org.parboiled2._

trait L4_AnPlusB { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Basics =>

	import L4_AnPlusB._


	def AnPlusB: Rule1[AnB] = rule {
		(Token("odd") ~ push(AnB(2, 1))) |
		(Token("even") ~ push(AnB(2, 0))) |
		(An ~ SingleLineWhitespace ~ B ~> ((a: Int, b: Int) => AnB(a, b)))
	}

	private def An: Rule1[Int] = rule {
		(Sign ~ (UnsignedInteger | push(1)) ~ 'n' ~> computeValue) | push(0)
	}

	private def B: Rule1[Int] = rule {
		(BSign ~ SingleLineWhitespace ~ (UnsignedInteger | push(0))) ~> computeValue
	}

	private def BSign: Rule1[Int] = rule {
		('-' ~ push(-1)) | (optional('+') ~ push(1))
	}

}

object L4_AnPlusB {
	private val computeValue: (Int, Int) => Int = _ * _
}
