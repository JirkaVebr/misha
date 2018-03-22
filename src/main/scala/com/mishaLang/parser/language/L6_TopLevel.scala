package com.mishaLang.parser.language

import com.mishaLang.ast.Language
import com.mishaLang.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import org.parboiled2._

trait L6_TopLevel { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers
	with L3_Literals
	with L4_Types
	with L5_Expressions =>

	def Program: Rule1[Language.Program] = rule {
		AnyWhitespace ~ Expression ~ AnyWhitespace ~ EOI ~> Language.Program
	}

}
