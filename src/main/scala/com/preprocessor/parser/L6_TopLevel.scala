package com.preprocessor.parser

import com.preprocessor.ast.Ast
import org.parboiled2._

trait L6_TopLevel { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Literals
	with L3_Types
	with L4_Expressions =>

	def Program: Rule1[Ast.Program] = rule {
		AnyWhitespace ~ Expression ~ AnyWhitespace ~ EOI ~> Ast.Program
	}

}
