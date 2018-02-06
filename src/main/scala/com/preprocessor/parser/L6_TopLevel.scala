package com.preprocessor.parser

import com.preprocessor.ast.Ast
import org.parboiled2._

trait L6_TopLevel { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics
	with L1_Literals
	with L2_Types
	with L3_Expressions
	with L4_Statements =>

	def Program: Rule1[Ast.Program] = rule {
		AnyWhitespace ~ Expression ~ AnyWhitespace ~ EOI ~> Ast.Program
	}

}
