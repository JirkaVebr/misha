package com.preprocessor.parser.language

import com.preprocessor.ast.Ast
import com.preprocessor.parser._
import org.parboiled2._

trait L6_TopLevel { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals
	with L4_Types
	with L5_Expressions =>

	def Program: Rule1[Ast.Program] = rule {
		AnyWhitespace ~ Expression ~ AnyWhitespace ~ EOI ~> Ast.Program
	}

}
