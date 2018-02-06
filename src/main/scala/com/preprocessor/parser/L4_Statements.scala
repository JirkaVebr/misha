package com.preprocessor.parser

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.{Expression, Statement}
import com.preprocessor.ast.Ast.Statement._
import org.parboiled2._

trait L4_Statements { this: org.parboiled2.Parser
		with StringBuilding
		with Whitespace
		with L0_Basics
		with L1_Literals
		with L2_Types
		with L3_Expressions =>

	def Statement: Rule1[Statement] = rule {
		sequence
	}

	private def sequence: Rule1[Sequence] = rule {
		sequenceNode ~ sequenceNode ~> Sequence
	}

	private def sequenceNode: Rule1[Statement] = rule {
		standalone(foo) | standalone(typeAliasDeclaration) | rule | noOp
	}

	val foo = () => rule {
		"foo" ~ push(FunctionDeclaration("foo", None, NoOp))
	}

	private def standalone(statement: () => Rule1[Statement]): Rule1[Statement] = rule {
		StartOfLine ~ statement() ~ EndOfLine
	}

	private val typeAliasDeclaration: () => Rule1[TypeAliasDeclaration] = () => rule {
		("@type" ~ TypeAlias ~ "=" ~ Type) ~> TypeAliasDeclaration
	}

	private def rule: Rule1[Ast.Statement.Rule] = rule {
		(QuotedString ~ block) ~> Ast.Statement.Rule
	}

	private def block: Rule1[Statement] = rule { // TODO replace '{' and '}' by INDENT and DEDENT respectively
		'{' ~ EndOfLine ~ Statement ~ '}' ~ EndOfLine
	}

	private def noOp: Rule1[Statement] = rule {
		push(NoOp)
	}
}
