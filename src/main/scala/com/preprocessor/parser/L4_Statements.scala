package com.preprocessor.parser

import com.preprocessor.ast.Ast
import com.preprocessor.ast.Ast.Expression._
import com.preprocessor.ast.Ast.Statement._
import com.preprocessor.ast.Ast.Term.Variable
import com.preprocessor.ast.Ast.Type
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

	private def standalone(statement: () => Rule1[Statement]): Rule1[Statement] = rule {
		StartOfLine ~ statement() ~ EndOfLine
	}

	private def sequenceNode: Rule1[Statement] = rule {
		standalone(typeAliasDeclaration) | rule | standalone(variableDeclaration) | standalone(expression) | noOp
	}

	private def rule: Rule1[Ast.Statement.Rule] = rule { // TODO using quoted strings is temporary
		(QuotedString ~ block) ~> Ast.Statement.Rule
	}

	private def block: Rule1[Statement] = rule { // TODO replace '{' and '}' by INDENT and DEDENT respectively
		'{' ~ EndOfLine ~ Statement ~ '}' ~ EndOfLine
	}

	private def noOp: Rule1[Statement] = rule {
		optional(EndOfLine) ~ push(NoOp)
	}


	// Standalone statements

	private val typeAliasDeclaration: () => Rule1[TypeAliasDeclaration] = () => rule {
		("@type" ~ TypeAlias ~ "=" ~ Type) ~> TypeAliasDeclaration
	}

	private val variableDeclaration: () => Rule1[VariableDeclaration] = () => rule {
		(Variable ~ optional(":" ~ Type) ~ "=" ~ Expression) ~> (
			(variable: Variable, typeAnnotation: Option[Ast.Type.Any], value: Expression) =>
				VariableDeclaration(variable.name, typeAnnotation, value)
		)
	}

	private val expression: () => Rule1[Expression] = () => rule {
		Expression
	}

}
