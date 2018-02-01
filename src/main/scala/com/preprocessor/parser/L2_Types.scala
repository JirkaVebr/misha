package com.preprocessor.parser

import com.preprocessor.ast.Ast
import org.parboiled2._

trait L2_Types { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics
	with L1_Literals =>


	def Type: Rule1[Ast.Type.Any] = rule {
		/* intersectionType | */ subtractionType
	}

	private def subtractionType: Rule1[Ast.Type.Any] = rule {
		unionType ~ optional(
			"--" ~ unionType ~> Ast.Type.Subtraction
		)
	}

	private def unionType: Rule1[Ast.Type.Any] = rule {
		oneOrMore(nonCompoundType).separatedBy("|") ~> (
			(subtypes: Seq[Ast.Type.Any]) => if (subtypes.lengthCompare(1) == 0) subtypes.head else Ast.Type.Union(subtypes.toSet)
		)
	}

	private def nonCompoundType: Rule1[Ast.Type.Any] = rule {
		simpleCompositeType | primitiveType | literalType
	}

	private def primitiveType: Rule1[Ast.Type.Primitive] = rule {
		valueMap(Map(
			"Boolean" -> Ast.Type.Boolean,
			"Integer" -> Ast.Type.Integer,
			"String" -> Ast.Type.String,
			"Unit" -> Ast.Type.Unit
		)) ~ whitespace
	}

	private def literalType: Rule1[Ast.Type.Literal] = rule {
		Literal ~> Ast.Type.Literal
	}

	private def simpleCompositeType: Rule1[Ast.Type.Composite] = rule {
		functionType | tuple2Type
	}

	// This syntax error is just IntelliJ being stupid; to the compiler this is fine.
	private def functionType: Rule1[Ast.Type.Function] = rule {
		("(" ~ zeroOrMore(Type).separatedBy(",") ~ optional(",") ~ ")" ~ "=>" ~ Type) ~> Ast.Type.Function
	}

	private def tuple2Type: Rule1[Ast.Type.Tuple2] = rule {
		"(" ~ Type ~ "," ~ Type ~ ")" ~> Ast.Type.Tuple2
	}


}
