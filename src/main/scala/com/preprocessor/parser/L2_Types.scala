package com.preprocessor.parser

import com.preprocessor.ast.Ast
import org.parboiled2._

trait L2_Types { this: org.parboiled2.Parser
	with StringBuilding
	with Whitespace
	with L0_Basics
	with L1_Literals =>


	def Type: Rule1[Ast.Type.Any] = rule {
		/* intersectionType | */ unionType
	}

	private def unionType: Rule1[Ast.Type.Any] = rule {
		oneOrMore(nonCompoundType).separatedBy("|") ~> (
			(subtypes: Seq[Ast.Type.Any]) => if (subtypes.lengthCompare(1) == 0) subtypes.head else Ast.Type.Union(subtypes.toSet)
		)
	}

	private def nonCompoundType: Rule1[Ast.Type.Any] = rule {
		compositeType | primitiveType
	}

	private def primitiveType: Rule1[Ast.Type.Primitive] = rule {
		(atomic("Unit") ~> (() => Ast.Type.Unit)) |
		(atomic("Boolean") ~> (() => Ast.Type.Boolean)) |
		(atomic("String") ~> (() => Ast.Type.String)) |
		(atomic("Integer") ~> (() => Ast.Type.Integer))
	}

	private def compositeType: Rule1[Ast.Type.Composite] = rule {
		functionType | tuple2Type //| unionType
	}

	// This syntax error is just IntelliJ being stupid; to the compiler this is fine.
	private def functionType: Rule1[Ast.Type.Function] = rule {
		("(" ~ zeroOrMore(Type).separatedBy(",") ~ optional(",") ~ ")" ~ "=>" ~ Type) ~> Ast.Type.Function
	}

	private def tuple2Type: Rule1[Ast.Type.Tuple2] = rule {
		"(" ~ Type ~ "," ~ Type ~ ")" ~> Ast.Type.Tuple2
	}


}
