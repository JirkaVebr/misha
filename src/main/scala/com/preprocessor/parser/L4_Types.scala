package com.preprocessor.parser

import com.preprocessor.ast.Ast
import com.preprocessor.interpreter.RootEnvironment
import org.parboiled2._

trait L4_Types { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals =>

	def TypeAnnotation: Rule1[Option[Ast.Type.Any]] = rule {
		optional(":" ~!~ Type)
	}

	def Type: Rule1[Ast.Type.Any] = rule {
		/* intersectionType | */ subtractionType
	}

	private def subtractionType: Rule1[Ast.Type.Any] = rule {
		unionType ~ optional(
			AnyWhitespaceAround("--") ~!~ unionType ~> Ast.Type.Subtraction
		)
	}

	private def unionType: Rule1[Ast.Type.Any] = rule {
		oneOrMore(facultativeType).separatedBy(AnyWhitespaceAround("|")) ~> (
			(subtypes: Seq[Ast.Type.Any]) => if (subtypes.lengthCompare(1) == 0) subtypes.head else Ast.Type.Union(subtypes.toSet)
		)
	}

	private def facultativeType: Rule1[Ast.Type.Any] = rule {
		nonCompoundType ~ optional(Token("?") ~> (
			(nonCompoundType: Ast.Type.Any) => Ast.Type.Union(Set[Ast.Type.Any](Ast.Type.Unit) + nonCompoundType))
		)
	}

	private def nonCompoundType: Rule1[Ast.Type.Any] = rule {
		simpleCompositeType | nonCompositeType | TypeAlias | literalType
	}

	private def nonCompositeType: Rule1[Ast.Type.Any] = rule {
		valueMap(RootEnvironment.preDefinedTypes)
	}

	def TypeAlias: Rule1[Ast.Type.TypeAlias] = rule {
		capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> (
			(alias: String) => Ast.Type.TypeAlias(alias)
		)
	}

	private def literalType: Rule1[Ast.Type.Literal] = rule {
		Literal ~> Ast.Type.Literal
	}

	private def simpleCompositeType: Rule1[Ast.Type.Composite] = rule {
		functionType | tuple2Type
	}

	// This syntax error is just IntelliJ being stupid; to the compiler this is fine.
	private def functionType: Rule1[Ast.Type.Function] = rule {
		('(' ~ AnyWhitespace ~ zeroOrMore(Type).separatedBy(AnyWhitespaceAround(",")) ~
			optional(AnyWhitespace ~ ',') ~ AnyWhitespaceAround(")") ~ Token("=>") ~ AnyWhitespace ~ Type) ~> Ast.Type.Function
	}

	private def tuple2Type: Rule1[Ast.Type.Tuple2] = rule {
		"(" ~ Type ~ "," ~ Type ~ ")" ~> Ast.Type.Tuple2
	}


}
