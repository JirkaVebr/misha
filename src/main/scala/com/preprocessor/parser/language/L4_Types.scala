package com.preprocessor.parser.language

import com.preprocessor.ast.Language
import com.preprocessor.interpreter.RootEnvironment
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import org.parboiled2._

trait L4_Types { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers
	with L3_Literals =>

	def TypeAnnotation: Rule1[Option[Language.Type.Any]] = rule {
		optional(":" ~!~ Type)
	}

	def Type: Rule1[Language.Type.Any] = rule {
		/* intersectionType | */ subtractionType
	}

	private def subtractionType: Rule1[Language.Type.Any] = rule {
		unionType ~ optional(
			AnyWhitespaceAround("--") ~!~ unionType ~> Language.Type.Subtraction
		)
	}

	private def unionType: Rule1[Language.Type.Any] = rule {
		oneOrMore(facultativeType).separatedBy(AnyWhitespaceAround("|")) ~> (
			(subtypes: Seq[Language.Type.Any]) => if (subtypes.lengthCompare(1) == 0) subtypes.head else Language.Type.Union(subtypes.toSet)
		)
	}

	private def facultativeType: Rule1[Language.Type.Any] = rule {
		nonCompoundType ~ optional(Token("?") ~> (
			(nonCompoundType: Language.Type.Any) => Language.Type.Union(Set[Language.Type.Any](Language.Type.Unit) + nonCompoundType))
		)
	}

	private def nonCompoundType: Rule1[Language.Type.Any] = rule {
		simpleCompositeType | nonCompositeType | TypeAlias | literalType
	}

	private def nonCompositeType: Rule1[Language.Type.Any] = rule {
		valueMap(RootEnvironment.preDefinedTypes)
	}

	def TypeAlias: Rule1[Language.Type.TypeAlias] = rule {
		capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) ~> (
			(alias: String) => Language.Type.TypeAlias(alias)
		)
	}

	private def literalType: Rule1[Language.Type.Literal] = rule {
		Literal ~> Language.Type.Literal
	}

	private def simpleCompositeType: Rule1[Language.Type.Composite] = rule {
		functionType | tuple2Type
	}

	private def functionType: Rule1[Language.Type.Function] = rule {
		('(' ~ AnyWhitespace ~ zeroOrMore(Type).separatedBy(AnyWhitespaceAround(",")) ~
			optional(AnyWhitespace ~ ',') ~ AnyWhitespaceAround(")") ~ Token("=>") ~ AnyWhitespace ~ Type) ~> Language.Type.Function
	}

	private def tuple2Type: Rule1[Language.Type.Tuple2] = rule {
		"(" ~ Type ~ "," ~ Type ~ ")" ~> Language.Type.Tuple2
	}


}
