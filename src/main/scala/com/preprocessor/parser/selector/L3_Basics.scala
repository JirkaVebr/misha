package com.preprocessor.parser.selector

import com.preprocessor.ast
import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, Namespace, NoNamespace}
import com.preprocessor.ast.{CssIdentifier, QualifiedAttribute, QualifiedName}
import com.preprocessor.parser.common.{L0_Whitespace, L1_Strings, L2_Numbers}
import org.parboiled2._

trait L3_Basics { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_Strings
	with L2_Numbers =>

	def CssIdentifier: Rule1[CssIdentifier] = rule {
		Identifier ~> ast.CssIdentifier
	}

	def QualifiedAttributeName: Rule1[QualifiedAttribute] = rule {
		qualifiedName ~> (
			(namespace: Option[Namespace], identifier: CssIdentifier) =>
				QualifiedAttribute(identifier, namespace.getOrElse(NoNamespace))
		)
	}

	def qualifiedName: Rule2[Option[Namespace], CssIdentifier] = rule {
		namespace ~ CssIdentifier
	}

	def namespace: Rule1[Option[Namespace]] = rule {
		optional(
			(('*' ~ push(AnyNamespace)) |
			(CssIdentifier ~> NamedNamespace) |
			push(NoNamespace)) ~ '|'
		)
	}


}
