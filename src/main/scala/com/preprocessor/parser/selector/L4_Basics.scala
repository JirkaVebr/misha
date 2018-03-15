package com.preprocessor.parser.selector

import com.preprocessor.ast
import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, Namespace, NoNamespace}
import com.preprocessor.ast.{CssIdentifier, QualifiedAttribute, QualifiedElement, QualifiedName}
import com.preprocessor.parser.common.{L0_Whitespace, L1_AstNode, L2_Strings, L3_Numbers}
import com.preprocessor.spec.HtmlElements
import com.preprocessor.spec.HtmlElements.{AnyElement, CustomElement}
import org.parboiled2._

trait L4_Basics { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace
	with L1_AstNode
	with L2_Strings
	with L3_Numbers =>

	def CssIdentifier: Rule1[CssIdentifier] = rule {
		Identifier ~> ast.CssIdentifier
	}

	def QualifiedAttributeName: Rule1[QualifiedAttribute] = rule {
		qualifiedName ~> (
			(namespace: Option[Namespace], identifier: CssIdentifier) =>
				QualifiedAttribute(identifier, namespace.getOrElse(NoNamespace))
		)
	}

	def QualifiedElementName: Rule1[QualifiedElement] = rule {
		namespace ~ (
			(ch('*') ~> (
				(namespace: Option[Namespace]) => QualifiedElement(AnyElement, namespace)
			)) |
			(CssIdentifier ~> (
				(namespace: Option[Namespace], identifier: CssIdentifier) =>
					QualifiedElement(HtmlElements.htmlElements.getOrElse(
						identifier.value, CustomElement(identifier.value)
					), namespace)
				))
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
