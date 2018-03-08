package com.preprocessor.ast

import com.preprocessor.ast.Namespace.{Namespace, NoNamespace}
import com.preprocessor.spec.HtmlElements.HtmlElement

/**
	* @see https://www.w3.org/TR/css3-namespace/#css-qnames
	*/
sealed abstract class QualifiedName


case class QualifiedElement(element: HtmlElement, namespace: Option[Namespace] = None) extends QualifiedName

/**
	*
	* @param name The attribute name
	* @param namespace It is not Option since [|att] === [att] as per the spec.
	*/
case class QualifiedAttribute(name: CssIdentifier, namespace: Namespace = NoNamespace) extends QualifiedName
