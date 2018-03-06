package com.preprocessor.ast

import com.preprocessor.ast.Namespace.Namespace

/**
	* @see https://www.w3.org/TR/css3-namespace/#css-qnames
	*/
case class QualifiedName(identifier: CssIdentifier, namespace: Option[Namespace] = None)
