package com.preprocessor.parser.selector

import com.preprocessor.ast
import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.{CssIdentifier, QualifiedAttribute}
import com.preprocessor.parser.BaseParserSpec

class BasicsSpec extends BaseParserSpec {


	behavior of "Basic selector parsing"

	it should "correctly parse qualified names" in {
		assert(parseQn("foo|A") === QualifiedAttribute(CssIdentifier("A"), NamedNamespace(CssIdentifier("foo"))))
		assert(parseQn("|B") === ast.QualifiedAttribute(CssIdentifier("B"), NoNamespace))
		assert(parseQn("*|C") === ast.QualifiedAttribute(CssIdentifier("C"), AnyNamespace))
		assert(parseQn("D") === ast.QualifiedAttribute(CssIdentifier("D"), NoNamespace))
	}


	protected def parseQn(input: String): QualifiedAttribute = parseSelectorRule(input, _.QualifiedAttributeName)

}
