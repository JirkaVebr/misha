package com.preprocessor.parser.selector

import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.{CssIdentifier, QualifiedName}
import com.preprocessor.parser.BaseParserSpec

class BasicsSpec extends BaseParserSpec {


	behavior of "Basic selector parsing"

	it should "correctly parse qualified names" in {
		assert(parseQn("foo|A") === QualifiedName(CssIdentifier("A"), Some(NamedNamespace(CssIdentifier("foo")))))
		assert(parseQn("|B") === QualifiedName(CssIdentifier("B"), Some(NoNamespace)))
		assert(parseQn("*|C") === QualifiedName(CssIdentifier("C"), Some(AnyNamespace)))
		assert(parseQn("D") === QualifiedName(CssIdentifier("D"), None))
	}


	protected def parseQn(input: String): QualifiedName = parseSelectorRule(input, _.QualifiedName)

}
