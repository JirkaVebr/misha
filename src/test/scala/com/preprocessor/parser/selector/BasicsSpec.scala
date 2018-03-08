package com.preprocessor.parser.selector

import com.preprocessor.ast
import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.QualifiedAttribute
import com.preprocessor.parser.BaseParserSpec

class BasicsSpec extends BaseParserSpec {


	behavior of "Basic selector parsing"

	it should "correctly parse qualified names" in {
		assert(parseQn("foo|A") === QualifiedAttribute("A", NamedNamespace("foo")))
		assert(parseQn("|B") === ast.QualifiedAttribute("B", NoNamespace))
		assert(parseQn("*|C") === ast.QualifiedAttribute("C", AnyNamespace))
		assert(parseQn("D") === ast.QualifiedAttribute("D", NoNamespace))
	}


	protected def parseQn(input: String): QualifiedAttribute = parseSelectorRule(input, _.QualifiedAttributeName)

}
