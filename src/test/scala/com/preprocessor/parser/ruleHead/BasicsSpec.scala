package com.preprocessor.parser.ruleHead

import com.preprocessor.ast
import com.preprocessor.ast.Namespace.{AnyNamespace, NamedNamespace, NoNamespace}
import com.preprocessor.ast.QualifiedAttribute
import com.preprocessor.parser.BaseParserSpec

class BasicsSpec extends BaseParserSpec {


	behavior of "Basic selector parsing"

	it should "correctly parse qualified names" in {
		assert(parseQan("foo|A") === QualifiedAttribute("A", NamedNamespace("foo")))
		assert(parseQan("|B") === ast.QualifiedAttribute("B", NoNamespace))
		assert(parseQan("*|C") === ast.QualifiedAttribute("C", AnyNamespace))
		assert(parseQan("D") === ast.QualifiedAttribute("D", NoNamespace))
	}


	protected def parseQan(input: String): QualifiedAttribute = parseSelectorRule(input, _.QualifiedAttributeName)

}
