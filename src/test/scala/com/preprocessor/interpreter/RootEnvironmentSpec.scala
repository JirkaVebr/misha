package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Type
import com.preprocessor.ast.Language.Type.Literal
import com.preprocessor.ast.Symbol.TypeSymbol

class RootEnvironmentSpec extends BaseInterpreterSpec {

	behavior of "Root environment"

	val environment = RootEnvironment()

	it should "retrieve builtin types from the spec" in {
		assert(isBuiltinUnionPresent("Animatable", "margin"))
		assert(isBuiltinUnionPresent("BlendMode", "hue"))
	}


	private def isBuiltinUnionPresent(name: String, exampleValue: String) =
		environment.lookup(TypeSymbol(name)).get
			.asInstanceOf[Type.Union].subs.contains(Literal(exampleValue))

}
