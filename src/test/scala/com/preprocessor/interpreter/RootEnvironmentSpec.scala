package com.preprocessor.interpreter

import com.preprocessor.ast.Language.{Type, Value}
import com.preprocessor.ast.Language.Type.Literal
import com.preprocessor.ast.Symbol.TypeSymbol

class RootEnvironmentSpec extends BaseInterpreterSpec {

	behavior of "Root environment"

	val environment = RootEnvironment()

	it should "retrieve builtin types from the spec" in {
		assert(environment.lookup(TypeSymbol("BlendMode")).get
			.asInstanceOf[Type.Union].subs.contains(Literal(Value.String("hue")))
		)
	}

}
