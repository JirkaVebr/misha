package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.Language.Value.Value

object EnvironmentWithValue {

	type Env = AugmentedEnvironment[Value]

	def apply(environment: Environment, value: Value = Value.Unit): Env =
		new AugmentedEnvironment[Value](environment, value)

}
