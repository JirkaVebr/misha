package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol

object Constants {

	lazy final val Constants: Map[String, Symbol.ValueSymbol#Value] = Map(
		"E" -> Value.Scalar(Math.E),
		"GOLDEN_RATIO" -> Value.Scalar((1d + Math.sqrt(5d)) / 2d), // Golden ratio
		"PI" -> Value.Scalar(Math.PI),
		"TAU" -> Value.Scalar(2 * Math.PI)
	)

}
