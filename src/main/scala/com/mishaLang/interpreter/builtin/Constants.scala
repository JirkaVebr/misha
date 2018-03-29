package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol

object Constants {

	lazy final val Constants: Map[String, Symbol.ValueSymbol#Value] = Map(
		"E" -> Value.Number(Math.E),
		"GOLDEN_RATIO" -> Value.Number((1d + Math.sqrt(5d)) / 2d), // Golden ratio
		"PI" -> Value.Number(Math.PI),
		"TAU" -> Value.Number(2 * Math.PI)
	)

}
