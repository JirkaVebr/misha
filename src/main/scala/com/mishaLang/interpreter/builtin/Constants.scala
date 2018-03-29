package com.mishaLang.interpreter.builtin

import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol
import com.mishaLang.interpreter.Symbol.ValueSymbol

object Constants {

	lazy final val Constants: Map[ValueSymbol, Symbol.ValueSymbol#Value] = Map(
		ValueSymbol("E") -> Value.Number(Math.E),
		ValueSymbol("GOLDEN_RATIO") -> Value.Number((1d + Math.sqrt(5d)) / 2d), // Golden ratio
		ValueSymbol("PI") -> Value.Number(Math.PI),
		ValueSymbol("TAU") -> Value.Number(2 * Math.PI)
	)

}
