package com.mishaLang.spec.properties

import com.mishaLang.ast.Language.{Type, Value}
import com.mishaLang.interpreter.Symbol
import com.mishaLang.interpreter.Symbol.ValueSymbol

object Display extends Module {


	override def apply(): Map[Symbol.ValueSymbol, Value.Callable] = Map(
		ValueSymbol("display") -> generateUnary("display", "Display"),
	)
}
