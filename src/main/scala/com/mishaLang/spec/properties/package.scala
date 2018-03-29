package com.mishaLang.spec

import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.Symbol.ValueSymbol

package object properties {


	lazy final val SpecProperties: Map[ValueSymbol, Value.Callable] =
		BoxModel() ++
		Color() ++
		Display()
}
