package com.mishaLang.spec.properties

import com.mishaLang.ast.Language.Value

trait Module {

	def apply(): Map[String, Value.Callable]

}
