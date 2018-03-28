package com.mishaLang.interpreter

import com.mishaLang.ast.Language
import com.mishaLang.ast.Language.Value
import com.mishaLang.interpreter.RuleContext.RuleContext

object Symbol {

	sealed trait Symbol {
		type Value
	}

	case class TypeSymbol(typeName: String) extends Symbol {
		override type Value = Language.Type.Any
	}

	case class ValueSymbol(valueName: String) extends Symbol {
		override type Value = Value.Value
	}

	case object RuleStoreSymbol extends Symbol {
		override type Value = RuleStore
	}

	case object RuleContextSymbol extends Symbol {
		override type Value = RuleContext
	}

}
