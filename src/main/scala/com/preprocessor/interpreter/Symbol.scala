package com.preprocessor.interpreter

import com.preprocessor.ast.Language.Value
import com.preprocessor.ast.{Language, PropertyRecord}
import com.preprocessor.interpreter.RuleContext.RuleContext

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

	case object PropertySymbol extends Symbol {
		override type Value = List[PropertyRecord]
	}

	case object RuleContextSymbol extends Symbol {
		override type Value = RuleContext
	}

}
