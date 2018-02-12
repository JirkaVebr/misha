package com.preprocessor.ast

import com.preprocessor.ast.Selector.Selector

object Symbol {

	sealed trait Symbol {
		type Value
	}

	case class TypeSymbol(typeName: String) extends Symbol {
		override type Value = Ast.Type.Any
	}

	case class ValueSymbol(valueName: String) extends Symbol {
		override type Value = Ast.Value.Value
	}

	case object Selector extends Symbol {
		override type Value = Selector
	}

}
