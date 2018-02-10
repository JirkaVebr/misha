package com.preprocessor.ast

import com.preprocessor.ast.Ast.{Type, Value}

object Symbol {

	/*sealed trait Symbol[V] extends Any {
		type Value = V
	}

	case class TypeSymbol(underlying: String) extends AnyVal with Symbol[Ast.Type.Any]

	case class ValueSymbol(underlying: String) extends AnyVal with Symbol[Ast.Value.Value]*/

	sealed trait Symbol[V] extends Any {
		type Value = V
	}

	case class TypeSymbol(underlying: String) extends AnyVal with Symbol[Type.Any]

	case class ValueSymbol(underlying: String) extends AnyVal with Symbol[Value.Value]

}
