package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol.{PropertySymbol, RuleContextSymbol, TypeSymbol, ValueSymbol}
import com.preprocessor.ast.{Language, Symbol}

class RootEnvironment extends Environment {


	override def lookup(name: Symbol.Symbol): Option[name.Value] = name match {
		case TypeSymbol(typeName) => RootEnvironment.preDefinedTypes.get(typeName).asInstanceOf[Option[name.Value]]
		case ValueSymbol(_) => None // TODO
		case PropertySymbol => None
		case RuleContextSymbol => None
	}

}

object RootEnvironment {
	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Any" -> Language.Type.Any,
		"Color" -> Language.Type.Color,
		"Boolean" -> Language.Type.Boolean,
		"Flag" -> Language.Type.Flag,
		"Percentage" -> Language.Type.Percentage,
		"Scalar" -> Language.Type.Scalar,
		"String" -> Language.Type.String,
		"Unit" -> Language.Type.Unit
	)
}
