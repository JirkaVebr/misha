package com.mishaLang.interpreter

import Symbol.{PropertySymbol, RuleContextSymbol, TypeSymbol, ValueSymbol}
import com.mishaLang.ast.Language
import com.mishaLang.spec.types._

class RootEnvironment extends Environment {


	override def lookupCurrent(name: Symbol.Symbol): Option[name.Value] = name match {
		case TypeSymbol(typeName) => RootEnvironment.preDefinedTypes.get(typeName).asInstanceOf[Option[name.Value]]
		case ValueSymbol(_) => None // TODO
		case PropertySymbol => None
		case RuleContextSymbol => None
	}

}

object RootEnvironment {

	def apply(): RootEnvironment =
		new RootEnvironment

	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Any" -> Language.Type.Any,
		"Color" -> Language.Type.Color,
		"Boolean" -> Language.Type.Boolean,
		"Flag" -> Language.Type.Flag,
		"Percentage" -> Language.Type.Percentage,
		"Scalar" -> Language.Type.Scalar,
		"String" -> Language.Type.String,
		"Unit" -> Language.Type.Unit
	) ++ specTypes
}
