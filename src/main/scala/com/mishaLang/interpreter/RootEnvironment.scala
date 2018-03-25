package com.mishaLang.interpreter

import Symbol.{PropertySymbol, RuleContextSymbol, TypeSymbol, ValueSymbol}
import com.mishaLang.ast.Language
import com.mishaLang.spec.types._

class RootEnvironment extends Environment {


	override def lookupCurrent(name: Symbol.Symbol): Option[name.Value] = name match {
		case TypeSymbol(typeName) => RootEnvironment.PreDefinedTypes.get(typeName).asInstanceOf[Option[name.Value]]
		case ValueSymbol(_) => None // TODO
		case PropertySymbol => None
		case RuleContextSymbol => None
	}

}

object RootEnvironment {

	def apply(): RootEnvironment =
		new RootEnvironment

	lazy final val PreDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Angle" -> Language.Type.Angle,
		"Any" -> Language.Type.Any,
		"Color" -> Language.Type.Color,
		"Boolean" -> Language.Type.Boolean,
		"Flag" -> Language.Type.Flag,
		"Flex" -> Language.Type.Flex,
		"Frequency" -> Language.Type.Frequency,
		"Length" -> Language.Type.Length,
		"Percentage" -> Language.Type.Percentage,
		"Resolution" -> Language.Type.Resolution,
		"Scalar" -> Language.Type.Scalar,
		"String" -> Language.Type.String,
		"Time" -> Language.Type.Time,
		"Unit" -> Language.Type.Unit
	) ++ SpecTypes
}
