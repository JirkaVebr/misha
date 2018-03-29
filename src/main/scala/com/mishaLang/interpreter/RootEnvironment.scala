package com.mishaLang.interpreter

import com.mishaLang.ast.Language
import com.mishaLang.interpreter.Symbol.{RuleStoreSymbol, TypeSymbol, ValueSymbol}
import com.mishaLang.spec.properties._
import com.mishaLang.spec.types._
import com.mishaLang.utils.LinkedMap

class RootEnvironment extends Environment


object RootEnvironment {

	lazy final val RootEnvironmentSymbolTable: Map[Symbol.Symbol, Symbol.Symbol#Value] =
		Map(RuleStoreSymbol -> LinkedMap.empty[Symbol.RuleContextSymbol.Value, PropertyStore].asInstanceOf[RuleStoreSymbol.Value]) ++
		RootEnvironment.PreDefinedValues.asInstanceOf[Map[Symbol.Symbol, Symbol.Symbol#Value]] ++
		RootEnvironment.PreDefinedTypes.asInstanceOf[Map[Symbol.Symbol, Symbol.Symbol#Value]]

	def apply(): RootEnvironment =
		new RootEnvironment

	lazy final val PreDefinedTypes: Map[TypeSymbol, Symbol.TypeSymbol#Value] = Map(
		TypeSymbol("Angle") -> Language.Type.Angle,
		TypeSymbol("Any") -> Language.Type.Any,
		TypeSymbol("Color") -> Language.Type.Color,
		TypeSymbol("Boolean") -> Language.Type.Boolean,
		TypeSymbol("Flag") -> Language.Type.Flag,
		TypeSymbol("Flex") -> Language.Type.Flex,
		TypeSymbol("Frequency") -> Language.Type.Frequency,
		TypeSymbol("Length") -> Language.Type.Length,
		TypeSymbol("Percentage") -> Language.Type.Percentage,
		TypeSymbol("Resolution") -> Language.Type.Resolution,
		TypeSymbol("Scalar") -> Language.Type.Scalar,
		TypeSymbol("String") -> Language.Type.String,
		TypeSymbol("Time") -> Language.Type.Time,
		TypeSymbol("Unit") -> Language.Type.Unit
	) ++ SpecTypes


	lazy final val PreDefinedValues: Map[ValueSymbol, Symbol.ValueSymbol#Value] =
		SpecProperties ++
		builtin.Builtins
}
