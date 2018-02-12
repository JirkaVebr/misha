package com.preprocessor.interpreter

import com.preprocessor.TypeScope
import com.preprocessor.ast.{Ast, Symbol}

class RootTypeScope extends TypeScope {


	override def lookup(name: Symbol.TypeSymbol): Option[Symbol.TypeSymbol#Value] =
		RootTypeScope.preDefinedTypes.get(name)

}

object RootTypeScope {
	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Any" -> Ast.Type.Any,
		"Color" -> Ast.Type.Color,
		"Boolean" -> Ast.Type.Boolean,
		"Dimensioned" -> Ast.Type.Dimensioned,
		"Flag" -> Ast.Type.Flag,
		"Formula" -> Ast.Type.Formula,
		"Integer" -> Ast.Type.Integer,
		"Number" -> Ast.Type.Number,
		"Numeric" -> Ast.Type.Numeric,
		"Percentage" -> Ast.Type.Numeric,
		"Rational" -> Ast.Type.Rational,
		"Ratio" -> Ast.Type.Ratio,
		"String" -> Ast.Type.String,
		"Unit" -> Ast.Type.Unit
	)
}
