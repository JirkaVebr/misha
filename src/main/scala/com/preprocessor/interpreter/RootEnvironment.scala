package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol.{Context, TypeSymbol, ValueSymbol}
import com.preprocessor.ast.{Ast, Symbol}

class RootEnvironment extends Environment {


	override def lookup(name: Symbol.Symbol): Option[name.Value] = name match {
		case TypeSymbol(typeName) => RootEnvironment.preDefinedTypes.get(typeName).asInstanceOf[Option[name.Value]]
		case ValueSymbol(_) => None // TODO
		case Context => None
	}

}

object RootEnvironment {
	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Any" -> Ast.Type.Any,
		"Color" -> Ast.Type.Color,
		"Boolean" -> Ast.Type.Boolean,
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
