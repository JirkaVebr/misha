package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol.{RuleContextSymbol, TypeSymbol, ValueSymbol}
import com.preprocessor.ast.{Ast, Symbol}

class RootEnvironment extends Environment {


	override def lookup(name: Symbol.Symbol): Option[name.Value] = name match {
		case TypeSymbol(typeName) => RootEnvironment.preDefinedTypes.get(typeName).asInstanceOf[Option[name.Value]]
		case ValueSymbol(_) => None // TODO
		case RuleContextSymbol => None
	}

}

object RootEnvironment {
	val preDefinedTypes: Map[String, Symbol.TypeSymbol#Value] = Map(
		"Any" -> Ast.Type.Any,
		"Color" -> Ast.Type.Color,
		"Boolean" -> Ast.Type.Boolean,
		"Flag" -> Ast.Type.Flag,
		"Formula" -> Ast.Type.Formula,
		"Number" -> Ast.Type.Number,
		"Numeric" -> Ast.Type.Numeric,
		"Percentage" -> Ast.Type.Numeric,
		"Scalar" -> Ast.Type.Scalar,
		"String" -> Ast.Type.String,
		"Unit" -> Ast.Type.Unit
	)
}
