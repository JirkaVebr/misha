package com.preprocessor.interpreter.typing

import com.preprocessor.ast.Ast.Type

object Subtype {

	def isSubtypeOf(left: Type.Any, right: Type.Any): Boolean = true // TODO

	def isEquivalentTo(left: Type.Any, right: Type.Any): Boolean = left == right // TODO

	def getLowestCommonSupertype(left: Type.Any, right: Type.Any): Type.Any = left // TODO

}
