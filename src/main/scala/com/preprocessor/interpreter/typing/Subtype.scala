package com.preprocessor.interpreter.typing

import com.preprocessor.ast.Ast.Type

object Subtype {

	// TODO (when this is done, uncomment the test in StatementInterpreterSpec)
	def isSubtypeOf(left: Type.Any, right: Type.Any): Boolean = left == right

	def isEquivalentTo(left: Type.Any, right: Type.Any): Boolean = left == right // TODO

	def getLowestCommonSupertype(left: Type.Any, right: Type.Any): Type.Any = left // TODO

}
