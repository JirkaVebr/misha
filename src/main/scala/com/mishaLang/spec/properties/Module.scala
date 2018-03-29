package com.mishaLang.spec.properties

import com.mishaLang.ast.Language.Expression.Block
import com.mishaLang.ast.Language.Statement.Property
import com.mishaLang.ast.Language.Term.Variable
import com.mishaLang.ast.Language.{Type, Value, ValueSymbolDeclaration}
import com.mishaLang.interpreter.Scope

trait Module {

	def apply(): Map[String, Value.Callable]

	final val UnaryArgumentVariable: String = "arg"

	protected def generateUnary(argumentType: Type.Any): Value.Lambda =
		Value.Lambda(
			None, Vector(ValueSymbolDeclaration[Unit](UnaryArgumentVariable, Some(argumentType), Unit)), Vector(),
			None, Block(
				Property(UnaryArgumentVariable, Variable(UnaryArgumentVariable))
			), Scope.rootScopeId
		)

}
