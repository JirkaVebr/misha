package com.mishaLang.spec.properties

import com.mishaLang.ast.Language.Expression.Block
import com.mishaLang.ast.Language.Statement.Property
import com.mishaLang.ast.Language.Term.Variable
import com.mishaLang.ast.Language.{Type, Value, ValueSymbolDeclaration}
import com.mishaLang.interpreter.Scope
import com.mishaLang.interpreter.Symbol.ValueSymbol

trait Module {

	def apply(): Map[ValueSymbol, Value.Callable]


	protected def generateUnary(name: String, argumentType: Type.Any): Value.Lambda =
		Value.Lambda(
			None, Vector(ValueSymbolDeclaration[Unit](name, Some(argumentType), Unit)), Vector(),
			None, Block(
				Property(name, Variable(name))
			), Scope.rootScopeId
		)

	protected implicit def string2Type(string: String): Type.Any = Type.TypeAlias(string)

}
