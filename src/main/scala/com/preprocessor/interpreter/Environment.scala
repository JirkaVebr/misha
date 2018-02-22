package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol._


class Environment private
	(val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value],
	 val subEnvironments: List[Environment]
	) {

	def this(parentEnvironment: Option[Environment] = None) =
		this(parentEnvironment, Map.empty, List[Environment]())

	def pushSubScope(): Environment =
		new Environment(Some(this), Map[Symbol, Symbol#Value](), subEnvironments)

	def pushSubScope(context: Context.Value): Environment =
		new Environment(Some(this), Map(Context -> context), subEnvironments)

	def popSubScope(): Option[Environment] = parentEnvironment match {
		case Some(parent) => Some(new Environment(
			parent.parentEnvironment,
			parent.symbolTable,
			this :: parent.subEnvironments
		))
		case None => None
	}

	def updated(name: Symbol)(value: name.Value): Environment =
		new Environment(parentEnvironment, symbolTable.updated(name, value), subEnvironments)

	def isInCurrentScope(name: Symbol): Boolean = symbolTable.get(name).nonEmpty

	def isInScope(name: Symbol): Boolean = isInCurrentScope(name) || (parentEnvironment match {
		case Some(parent) => parent.isInScope(name)
		case None => false
	})

	def isWritable(name: ValueSymbol): Boolean = true // TODO


	def lookup(name: Symbol): Option[name.Value] = {
		val value = symbolTable.get(name).asInstanceOf[Option[name.Value]]
		value match {
			case Some(_) => value
			case None => parentEnvironment match {
				case Some(parent) => parent.lookup(name)
				case None => None
			}
		}
	}

}

