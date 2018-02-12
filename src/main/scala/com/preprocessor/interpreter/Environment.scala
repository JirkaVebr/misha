package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol._


class Environment private
	(val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value],
	 val subEnvironments: List[Environment]
	) {

	def this(parentEnvironment: Option[Environment] = None) =
		this(parentEnvironment, Map.empty, List[Environment]())

	def pushSubScope(context: Option[Context.Value] = None): Environment =
		new Environment(Some(this), context match {
			case Some(x) => Map[Symbol, Symbol#Value](
				Context -> x
			)
			case None => Map[Symbol, Symbol#Value]()
		}, subEnvironments)

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


	def lookup(name: Symbol): Option[Symbol#Value] = {
		val value = symbolTable.get(name)
		value match {
			case Some(_) => value
			case None => parentEnvironment match {
				case Some(parent) => parent.lookup(name)
				case None => None
			}
		}
	}

}

