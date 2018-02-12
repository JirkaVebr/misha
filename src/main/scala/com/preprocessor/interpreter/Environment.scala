package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol._


class Environment private
	(val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value]
	) {

	def this(parentEnvironment: Option[Environment] = None, selector: Option[Selector.Value] = None) = {
		this(parentEnvironment, selector match {
			case Some(x) => Map[Symbol, Symbol#Value](
				Selector -> x
			)
			case None => Map[Symbol, Symbol#Value]()
		})
	}

	def pushSubScope(selector: Option[Selector.Value] = None): Environment = new Environment(Some(this), selector)

	def updated(name: Symbol)(value: name.Value): Environment =
		new Environment(parentEnvironment, symbolTable.updated(name, value))


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

