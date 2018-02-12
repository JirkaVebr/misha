package com.preprocessor.interpreter

import com.preprocessor.ast.Symbol

class Scope[K <: Symbol.Symbol[V], V] private(val parentScope: Option[Scope[K, V]], val symbolTable: Map[K, V]) {

	def this(parentScope: Option[Scope[K, V]] = None) = {
		this(parentScope, Map.empty)
	}

	def pushSubScope(): Scope[K, V] = new Scope[K, V](Some(this))

	def updated(name: K, value: V): Scope[K, V] = new Scope(parentScope, symbolTable.updated(name, value))

	def lookup(name: K): Option[V] = {
		val value = symbolTable.get(name)
		value match {
			case Some(_) => value
			case None => parentScope match {
				case Some(parent) => parent.lookup(name)
				case None => None
			}
		}
	}

}
