package com.preprocessor.interpreter.scope

import com.preprocessor.ast.Symbol
import com.preprocessor.ast.Symbol.{TypeSymbol, ValueSymbol}

import scala.annotation.tailrec

class Scope[K <: Symbol.Symbol[V], V](val symbols: List[Map[K, V]] = List.empty) {


	def pushSubScope(): Scope[K, V] = new Scope[K, V](Map[K, V]() :: symbols)

	def updated(name: K, value: V): Scope[K, V] = new Scope(symbols match {
		case Nil => List(Map(name -> value))
		case head :: tail => head.updated(name, value) :: tail
	})

	def lookup(name: K): Option[V] = {
		@tailrec
		def search(stack: List[Map[K, V]]): Option[V] = stack match {
			case Nil => None
			case head :: tail => head.get(name) match {
				case Some(value) => Some(value)
				case None => search(tail)
			}
		}

		search(symbols)
	}

}

object Scope {

	def createTypeScope(): Scope[TypeSymbol, TypeSymbol#Value] = {
		new Scope[TypeSymbol, TypeSymbol#Value]()
	}

	def createValueScope(): Scope[ValueSymbol, ValueSymbol#Value] = {
		new Scope[ValueSymbol, ValueSymbol#Value]()
	}
}
