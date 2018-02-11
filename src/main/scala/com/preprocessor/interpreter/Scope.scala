package com.preprocessor.interpreter

import com.preprocessor._
import com.preprocessor.ast.Symbol

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

	def createTypeScope(): TypeScope = new TypeScope

	def createValueScope(): ValueScope = new ValueScope
}
