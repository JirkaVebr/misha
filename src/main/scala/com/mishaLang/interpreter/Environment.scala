package com.mishaLang.interpreter

import Symbol._

import scala.annotation.tailrec


class Environment private
	(val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value],
	 childEnvironments: Vector[Environment]
	) {

	val subEnvironments: Vector[Environment] = childEnvironments.map(environment => environment.cloneWithNewParent(this))

	def this(parentEnvironment: Option[Environment] = None) =
		this(parentEnvironment, Map(), Vector())

	def pushSubScope(): Environment =
		new Environment(Some(this), Map[Symbol, Symbol#Value](), subEnvironments)

	def pushSubScope(context: RuleContextSymbol.Value): Environment =
		new Environment(Some(this), Map(RuleContextSymbol -> context), subEnvironments)

	def popSubScope(): Option[Environment] = parentEnvironment match {
		case Some(parent) => Some(
			if (lookupCurrent(PropertySymbol).nonEmpty || subEnvironments.nonEmpty)
				new Environment(
					parent.parentEnvironment,
					parent.symbolTable,
					parent.subEnvironments :+ this
				)
			else parent
		)
		case None => None
	}

	def updated(name: Symbol)(value: name.Value): Environment =
		if (isInCurrentScope(name))
			putNew(name)(value)
		else parentEnvironment match {
			case Some(parent) => cloneWithNewParent(parent.updated(name)(value))
			case None => putNew(name)(value) // If this method is used correctly, this won't ever be executed
		}

	def putNew(name: Symbol)(value: name.Value): Environment =
		new Environment(parentEnvironment, symbolTable.updated(name, value), subEnvironments)

	def isInCurrentScope(name: Symbol): Boolean = symbolTable.get(name).nonEmpty

	@tailrec final def isInScope(name: Symbol): Boolean = isInCurrentScope(name) || (parentEnvironment match {
		case Some(parent) => parent.isInScope(name)
		case None => false
	})

	def isWritable(name: ValueSymbol): Boolean = true // TODO

	def cloneWithNewParent(newParent: Environment): Environment =
		new Environment(Some(newParent), symbolTable, subEnvironments)

	def lookupCurrent(name: Symbol): Option[name.Value] =
		symbolTable.get(name).asInstanceOf[Option[name.Value]]

	def lookupContext(): Option[RuleContextSymbol.Value] =
		lookup(RuleContextSymbol)

	@tailrec final def lookup(name: Symbol): Option[name.Value] = {
		val value = lookupCurrent(name)
		value match {
			case Some(_) => value
			case None => parentEnvironment match {
				case Some(parent) => parent.lookup(name)
				case None => None
			}
		}
	}

}

