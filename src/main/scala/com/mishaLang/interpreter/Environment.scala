package com.mishaLang.interpreter

import Symbol._

import scala.annotation.tailrec


class Environment private
	(val scopeId: ScopeId,
	 val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value],
	 childEnvironments: Vector[Environment]
	) {

	val subEnvironments: Vector[Environment] = childEnvironments.map(environment => environment.cloneWithNewParent(this))


	def this(parentEnvironment: Option[Environment] = None) =
		this(Scope.rootScopeId, parentEnvironment, Map(), Vector())


	protected def cloneWithNewParent(newParent: Environment): Environment =
		createInstance(scopeId, Some(newParent), symbolTable, subEnvironments)


	private def createAndUpdateTree(id: ScopeId, newParent: Option[Environment], symbols: Map[Symbol, Symbol#Value],
																	children: Vector[Environment]): Environment = {
		val newInstance = createInstance(id, newParent, symbols, children)
		newParent match {
			case Some(parent) =>
				createInstance(id, Some(parent.createAndUpdateTree(
					parent.scopeId, parent.parentEnvironment, parent.symbolTable,
					parent.subEnvironments.updated(id.head, newInstance)
				)), symbols, children)
			case None => newInstance
		}
	}


	protected def createInstance(childId: ScopeId, parent: Option[Environment], symbols: Map[Symbol, Symbol#Value],
															 children: Vector[Environment]): Environment =
		new Environment(childId, parent, symbols, children)


	def pushSubScope(): Environment = {
		val child = createInstance(subEnvironments.length :: scopeId, Some(this), Map[Symbol, Symbol#Value](), Vector())
		val newThis = createAndUpdateTree(scopeId, parentEnvironment, symbolTable, subEnvironments :+ child)
		newThis.subEnvironments.last
	}


	def pushSubScope(context: RuleContextSymbol.Value): Environment = {
		val child = createInstance(subEnvironments.length :: scopeId, Some(this), Map(RuleContextSymbol -> context), Vector())
		val newThis = createAndUpdateTree(scopeId, parentEnvironment, symbolTable, subEnvironments :+ child)
		newThis.subEnvironments.last
	}


	def popSubScope(): Option[Environment] = parentEnvironment


	def updated(name: Symbol)(value: name.Value): Environment =
		if (isInCurrentScope(name))
			putNew(name)(value)
		else parentEnvironment match {
			case Some(parent) => cloneWithNewParent(parent.updated(name)(value))
			case None => putNew(name)(value) // If this method is used correctly, this won't ever be executed
		}


	def putNew(name: Symbol)(value: name.Value): Environment =
		createAndUpdateTree(scopeId, parentEnvironment, symbolTable.updated(name, value), subEnvironments)


	def isInCurrentScope(name: Symbol): Boolean = symbolTable.get(name).nonEmpty


	@tailrec
	final def isInScope(name: Symbol): Boolean = isInCurrentScope(name) || (parentEnvironment match {
		case Some(parent) => parent.isInScope(name)
		case None => false
	})


	def isWritable(name: ValueSymbol): Boolean = true // TODO


	def lookupCurrent(name: Symbol): Option[name.Value] =
		symbolTable.get(name).asInstanceOf[Option[name.Value]]


	def lookupContext(): Option[RuleContextSymbol.Value] =
		lookup(RuleContextSymbol)


	@tailrec
	final def lookup(name: Symbol): Option[name.Value] = {
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

