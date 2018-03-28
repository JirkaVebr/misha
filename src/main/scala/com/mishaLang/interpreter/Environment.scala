package com.mishaLang.interpreter

import com.mishaLang.interpreter.Environment._
import com.mishaLang.interpreter.EnvironmentType.{EnvironmentType, FunctionEnvironment, RuleEnvironment, ScopeEnvironment}
import com.mishaLang.interpreter.Symbol._
import com.mishaLang.utils.LinkedMap

import scala.annotation.tailrec


class Environment private
	(val meta: EnvironmentMeta,
	 val parentEnvironment: Option[Environment],
	 val symbolTable: Map[Symbol, Symbol#Value],
	 childEnvironments: Vector[Environment]
	) {

	val subEnvironments: Vector[Environment] = childEnvironments.map(environment => environment.cloneWithNewParent(this))


	def this(parentEnvironment: Option[Environment] = None) =
		this(EnvironmentMeta(Scope.rootScopeId, RuleEnvironment), parentEnvironment, Map(
			RuleStoreSymbol -> LinkedMap.empty[Symbol.RuleContextSymbol.Value, PropertyStore].asInstanceOf[RuleStoreSymbol.Value]
		), Vector())


	protected def cloneWithNewParent(newParent: Environment): Environment =
		createInstance(meta, Some(newParent), symbolTable, subEnvironments)


	private def createAndUpdateTree(newMeta: EnvironmentMeta, newParent: Option[Environment], symbols: Map[Symbol, Symbol#Value],
																	children: Vector[Environment]): Environment = {
		val newInstance = createInstance(newMeta, newParent, symbols, children)
		newParent match {
			case Some(parent) =>
				createInstance(newMeta, Some(parent.createAndUpdateTree(
					parent.meta, parent.parentEnvironment, parent.symbolTable,
					parent.subEnvironments.updated(newMeta.id.last, newInstance)
				)), symbols, children)
			case None => newInstance
		}
	}


	protected def createInstance(childMeta: EnvironmentMeta, parent: Option[Environment], symbols: Map[Symbol, Symbol#Value],
															 children: Vector[Environment]): Environment =
		new Environment(childMeta, parent, symbols, children)


	def pushSubScope(): Option[Environment] = {
		pushSubScope(createSubScopeChild(Map(), ScopeEnvironment))
	}


	def pushSubScope(context: RuleContextSymbol.Value): Option[Environment] = {
		pushSubScope(createSubScopeChild(Map(RuleContextSymbol -> context), RuleEnvironment))
	}


	def pushFunctionScope(): Option[Environment] = {
		pushSubScope(createSubScopeChild(Map(), FunctionEnvironment))
	}


	def pushFunctionScope(context: RuleContextSymbol.Value): Option[Environment] = {
		pushSubScope(createSubScopeChild(Map(RuleContextSymbol -> context), FunctionEnvironment))
	}


	private def pushSubScope(child: Environment): Option[Environment] = {
		val newThis = createAndUpdateTree(meta, parentEnvironment, symbolTable, subEnvironments :+ child)

		if (child.meta.id.length >= MaxStackSize) None
		else Some(newThis.subEnvironments.last)
	}


	private def createSubScopeChild(childSymbols: Map[Symbol, Symbol#Value], environmentType: EnvironmentType): Environment =
		createInstance(
			EnvironmentMeta(meta.id :+ subEnvironments.length, environmentType),
			Some(this), childSymbols,
			Vector()
		)


	def popSubScope(): Option[Environment] = parentEnvironment


	def updated(name: Symbol)(value: name.Value): Environment =
		if (isInCurrentScope(name))
			putNew(name)(value)
		else parentEnvironment match {
			case Some(parent) => cloneWithNewParent(parent.updated(name)(value))
			case None => putNew(name)(value) // If this method is used correctly, this won't ever be executed
		}


	def putNew(name: Symbol)(value: name.Value): Environment =
		createAndUpdateTree(meta, parentEnvironment, symbolTable.updated(name, value), subEnvironments)


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


	def getEnvironmentByScopeId(id: ScopeId): Option[Environment] = {
		val longestCommonPrefix = meta.id.zip(id).takeWhile(Function.tupled(_ == _))

		getNthParent(meta.id.length - longestCommonPrefix.length) match {
			case Some(nthParent) => Some(nthParent.getChildren(id.drop(longestCommonPrefix.length)))
			case None => None
		}
	}


	@tailrec
	private def getNthParent(n: Int): Option[Environment] = {
		if (n == 0) Some(this)
		else parentEnvironment match {
			case Some(parent) => parent.getNthParent(n - 1)
			case None => None
		}
	}


	/**
		* @param relativeId The id is *relative* to this environment, meaning that it is assumed that there always will
		*                   be appropriate sub-environments
		*/
	@tailrec
	private def getChildren(relativeId: ScopeId): Environment = {
		if (relativeId.length == 1) subEnvironments(relativeId(0))
		else if (relativeId.isEmpty) this
		else subEnvironments(relativeId(0)).getChildren(relativeId.tail)
	}

}


object Environment {

	val MaxStackSize: Int = 30 // TODO make this configurable
}
