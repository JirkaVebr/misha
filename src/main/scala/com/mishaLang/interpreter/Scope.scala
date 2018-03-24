package com.mishaLang.interpreter

object Scope {

	/**
		* The id's are stored in deepest-last order
		*/
	type Id = Vector[Int]

	val rootScopeId: Id = Vector()

}
