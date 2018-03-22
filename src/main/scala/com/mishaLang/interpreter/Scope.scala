package com.mishaLang.interpreter

object Scope {

	/**
		* The id's are stored in reversed order, meaning that the head is the deepest id
		*/
	type Id = List[Int]

	val rootScopeId: Id = Nil

}
