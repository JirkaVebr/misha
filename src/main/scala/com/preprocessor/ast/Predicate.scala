package com.preprocessor.ast

object Predicate {

	sealed trait Predicate[T]


	case class Term[T](value: T) extends Predicate[T]

	case class Not[T](predicate: Predicate[T]) extends Predicate[T]

	case class And[T](left: Predicate[T], right: Predicate[T]) extends Predicate[T]
	case class Or[T](left: Predicate[T], right: Predicate[T]) extends Predicate[T]

}
