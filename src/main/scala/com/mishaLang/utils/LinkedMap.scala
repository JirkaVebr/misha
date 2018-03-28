package com.mishaLang.utils

import scala.collection.generic.{CanBuildFrom, ImmutableMapFactory}
import scala.collection.immutable.{AbstractMap, MapLike}
import scala.collection.immutable


sealed class LinkedMap[K, +V] private(private val keysOrder: Vector[K], private val underlyingMap: Map[K, V])
	extends AbstractMap[K, V]
		with immutable.Map[K, V]
		with MapLike[K, V, LinkedMap[K, V]] {

	def this() = {
		this(Vector.empty, Map())
	}

	override def empty: LinkedMap[K, V] = LinkedMap.empty


	override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = {
		if (underlyingMap.contains(kv._1))
			new LinkedMap(keysOrder, underlyingMap + kv)
		else
			new LinkedMap(keysOrder :+ kv._1, underlyingMap + kv)
	}


	override def get(key: K): Option[V] =
		underlyingMap.get(key)


	override def iterator: Iterator[(K, V)] =
		keysOrder.map {
			key => (key, underlyingMap(key))
		}.iterator


	override def -(key: K): LinkedMap[K, V] = ???


}


object LinkedMap extends ImmutableMapFactory[LinkedMap] {

	implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), LinkedMap[A, B]] = new MapCanBuildFrom[A, B]

	override def empty[A, B]: LinkedMap[A, B] = EmptyLinkedMap.asInstanceOf[LinkedMap[A, B]]

	private object EmptyLinkedMap extends LinkedMap[Any, Nothing]
}
