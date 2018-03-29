package com.mishaLang.utils

trait ImmutableProduct { this: Product =>

	override lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)

}
