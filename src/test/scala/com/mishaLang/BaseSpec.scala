package com.mishaLang

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{FlatSpec, Matchers}

class BaseSpec extends FlatSpec with Matchers {


	implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.00000001)

}
