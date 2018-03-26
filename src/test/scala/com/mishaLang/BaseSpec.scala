package com.mishaLang

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FlatSpec

class BaseSpec extends FlatSpec {


	implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.00000001)

}
