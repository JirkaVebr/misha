package com.mishaLang

import com.mishaLang.ast.Language.Expression.Expression
import com.mishaLang.ast.Language.Value
import com.mishaLang.ast.NumberUnit.{Percentage, SimpleUnit, SubUnits}

package object ast {

	implicit def string2CssIdentifier(string: String): CssIdentifier = CssIdentifier(string)
	implicit def cssIdentifier2String(identifier: CssIdentifier): String = identifier.value

	implicit def string2Value(string: String): Value.String = Value.String(string)

	implicit def simpleUnitToSubUnits(unit: SimpleUnit): SubUnits = Map(unit -> 1)


	type RuleHeadComponent = Either[String, Expression]
	type RuleHead = Vector[RuleHeadComponent]


	lazy final val PercentageUnit: SubUnits = Map(Percentage -> 1)

}
