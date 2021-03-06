package com.mishaLang.spec

/**
	* @see https://drafts.csswg.org/selectors-4/#attribute-selectors
	*/
object AttributeSelector {

	sealed trait Modifier {
		val name: String
	}
	case object CaseInsensitive extends Modifier {
		override val name: String = "i"
	}

	lazy final val Modifiers: Map[String, Modifier] = Map(
		CaseInsensitive.name -> CaseInsensitive
	)


	sealed trait Matcher {
		def symbol: String
	}
	case object Equals extends Matcher {
		override val symbol: String = "=" // [attr=value]
	}
	case object Includes extends Matcher {
		override val symbol: String = "~=" // [attr~=value]
	}
	case object Prefix extends Matcher {
		override val symbol: String = "|=" // [attr|=value]
	}
	case object Preceded extends Matcher {
		override val symbol: String = "^=" // [attr^=value]
	}
	case object Followed extends Matcher {
		override val symbol: String = "$=" // [attr$=value]
	}
	case object Contains extends Matcher {
		override val symbol: String = "*=" // [attr*=value]
	}

	lazy final val Matchers: Map[String, Matcher] = Map(
		Equals.symbol -> Equals,
		Includes.symbol -> Includes,
		Prefix.symbol -> Prefix,
		Preceded.symbol -> Preceded,
		Followed.symbol -> Followed,
		Contains.symbol -> Contains
	)

}


