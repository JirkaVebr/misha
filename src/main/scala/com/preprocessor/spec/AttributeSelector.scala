package com.preprocessor.spec

/**
	* @see https://drafts.csswg.org/selectors-4/#attribute-selectors
	*/
object AttributeSelector {

	sealed trait Modifier {
		def name: String
	}
	case object CaseInsensitive extends Modifier {
		override def name: String = "i"
	}

	val modifiers: Map[String, Modifier] = Map(
		CaseInsensitive.name -> CaseInsensitive
	)


	sealed trait Matcher {
		def symbol: String
	}
	case object Equals extends Matcher {
		override def symbol: String = "=" // [attr=value]
	}
	case object Includes extends Matcher {
		override def symbol: String = "~=" // [attr~=value]
	}
	case object Prefix extends Matcher {
		override def symbol: String = "|=" // [attr|=value]
	}
	case object Preceded extends Matcher {
		override def symbol: String = "^=" // [attr^=value]
	}
	case object Followed extends Matcher {
		override def symbol: String = "$=" // [attr$=value]
	}
	case object Contains extends Matcher {
		override def symbol: String = "*=" // [attr*=value]
	}

	val matchers: Map[String, Matcher] = Map(
		Equals.symbol -> Equals,
		Includes.symbol -> Includes,
		Prefix.symbol -> Prefix,
		Preceded.symbol -> Preceded,
		Followed.symbol -> Followed,
		Contains.symbol -> Contains
	)

}


