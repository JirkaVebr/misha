package com.mishaLang.ast

import com.mishaLang.spec.AttributeSelector.Matcher

case class MatchTarget(matcher: Matcher, value: CssIdentifier)
