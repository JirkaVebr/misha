package com.preprocessor.ast

import com.preprocessor.spec.AttributeSelector.Matcher

case class MatchTarget(matcher: Matcher, value: CssIdentifier)
