package com.mishaLang.ast

import com.mishaLang.ast.Language.Value
import com.mishaLang.utils.ImmutableProduct

case class PropertyRecord(name: String, original: Value.Value, output: String, flags: Set[Value.Flag] = Set.empty)
	extends ImmutableProduct
