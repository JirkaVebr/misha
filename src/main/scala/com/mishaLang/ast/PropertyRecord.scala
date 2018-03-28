package com.mishaLang.ast

import com.mishaLang.ast.Language.Value

case class PropertyRecord(name: String, original: Value.Value, output: String, flags: Set[Value.Flag] = Set.empty)
