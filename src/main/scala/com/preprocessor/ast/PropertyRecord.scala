package com.preprocessor.ast

import com.preprocessor.ast.Ast.Value

case class PropertyRecord(name: String, value: String, flags: Set[Value.Flag] = Set.empty)
