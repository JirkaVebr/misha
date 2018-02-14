package com.preprocessor.ast

import com.preprocessor.ast.Ast.{Type, Value}

case class ValueRecord(value: Value.Value, recordType: Type.Any)
