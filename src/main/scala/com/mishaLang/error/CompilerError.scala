package com.mishaLang.error

case class CompilerError(message: String, usefulData: Any*) extends Error
