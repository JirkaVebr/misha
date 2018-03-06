package com.preprocessor.ast

object Namespace {
	sealed trait Namespace


	case class NamedNamespace(name: CssIdentifier) extends Namespace

	case object AnyNamespace extends Namespace

	case object NoNamespace extends Namespace
}
