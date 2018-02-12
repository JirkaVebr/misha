package com.preprocessor.ast

object Namespace {
	sealed trait Namespace


	case class NamedNamespace(name: String) extends Namespace

	case object UniversalNamespace extends Namespace

	case object NoNamespace extends Namespace
}
