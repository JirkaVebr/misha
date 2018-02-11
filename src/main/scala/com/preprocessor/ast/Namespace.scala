package com.preprocessor.ast

sealed abstract class Namespace


case class NamedNamespace(name: String) extends Namespace

case object UniversalNamespace extends Namespace

case object NoNamespace extends Namespace
