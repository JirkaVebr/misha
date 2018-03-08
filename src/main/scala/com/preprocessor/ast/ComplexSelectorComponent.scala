package com.preprocessor.ast

import com.preprocessor.ast.Selector.Compound
import com.preprocessor.spec.SelectorCombinator.Combinator

case class ComplexSelectorComponent(combinator: Combinator, selector: Compound)
