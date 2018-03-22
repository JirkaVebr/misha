package com.mishaLang.parser.common

import com.mishaLang.ast.Language.Node
import com.mishaLang.ast.NodePosition
import org.parboiled2._
import shapeless._

trait L1_AstNode { this: org.parboiled2.Parser
	with StringBuilding
	with L0_Whitespace =>


	def nodeStart: Rule1[Int] = rule {
		push(cursor)
	}

	def nodeEnd[N <: Node]: Rule[Int :: N :: HNil, N :: HNil] = rule {
		run((start: Int, node: N) => {
			node.position = NodePosition(start, cursor)

			node
		})
	}

}
