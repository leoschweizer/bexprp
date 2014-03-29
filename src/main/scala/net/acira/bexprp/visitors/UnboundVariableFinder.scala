package net.acira.bexprp.visitors

import net.acira.bexprp.core.VariableLiteral

class UnboundVariableFinder extends Visitor[Set[VariableLiteral]] {

	var unboundVariables = Set.empty[VariableLiteral]

	override def visitVariable(expression: VariableLiteral) = {
		if (!expression.isBound) unboundVariables += expression
		this
	}

	override def result: Set[VariableLiteral] = unboundVariables

}
