package net.acira.bexprp.visitors

import net.acira.bexprp.core.FreeVariable

class UnboundVariableFinder extends Visitor[Set[FreeVariable]] {

	var unboundVariables = Set.empty[FreeVariable]

	override def visitFreeVariable(expression: FreeVariable) = {
		unboundVariables += expression
		this
	}

	override def result: Set[FreeVariable] = unboundVariables

}
