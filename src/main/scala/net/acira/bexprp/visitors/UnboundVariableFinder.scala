package net.acira.bexprp.visitors

import net.acira.bexprp.core.Variable

class UnboundVariableFinder extends Visitor[Set[Variable]] {

	var unboundVariables = Set.empty[Variable]

	override def visitVariable(expression: Variable) = {
		if (!expression.isBound) unboundVariables += expression
		this
	}

	override def result: Set[Variable] = unboundVariables

}
