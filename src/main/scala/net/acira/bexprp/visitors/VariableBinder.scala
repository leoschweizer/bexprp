package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class VariableBinder(variableBindings: Map[Variable, Boolean]) extends TraversingVisitor[Expression] {

	private def bindIfPossible(expression: Expression): Expression = expression match {
		case v: Variable => if (variableBindings.contains(v))BoundVariable(v.literal, variableBindings(v)) else v
		case _ => expression.accept(this)
	}

	override def visitNot(expression: Not) = Not(bindIfPossible(expression.operand))
	override def visitAnd(expression: And) = And(bindIfPossible(expression.left), bindIfPossible(expression.right))
	override def visitOr(expression: Or) = Or(bindIfPossible(expression.left), bindIfPossible(expression.right))
	override def visitImplication(expression: Implication) = Implication(bindIfPossible(expression.left), bindIfPossible(expression.right))
	override def visitLeftImplication(expression: LeftImplication) = LeftImplication(bindIfPossible(expression.left), bindIfPossible(expression.right))
	override def visitEquivalence(expression: Equivalence) = Equivalence(bindIfPossible(expression.left), bindIfPossible(expression.right))
	override def visitConstant(expression: Constant) = expression
	override def visitBoundVariable(expression: BoundVariable) = bindIfPossible(expression)
	override def visitFreeVariable(expression: FreeVariable) = bindIfPossible(expression)

}
