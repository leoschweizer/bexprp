package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class UnboundVariableFinder extends Visitor[Set[VariableLiteral]] {

	override def visit(expression: Not) = expression.operand.accept(this)
	override def visit(expression: And) = expression.left.accept(this) ++ expression.right.accept(this)
	override def visit(expression: Or) = expression.left.accept(this) ++ expression.right.accept(this)
	override def visit(expression: Implication) = expression.left.accept(this) ++ expression.right.accept(this)
	override def visit(expression: LeftImplication) = expression.left.accept(this) ++ expression.right.accept(this)
	override def visit(expression: Equivalence) = expression.left.accept(this) ++ expression.right.accept(this)
	override def visit(expression: VariableLiteral) = if (!expression.isBound) Set(expression) else Set.empty
	override def visit(expression: ConstantLiteral) = Set.empty

}
