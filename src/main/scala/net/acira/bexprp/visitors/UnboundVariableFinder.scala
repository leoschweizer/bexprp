package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class UnboundVariableFinder extends Visitor[Set[VariableLiteral]] {

	override def defaultUnaryOperation = (expression: UnaryOperation) => expression.operand.accept(this)
	override def defaultBinaryOperation = (expression: BinaryOperation) => expression.left.accept(this) ++ expression.right.accept(this)

	override def visit(expression: VariableLiteral) = if (!expression.isBound) Set(expression) else Set.empty
	override def visit(expression: ConstantLiteral) = Set.empty

}
