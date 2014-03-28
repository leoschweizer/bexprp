package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class PrettyPrintVisitor extends Visitor[String] {

	override def visit(expression: Not) = s"¬(${expression.operand.accept(this)})"
	override def visit(expression: And) = s"(${expression.left.accept(this)} ∧ ${expression.right.accept(this)})"
	override def visit(expression: Or) = s"(${expression.left.accept(this)} ∨ ${expression.right.accept(this)})"
	override def visit(expression: Implication) = s"(${expression.left.accept(this)} → ${expression.right.accept(this)})"
	override def visit(expression: LeftImplication) = s"(${expression.left.accept(this)} ← ${expression.right.accept(this)})"
	override def visit(expression: Equivalence) = s"(${expression.left.accept(this)} ↔ ${expression.right.accept(this)})"
	override def visit(expression: VariableLiteral) = expression.literal
	override def visit(expression: ConstantLiteral) = expression.value.toString

}
