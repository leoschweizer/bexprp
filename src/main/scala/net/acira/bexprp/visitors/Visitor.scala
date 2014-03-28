package net.acira.bexprp.visitors

import net.acira.bexprp.core._

trait Visitor[R] {

	def visit(expression: UnaryOperation) = this
	def visit(expression: Not) = this
	def visit(expression: BinaryOperation) = this
	def visit(expression: And) = this
	def visit(expression: Or) = this
	def visit(expression: Implication) = this
	def visit(expression: LeftImplication) = this
	def visit(expression: Equivalence) = this
	def visit(expression: Literal) = this
	def visit(expression: VariableLiteral) = this
	def visit(expression: ConstantLiteral) = this

	def result: R

}
