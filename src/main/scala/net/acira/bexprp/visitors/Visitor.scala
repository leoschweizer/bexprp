package net.acira.bexprp.visitors

import net.acira.bexprp.core._

abstract class Visitor[R] {

	def visit(expression: Not): R
	def visit(expression: And): R
	def visit(expression: Or): R
	def visit(expression: Implication): R
	def visit(expression: LeftImplication): R
	def visit(expression: Equivalence): R
	def visit(expression: VariableLiteral): R
	def visit(expression: ConstantLiteral): R

}
