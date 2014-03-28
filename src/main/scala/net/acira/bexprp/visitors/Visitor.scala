package net.acira.bexprp.visitors

import net.acira.bexprp.core._

abstract class Visitor[R] {

	protected def defaultUnaryOperation: (UnaryOperation => R) = ???
	protected def defaultBinaryOperation: (BinaryOperation => R) = ???
	protected def defaultLiteralOperation: (Literal => R) = ???

	def visit(expression: Not): R = defaultUnaryOperation(expression)
	def visit(expression: And): R = defaultBinaryOperation(expression)
	def visit(expression: Or): R = defaultBinaryOperation(expression)
	def visit(expression: Implication): R = defaultBinaryOperation(expression)
	def visit(expression: LeftImplication): R = defaultBinaryOperation(expression)
	def visit(expression: Equivalence): R = defaultBinaryOperation(expression)
	def visit(expression: VariableLiteral): R = defaultLiteralOperation(expression)
	def visit(expression: ConstantLiteral): R = defaultLiteralOperation(expression)

}
