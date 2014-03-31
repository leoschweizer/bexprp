package net.acira.bexprp.visitors

import net.acira.bexprp.core._

trait Visitor[R] {

	def visit(expression: Expression): Visitor[R] = expression match {
		case e: Not => {
			visitUnary(e)
			visitNot(e)
		}
		case e: And => {
			visitBinary(e)
			visitAnd(e)
		}
		case e: Or => {
			visitBinary(e)
			visitOr(e)
		}
		case e: Implication => {
			visitBinary(e)
			visitImplication(e)
		}
		case e: LeftImplication => {
			visitBinary(e)
			visitLeftImplication(e)
		}
		case e: Equivalence => {
			visitBinary(e)
			visitEquivalence(e)
		}
		case e: Constant => {
			visitLiteral(e)
			visitConstant(e)
		}
		case e: FreeVariable => {
			visitLiteral(e)
			visitVariable(e)
			visitFreeVariable(e)
		}
		case e: BoundVariable => {
			visitLiteral(e)
			visitVariable(e)
			visitBoundVariable(e)
		}
	}

	def visitUnary(expression: UnaryOperation) = this
	def visitNot(expression: Not) = this
	def visitBinary(expression: BinaryOperation) = this
	def visitAnd(expression: And) = this
	def visitOr(expression: Or) = this
	def visitImplication(expression: Implication) = this
	def visitLeftImplication(expression: LeftImplication) = this
	def visitEquivalence(expression: Equivalence) = this
	def visitLiteral(expression: Literal) = this
	def visitVariable(expression: Variable) = this
	def visitFreeVariable(expression: FreeVariable) = this
	def visitBoundVariable(expression: BoundVariable) = this
	def visitConstant(expression: Constant) = this

	def result: R

}

trait TraversingVisitor[R] {

	protected def defaultUnaryOperation: (UnaryOperation => R) = ???
	protected def defaultBinaryOperation: (BinaryOperation => R) = ???
	protected def defaultLiteralOperation: (Literal => R) = ???

	def visit(expression: Expression): R = expression match {
		case e: Not => visitNot(e)
		case e: And => visitAnd(e)
		case e: Or => visitOr(e)
		case e: Implication => visitImplication(e)
		case e: LeftImplication => visitLeftImplication(e)
		case e: Equivalence => visitEquivalence(e)
		case e: FreeVariable => visitFreeVariable(e)
		case e: BoundVariable => visitBoundVariable(e)
		case e: Constant => visitConstant(e)
	}

	def visitNot(expression: Not): R = defaultUnaryOperation(expression)
	def visitAnd(expression: And): R = defaultBinaryOperation(expression)
	def visitOr(expression: Or): R = defaultBinaryOperation(expression)
	def visitImplication(expression: Implication): R = defaultBinaryOperation(expression)
	def visitLeftImplication(expression: LeftImplication): R = defaultBinaryOperation(expression)
	def visitEquivalence(expression: Equivalence): R = defaultBinaryOperation(expression)
	def visitFreeVariable(expression: FreeVariable): R = defaultLiteralOperation(expression)
	def visitBoundVariable(expression: BoundVariable): R = defaultLiteralOperation(expression)
	def visitConstant(expression: Constant): R = defaultLiteralOperation(expression)

}
