package net.acira.bexprp.visitors

import net.acira.bexprp.core._

trait Visitor[R] {

	def visit(expression: Expression): Visitor[R] = expression match {
		case Not(_) => {
			visitUnary(expression.asInstanceOf[UnaryOperation])
			visitNot(expression.asInstanceOf[Not])
		}
		case And(_, _) => {
			visitBinary(expression.asInstanceOf[BinaryOperation])
			visitAnd(expression.asInstanceOf[And])
		}
		case Or(_, _) => {
			visitBinary(expression.asInstanceOf[BinaryOperation])
			visitOr(expression.asInstanceOf[Or])
		}
		case Implication(_, _) => {
			visitBinary(expression.asInstanceOf[BinaryOperation])
			visitImplication(expression.asInstanceOf[Implication])
		}
		case LeftImplication(_, _) => {
			visitBinary(expression.asInstanceOf[BinaryOperation])
			visitLeftImplication(expression.asInstanceOf[LeftImplication])
		}
		case Equivalence(_ , _) => {
			visitBinary(expression.asInstanceOf[BinaryOperation])
			visitEquivalence(expression.asInstanceOf[Equivalence])
		}
		case Constant(_) => {
			visitLiteral(expression.asInstanceOf[Literal])
			visitConstant(expression.asInstanceOf[Constant])
		}
		case Variable(_) => {
			visitLiteral(expression.asInstanceOf[Literal])
			visitVariable(expression.asInstanceOf[Variable])
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
	def visitConstant(expression: Constant) = this

	def result: R

}

trait TraversingVisitor[R] {

	protected def defaultUnaryOperation: (UnaryOperation => R) = ???
	protected def defaultBinaryOperation: (BinaryOperation => R) = ???
	protected def defaultLiteralOperation: (Literal => R) = ???

	def visit(expression: Expression) = expression match {
		case Not(_) => visitNot(expression.asInstanceOf[Not])
		case And(_, _) => visitAnd(expression.asInstanceOf[And])
		case Or(_, _) => visitOr(expression.asInstanceOf[Or])
		case Implication(_, _) => visitImplication(expression.asInstanceOf[Implication])
		case LeftImplication(_, _) => visitLeftImplication(expression.asInstanceOf[LeftImplication])
		case Equivalence(_, _) => visitEquivalence(expression.asInstanceOf[Equivalence])
		case Variable(_) => visitVariable(expression.asInstanceOf[Variable])
		case Constant(_) => visitConstant(expression.asInstanceOf[Constant])
	}

	def visitNot(expression: Not): R = defaultUnaryOperation(expression)
	def visitAnd(expression: And): R = defaultBinaryOperation(expression)
	def visitOr(expression: Or): R = defaultBinaryOperation(expression)
	def visitImplication(expression: Implication): R = defaultBinaryOperation(expression)
	def visitLeftImplication(expression: LeftImplication): R = defaultBinaryOperation(expression)
	def visitEquivalence(expression: Equivalence): R = defaultBinaryOperation(expression)
	def visitVariable(expression: Variable): R = defaultLiteralOperation(expression)
	def visitConstant(expression: Constant): R = defaultLiteralOperation(expression)

}
