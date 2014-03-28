package net.acira.bexprp.core

import net.acira.bexprp.visitors.{UnboundVariableFinder, Visitor}


trait Expression {
	def evaluate: Boolean
	def unboundVariables: Set[VariableLiteral] = this.accept(new UnboundVariableFinder)
	def accept[R](visitor: Visitor[R]): R
}

trait Literal extends Expression

case class ConstantLiteral(value: Boolean) extends Literal {
	override def evaluate: Boolean = value
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class VariableLiteral(literal: String) extends Literal {
	var boundValue: Option[Boolean] = None
	override def evaluate: Boolean = boundValue.get
	def bind(value: Boolean): Unit = boundValue = Some(value)
	def isBound = boundValue.isDefined
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

trait UnaryOperation extends Expression {
	def operand: Expression
}

trait BinaryOperation extends Expression {
	def left: Expression
	def right: Expression
}

case class Not(operand: Expression) extends UnaryOperation {
	override def evaluate: Boolean = !operand.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class And(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate & right.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class Or(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate | right.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class Implication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !left.evaluate | right.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class LeftImplication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !right.evaluate | left.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class Equivalence(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = left.evaluate == right.evaluate
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}