package net.acira.bexprp.core

import net.acira.bexprp.visitors._

trait Expression {
	def evaluate: Boolean
	def operationSymbol: String
	def accept[R](visitor: Visitor[R]): Visitor[R]

	def accept[R](visitor: TraversingVisitor[R]): R = visitor.visit(this)
	def unboundVariables: Set[VariableLiteral] = this.accept(new UnboundVariableFinder()).result
}

trait Literal extends Expression {

	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)

}

case class ConstantLiteral(value: Boolean) extends Literal {
	override def evaluate: Boolean = value
	override def operationSymbol = value.toString
}

case class VariableLiteral(literal: String) extends Literal {
	var boundValue: Option[Boolean] = None
	override def evaluate: Boolean = boundValue.get
	override def operationSymbol = boundValue.map(_.toString).getOrElse(literal)
	def bind(value: Boolean): Unit = boundValue = Some(value)
	def isBound = boundValue.isDefined
}

trait UnaryOperation extends Expression {

	def operand: Expression

	override def accept[R](visitor: Visitor[R]) = {
		operand.accept(visitor)
		visitor.visit(this)
	}

}

trait BinaryOperation extends Expression {

	def left: Expression
	def right: Expression

	override def accept[R](visitor: Visitor[R]) = {
		left.accept(visitor)
		right.accept(visitor)
		visitor.visit(this)
	}

}

case class Not(operand: Expression) extends UnaryOperation {
	override def evaluate: Boolean = !operand.evaluate
	override def operationSymbol = "¬"
}

case class And(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate & right.evaluate
	override def operationSymbol = "∧"
}

case class Or(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate | right.evaluate
	override def operationSymbol = "∨"
}

case class Implication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !left.evaluate | right.evaluate
	override def operationSymbol = "→"
}

case class LeftImplication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !right.evaluate | left.evaluate
	override def operationSymbol = "←"
}

case class Equivalence(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = left.evaluate == right.evaluate
	override def operationSymbol = "↔"
}