package net.acira.bexprp.core


trait Expression {
	def evaluate: Boolean
	def allVariables: Set[VariableLiteral]
}

trait Literal extends Expression

case class ConstantLiteral(value: Boolean) extends Literal {
	override def evaluate: Boolean = value
	override def allVariables = Set.empty
}

case class VariableLiteral(literal: String) extends Literal {
	var boundValue: Option[Boolean] = None
	override def evaluate: Boolean = boundValue.get
	def allVariables = Set(this)
	def bind(value: Boolean): Unit = boundValue = Some(value)
	def isBound = boundValue.isDefined
}

trait UnaryOperation extends Expression {
	def operand: Expression
	override def allVariables = operand.allVariables
}

trait BinaryOperation extends Expression {
	def left: Expression
	def right: Expression
	override def allVariables = left.allVariables ++ right.allVariables
}

case class Not(operand: Expression) extends UnaryOperation {
	override def evaluate: Boolean = !operand.evaluate
}

case class And(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate & right.evaluate
}

case class Or(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate: Boolean = left.evaluate | right.evaluate
}

case class Implication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !left.evaluate | right.evaluate
}

case class LeftImplication(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = !right.evaluate | left.evaluate
}

case class Equivalence(left: Expression, right: Expression) extends BinaryOperation {
	override def evaluate = left.evaluate == right.evaluate
}