package net.acira.bexprp.core

import net.acira.bexprp.visitors._
import java.io.PrintStream
import scala.reflect.ClassTag

trait Expression {
	def evaluate: Boolean
	def operationSymbol: String
	def accept[R](visitor: Visitor[R]): Visitor[R]

	def accept[R](visitor: TraversingVisitor[R]): R = visitor.visit(this)
	def bind(variableBindings: (String, Boolean)*): Expression = bind(variableBindings.map(x => FreeVariable(x._1) -> x._2): _*)
	def bind[X: ClassTag](variableBindings: (Variable, Boolean)*): Expression = accept(new VariableBinder(variableBindings.toMap))
	def unboundVariables: Set[FreeVariable] = this.accept(new UnboundVariableFinder()).result
	def prettyPrint: Unit = prettyPrint(Console.out)
	def prettyPrint(on: PrintStream): Unit = on.println(accept(new PrettyPrinter()))
}

trait Literal extends Expression {
	override def accept[R](visitor: Visitor[R]) = visitor.visit(this)
}

case class Constant(value: Boolean) extends Literal {
	override def evaluate: Boolean = value
	override def operationSymbol = value.toString
}

trait Variable extends Literal {
	def literal: String
}

case class FreeVariable(literal: String) extends Variable {
	override def evaluate = ???
	override def operationSymbol = literal
}

case class BoundVariable(literal: String, value: Boolean) extends Variable {
	override def evaluate = value
	override def operationSymbol = value.toString
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