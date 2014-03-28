package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class EverythingVisitor extends Visitor[Unit] {

	override def visit(expression: UnaryOperation) = { println("1 visited: " + expression); this }
	override def visit(expression: Not) =  { println("2 visited: " + expression); this }
	override def visit(expression: BinaryOperation) = { println("3 visited: " + expression); this }
	override def visit(expression: And) = { println("4 visited: " + expression); this }
	override def visit(expression: Or) = { println("5 visited: " + expression); this }
	override def visit(expression: Implication) = { println("6 visited: " + expression); this }
	override def visit(expression: LeftImplication) = { println("7 visited: " + expression); this }
	override def visit(expression: Equivalence) = { println("8 visited: " + expression); this }
	override def visit(expression: Literal) = { println("9 visited: " + expression); this }
	override def visit(expression: VariableLiteral) = { println("10 visited: " + expression); this }
	override def visit(expression: ConstantLiteral) = { println("11 visited: " + expression); this }

	def result: Unit = {}

}
