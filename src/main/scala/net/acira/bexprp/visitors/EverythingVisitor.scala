package net.acira.bexprp.visitors

import net.acira.bexprp.core._

class EverythingVisitor extends Visitor[Unit] {

	override def visitUnary(expression: UnaryOperation) = { println("22 visited: " + expression); this }
	override def visitNot(expression: Not) =  { println("1 visited: " + expression); this }
	override def visitBinary(expression: BinaryOperation) = { println("23 visited: " + expression); this }
	override def visitAnd(expression: And) = { println("2 visited: " + expression); this }
	override def visitOr(expression: Or) = { println("3 visited: " + expression); this }
	override def visitImplication(expression: Implication) = { println("4 visited: " + expression); this }
	override def visitLeftImplication(expression: LeftImplication) = { println("5 visited: " + expression); this }
	override def visitEquivalence(expression: Equivalence) = { println("6 visited: " + expression); this }
	override def visitLiteral(expression: Literal) = { println("21 visited: " + expression); this }
	override def visitVariable(expression: Variable) = { println("7 visited: " + expression); this }
	override def visitConstant(expression: Constant) = { println("8 visited: " + expression); this }

	def result: Unit = {}

}
