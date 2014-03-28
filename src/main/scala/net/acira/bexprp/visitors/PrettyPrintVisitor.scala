package net.acira.bexprp.visitors

class PrettyPrintVisitor extends TraversingVisitor[String] {

	override def defaultUnaryOperation = (expression) => s"${expression.operationSymbol}(${expression.operand.accept(this)})"

	override def defaultBinaryOperation = (expression) =>
		s"(${expression.left.accept(this)} ${expression.operationSymbol} ${expression.right.accept(this)})"

	override def defaultLiteralOperation = (expression) => expression.operationSymbol

}
