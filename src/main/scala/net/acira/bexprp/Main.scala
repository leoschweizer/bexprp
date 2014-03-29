package net.acira.bexprp

import net.acira.bexprp.visitors.EverythingVisitor

object Main {

	def main(args: Array[String]) {
		val e = "(not a -> b) or c"
		e.prettyPrint
		e.expression.get.accept(new EverythingVisitor)
		val s = e.expression.get.unboundVariables
		println
	}

}
