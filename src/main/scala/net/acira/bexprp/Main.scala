package net.acira.bexprp

object Main {

	def main(args: Array[String]) {
		val e = "(not a -> b) or c"
		e.prettyPrint
		val s = e.expression.get.unboundVariables
		println
	}

}
