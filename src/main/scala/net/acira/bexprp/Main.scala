package net.acira.bexprp

object Main {

	def main(args: Array[String]) {
		val e = "(not a -> b) or c"
		val s = e.expression.flatMap(_.evaluate)
		val b = e.bind("a" -> true, "b" -> false)
		b.get.prettyPrint
		println
	}

}
