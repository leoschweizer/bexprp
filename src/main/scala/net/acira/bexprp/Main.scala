package net.acira.bexprp

object Main {

	def main(args: Array[String]) {
		val exp = "(a -> b) or c".expression.get
		val v = exp.allVariables
		for (r <- TruthTable.on(v.toList)) { println(r) }
		println
	}

}
