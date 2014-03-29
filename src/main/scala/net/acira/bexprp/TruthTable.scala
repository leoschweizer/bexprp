package net.acira.bexprp

import net.acira.bexprp.core.Variable

class TruthTable(val variables: List[Variable]) extends Iterator[Vector[(Variable, Boolean)]] {

	private var currentValue: Int = 0
	private lazy val maxValue: Int = Math.pow(2, variables.size).toInt

	override def next(): Vector[(Variable, Boolean)] = {
		val boolStr: String = Integer.toBinaryString(currentValue).reverse.padTo(variables.size, "0").reverse.mkString
		val bools = boolStr.toIndexedSeq.map(c => '0'.equals(c))
		currentValue += 1
		variables.zip(bools).toVector
	}

	override def hasNext: Boolean = currentValue < maxValue

}

object TruthTable {

	def on(variables: List[Variable]) = new TruthTable(variables)

}
