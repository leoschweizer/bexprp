package net.acira.bexprp

import net.acira.bexprp.core.VariableLiteral

class TruthTable(val variables: List[VariableLiteral]) extends Iterator[Vector[(VariableLiteral, Boolean)]] {

	private var currentValue: Int = 0
	private lazy val maxValue: Int = Math.pow(2, variables.size).toInt

	override def next(): Vector[(VariableLiteral, Boolean)] = {
		val boolStr: String = Integer.toBinaryString(currentValue).reverse.padTo(variables.size, "0").reverse.mkString
		val bools = boolStr.toIndexedSeq.map(c => '0'.equals(c))
		currentValue += 1
		variables.zip(bools).toVector
	}

	override def hasNext: Boolean = currentValue < maxValue

}

object TruthTable {

	def on(variables: List[VariableLiteral]) = new TruthTable(variables)

}
