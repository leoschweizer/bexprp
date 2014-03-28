package net.acira

import net.acira.bexprp.core.{Expression, VariableLiteral, BooleanExpressionParser, Literal}
import scala.reflect.ClassTag

package object bexprp {

	implicit def strToLiteral(self: String) = VariableLiteral(self)

	implicit class BexprpString(self: String) {

		def evaluate(literalValues: (String, Boolean)*): Boolean = evaluate(literalValues.map(x => VariableLiteral(x._1) -> x._2):_*)

		def evaluate[X: ClassTag](literalValues: (Literal, Boolean)*): Boolean = {
			val lv = literalValues.toMap
			val expression = BooleanExpressionParser.parse(self).get
			val literals = expression.allVariables
			literals.foreach(literal => {
				if (lv.contains(literal)) literal.bind(lv(literal))
			})
			expression.evaluate
		}

		def expression: Option[Expression] = {
			BooleanExpressionParser.parse(self)
		}

		def truthTable = {
			val exp = expression.get
			val (boundVariables, unboundVariables) = exp.allVariables.partition(_.isBound)
			val sortedUnboundVariables = unboundVariables.toList.sortBy(_.literal)
			val v = List(true, false)
			val truthValues = v.combinations(sortedUnboundVariables.size)
			(0 to Math.pow(2, unboundVariables.size).toInt - 1).foreach(x => {
				println(0 until unboundVariables.size map { pos: Int => (x & (1 << pos)) != 0 })
			})
		}

	}

}
