package net.acira

import net.acira.bexprp.core._
import scala.reflect.ClassTag
import java.io.PrintStream

package object bexprp {

	implicit class BexprpString(self: String) {

		def expression: Option[Expression] = BooleanExpressionParser.parse(self)

		def bind(variableBindings: (String, Boolean)*): Expression = expression.get.bind(variableBindings: _*)
		def bind[X: ClassTag](variableBindings: (Variable, Boolean)*): Expression = expression.get.bind(variableBindings: _*)

		def prettyPrint: Unit = expression.get.prettyPrint
		def prettyPrint(on: PrintStream): Unit = expression.get.prettyPrint(on)

	}

}
