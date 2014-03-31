package net.acira

import net.acira.bexprp.core._
import scala.reflect.ClassTag
import java.io.PrintStream
import net.acira.bexprp.visitors.{VariableBinder, PrettyPrinter}

package object bexprp {

	implicit class BexprpString(self: String) {

		def expression: Option[Expression] = BooleanExpressionParser.parse(self)

		def bind(variableBindings: (String, Boolean)*): Expression = bind(variableBindings.map(x => FreeVariable(x._1) -> x._2):_*)

		def bind[X: ClassTag](variableBindings: (Variable, Boolean)*): Expression = {
			expression.get.accept(new VariableBinder(variableBindings.toMap))
		}

		def prettyPrint: Unit = expression.get.prettyPrint
		def prettyPrint(on: PrintStream): Unit = expression.get.prettyPrint(on)

	}

}
