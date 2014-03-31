package net.acira

import net.acira.bexprp.core._
import scala.reflect.ClassTag
import java.io.PrintStream
import scala.util.Try

package object bexprp {

	implicit class BexprpString(self: String) {

		def expression: Try[Expression] = BooleanExpressionParser.parse(self)

		def bind(variableBindings: (String, Boolean)*): Try[Expression] = expression.map(_.bind(variableBindings: _*))
		def bind[X: ClassTag](variableBindings: (Variable, Boolean)*): Try[Expression] = expression.map(_.bind(variableBindings: _*))

		def prettyPrint: Unit = expression.get.prettyPrint
		def prettyPrint(on: PrintStream): Unit = expression.get.prettyPrint(on)

	}

}
