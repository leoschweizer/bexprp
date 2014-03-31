package net.acira.bexprp.core

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object BooleanExpressionParser extends StandardTokenParsers {

	lexical.delimiters ++= List("(", ")", "<->", "↔", "←", "<-", "→", "->")
	lexical.reserved ++= Set(
		"true", "false",
		"and", "&", "∧",
		"or", "|", "∨",
		"not", "!", "¬",
		"->", "→",
		"<-", "←",
		"<->", "↔", "="
	)

	def expression: Parser[Expression] = equivalence

	def equivalence: Parser[Expression] = leftImplication ~ rep("<->" ~> leftImplication | "↔" ~> leftImplication | "=" ~> leftImplication) ^^ {
		case l ~ Nil => l
		case l ~ xs => xs.foldLeft(l)(Equivalence(_, _))
	}

	def leftImplication: Parser[Expression] = implication ~ rep("<-" ~> implication | "←" ~> implication) ^^ {
		case l ~ Nil => l
		case l ~ xs => xs.foldLeft(l)(LeftImplication(_, _))
	}

	def implication: Parser[Expression] = or ~ rep("->" ~> or | "→" ~> or) ^^ {
		case l ~ Nil => l
		case l ~ xs => xs.foldLeft(l)(Implication(_, _))
	}

	def or: Parser[Expression] = and ~ rep("or" ~> and | "|" ~> and | "∨" ~> and) ^^ {
		case l ~ Nil => l
		case l ~ xs => xs.foldLeft(l)(Or(_, _))
	}

	def and: Parser[Expression] = not ~ rep("and" ~> not | "&" ~> not | "∧" ~> not) ^^ {
		case l ~ Nil => l
		case l ~ xs => xs.foldLeft(l)(And(_, _))
	}

	def not: Parser[Expression] = rep("not" | "!" | "¬") ~ primary ^^ {
		case Nil ~ p => p
		case xs ~ p => xs.foldRight(p)((_, acc) => Not(acc))
	}

	def primary: Parser[Expression] = "(" ~> expression <~ ")" | literal

	def literal: Parser[Expression] = ident ^^ {
		case "true" => Constant(true)
		case "false" => Constant(false)
		case n => FreeVariable(n)
	}

	def parse(s: String) = expression(new lexical.Scanner(s)) match {
		case Success(p, _) => Option(p)
		case Failure(msg, _) => println(msg); None
		case Error(msg, _) => println(msg); None
	}

}
