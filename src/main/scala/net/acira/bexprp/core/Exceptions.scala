package net.acira.bexprp.core


abstract class BexprpException extends RuntimeException

case class ParseException(msg: String) extends BexprpException
case class EvaluationException(msg: String) extends BexprpException
