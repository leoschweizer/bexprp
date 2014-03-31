package net.acira.bexprp.core


abstract class BexprpException extends RuntimeException

case class ParseException(msg: String) extends BexprpException
