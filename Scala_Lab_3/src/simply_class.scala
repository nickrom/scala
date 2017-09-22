class Pos private ( val prog : String , val offs : Int , val line : Int , val col : Int ) {
  def this (prog : String) = this (prog,0,1,1)
  def ch: Int = if (offs == prog.length) '\uFFFF' else prog(offs)
  def inc = ch match {
      case '\n' => new Pos ( prog , offs +1 , line +1 , 1)
      case '\uFFFF' => this
      case _ => new Pos ( prog , offs +1 , line , col +1)
  }
  override def toString = "(" + line + " , " + col + ")"
}

object DomainTags extends Enumeration {
  type Tag = Value
  val WHITESPACE , KEYS , IDENT , COMMENT , ERROR , END_OF_PROGRAM = Value
}

import DomainTags ._


class Scanner {
  def scan ( start : Pos ): ( Tag , Pos ) =
    (ERROR, start.inc )
}

class Token ( val start : Pos , scanner : Scanner ) {
  val ( tag , follow ) = start.ch match {
    case '\uFFFF' => ( END_OF_PROGRAM , start )
    case _ => scanner.scan ( start )
  }
  def image = {
    //println(tag, ",", start.offs, ",", follow.offs, ",", start.prog.length)
    start.prog.substring (start.offs , follow.offs )
  }
  def next = new Token ( follow , scanner )
}

trait Whitespaces extends Scanner {
  private def missWhitespace ( pos : Pos ): Pos = pos.ch match {
    case ' ' => missWhitespace ( pos.inc )
    case '\t' => missWhitespace ( pos.inc )
    case '\n' => missWhitespace ( pos.inc )
    case _ => pos
  }

  override def scan ( start : Pos ) = {
    val follow =  missWhitespace(start)
    if ( start != follow )
      (WHITESPACE,follow)
    else
      super.scan (start)
  }

}