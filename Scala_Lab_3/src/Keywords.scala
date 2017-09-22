/**
  * Created by dalatar on 08.04.17.
  */
trait Keywords extends Scanner{
  private val keywords = Set("with","end","**")

  private def revealKeyword(start : Pos, pos : Pos, revealed : String) : Pos = pos.ch match {
    case a if keywords.count(_.startsWith(revealed + a)) > 0 => revealKeyword(start,pos.inc, revealed + a)
    case _ if keywords.contains(revealed) => pos
    case _ => start
  }


}