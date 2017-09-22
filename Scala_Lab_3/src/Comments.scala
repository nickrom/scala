import DomainTags ._
/**
  * Created by dalatar on 12.04.17.
  */
trait Comments extends Scanner{

 /* private def findComment (pos : Pos) : Pos = {
    //println(pos.ch == '*' && pos.col -1 == 0)
    if(pos.ch == '*' && pos.col -1 == 0)
      findCommentEnd(pos.inc)
    else
      pos
  }
*/
  private def findComment (pos : Pos) : Pos = pos.ch match {
    case '*' if pos.col -1 == 0 => findCommentEnd(pos.inc)
    case _ => pos
  }


  private def findCommentEnd (pos : Pos) : Pos = pos.ch match {
    case '\n' => pos
    case _ => findCommentEnd(pos.inc)
  }
/*
  private def findCommentEnd (pos : Pos) : Pos = {
    if(pos.ch != '\n')
      findCommentEnd(pos.inc)
    else
      pos
  }
*/

  override def scan ( start : Pos ) = {
    val follow = findComment(start)
    if(start != follow)
      (COMMENT,follow)
    else
      super.scan(start)
  }
}
