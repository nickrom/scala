import scala.util.matching.Regex
import DomainTags ._

/**
  * Created by dalatar on 08.04.17.
  */
trait Idents extends Scanner{


  private def findIdent (pos : Pos) : Pos = {

    if ((pos.ch >= 'a' && pos.ch <= 'z') || (pos.ch >= 'A' && pos.ch <= 'Z')) {
      findWord(pos.inc)
    } else if (pos.ch == '*') {
      findStar(pos.inc)
    } else
        pos
    }

  private def findWord (pos : Pos) : Pos = {
    if(pos.ch >= 'a' && pos.ch <= 'z')
      findWord(pos.inc)
    else
      pos
  }

  private def findStar (pos : Pos) : Pos = pos.ch match {
    case '*' => findStar(pos.inc)
    case _ => pos
  }



  override def scan ( start : Pos ) = {
    val follow =  findIdent(start)


    if (start != follow)
      (IDENT,follow)
    else
      super.scan (start)
  }

}
