import DomainTags ._
/**
  * Created by dalatar on 08.04.17.
  */
object Main extends App {
  val lines = scala.io.Source.fromFile("/home/foromik/test.txt").mkString
  var t = new Token(
    new Pos(lines),
    new Scanner
      with Whitespaces
      with Idents
      with Comments
  )

  while (t.tag != END_OF_PROGRAM) {

    if(t.tag == IDENT && (t.image.equals("with") || t.image.equals("end") || t.image.equals("**")))
      println(KEYS + " " + t.start + "-" + t.follow + ": " + t.image)
    else if(t.tag == ERROR || (t.tag == IDENT && t.image.length % 2 == 0 && !t.image.contains('*')) || (t.tag == IDENT && t.image.matches("^[a-zA-Z]*$") && t.image.contains('*')) )
      println("syntax error: " + t.start + "-" + t.follow + ": " + t.image)
    else {
      println(t.tag.toString + " " + t.start + "-" + t.follow + ": " + t.image)
    }
    t = t.next
  }
}
