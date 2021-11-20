package codecs

import org.typelevel.jawn.Facade.SimpleFacade
import org.typelevel.jawn.Parser

object Util {

  def parseJson(s: String): Option[Json] = Parser.parseFromString[Json](s).toOption

  def parseAndDecode[A](s: String)(implicit decoder: Decoder[A]): Option[A] =
    for {
      json <- parseJson(s)
      a <- decoder.decode(json)
    } yield a

  def renderJson[A](value: A)(implicit encoder: Encoder[A]): String =
    render(encoder.encode(value))

  private def render(json: Json): String = json match {
    case Json.Null => "null"
    case Json.Bool(b) => b.toString
    case Json.Num(n) => n.toString
    case Json.Str(s) => renderString(s)
    case Json.Arr(vs) => vs.map(render).mkString("[", ",", "]")
    case Json.Obj(vs) => vs.map { case (k, v) => s"${renderString(k)}:${render(v)}" }.mkString("{", ",", "}")
  }

  private def renderString(s: String): String = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      s.charAt(i) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ') sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"').toString
  }

  implicit val facade: SimpleFacade[Json] = new SimpleFacade[Json] {
    def jnull = Json.Null
    def jtrue = Json.Bool(true)
    def jfalse = Json.Bool(false)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = Json.Num(BigDecimal(s.toString))
    def jstring(s: CharSequence) = Json.Str(s.toString)
    def jarray(vs: List[Json]) = Json.Arr(vs)
    def jobject(vs: Map[String, Json]) = Json.Obj(vs)
  }

}