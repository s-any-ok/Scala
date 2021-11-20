package codecs

sealed trait Json {

  def decodeAs[A](implicit decoder: Decoder[A]): Option[A] = decoder.decode(this)
}

object Json {
  case object Null extends Json
  case class Bool(value: Boolean) extends Json
  case class Num(value: BigDecimal) extends Json
  case class Str(value: String) extends Json
  case class Obj(fields: Map[String, Json]) extends Json
  case class Arr(items: List[Json]) extends Json
}


trait Encoder[-A] {
  def encode(value: A): Json
  def transform[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B](value => this.encode(f(value)))
}

object Encoder extends EncoderInstances {
  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }
}

trait EncoderInstances {

  implicit val unitEncoder: Encoder[Unit] =
    Encoder.fromFunction(_ => Json.Null)

  implicit val intEncoder: Encoder[Int] =
    Encoder.fromFunction(n => Json.Num(BigDecimal(n)))

  implicit val stringEncoder: Encoder[String] =
    Encoder.fromFunction(n => Json.Str(n))

  implicit val boolEncoder: Encoder[Boolean] =
    Encoder.fromFunction(n => Json.Bool(n))

  implicit def listEncoder[A](implicit encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => Json.Arr(as.map(encoder.encode)))

}

trait ObjectEncoder[-A] extends Encoder[A] {
  // Refines the encoding result to `Json.Obj`
  def encode(value: A): Json.Obj

  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { case (a, b) =>
      Json.Obj(this.encode(a).fields ++ that.encode(b).fields)
    }
}

object ObjectEncoder {
  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = new ObjectEncoder[A] {
    def encode(value: A): Json.Obj = f(value)
  }

  def field[A](name: String)(implicit encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.encode(a))))
}

trait Decoder[+A] {
  def decode(data: Json): Option[A]

  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction { json =>
      this.decode(json).zip(that.decode(json))
    }

  def transform[B](f: A => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))
}

object Decoder extends DecoderInstances {

  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)
}

trait DecoderInstances {

  implicit val unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction { case Json.Null => () }

  implicit val intDecoder: Decoder[Int] =
    Decoder.fromPartialFunction { case Json.Num(n) if n.isValidInt => n.toInt}

  implicit val stringDecoder: Decoder[String] =
    Decoder.fromPartialFunction { case Json.Str(str) => str }

  implicit val boolDecoder: Decoder[Boolean] =
    Decoder.fromPartialFunction { case Json.Bool(bool) => bool.booleanValue() }

  implicit def listDecoder[A](implicit decoder: Decoder[A]): Decoder[List[A]] =
    Decoder.fromFunction {
      case Json.Arr(x) =>
        val items = x.map(decoder.decode)
        if (items.forall(_.isDefined)) Some(items.map(_.get))
        else Option.empty
      case _ => Option.empty
    }

  def field[A](name: String)(implicit decoder: Decoder[A]): Decoder[A] =
    Decoder.fromFunction {
      case Json.Obj(x) if x.contains(name) => decoder.decode(x(name) )
      case _ => None
    }
}

case class Person(name: String, age: Int)

object Person extends PersonCodecs

trait PersonCodecs {

  implicit val personEncoder: Encoder[Person] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[Int]("age"))
      .transform[Person](user => (user.name, user.age))

  implicit val personDecoder: Decoder[Person] =
    Decoder.field[String]("name")
      .zip(Decoder.field[Int]("age"))
      .transform[Person](user => Person(user._1, user._2))
}

case class Contacts(people: List[Person])

object Contacts extends ContactsCodecs

trait ContactsCodecs {

  implicit val contactsEncoder: Encoder[Contacts] =
  ObjectEncoder.field[List[Person]]("people")
    .transform[Contacts](contact => contact.people)

  implicit val contactsDecoder: Decoder[Contacts] =
    Decoder.field[List[Person]]("people")
      .transform[Contacts](contact => Contacts(contact))
}

object Main {
  import Util._

  def main(args: Array[String]): Unit = {
    println(renderJson(42))
    println(renderJson("foo"))

    val maybeJsonString = parseJson(""" "foo" """)
    val maybeJsonObj    = parseJson(""" { "name": "Alice", "age": 42 } """)
    val maybeJsonObj2   = parseJson(""" { "name": "Alice", "age": "42" } """)
    // Uncomment the following lines as you progress in the assignment
     println(maybeJsonString.flatMap(_.decodeAs[Int]))
     println(maybeJsonString.flatMap(_.decodeAs[String]))
     println(maybeJsonObj.flatMap(_.decodeAs[Person]))
     println(maybeJsonObj2.flatMap(_.decodeAs[Person]))
     println(renderJson(Person("Bob", 66)))
  }
}
