package codecs

sealed trait Json:
  def decodeAs[A](using decoder: Decoder[A]): Option[A] = decoder.decode(this)

object Json:
  case object Null extends Json
  case class Bool(value: Boolean) extends Json
  case class Num(value: BigDecimal) extends Json
  case class Str(value: String) extends Json
  case class Obj(fields: Map[String, Json]) extends Json
  case class Arr(items: List[Json]) extends Json

trait Encoder[-A]:

  def encode(value: A): Json
  def transform[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B](value => this.encode(f(value)))

end Encoder

object Encoder extends EncoderInstances:

  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }

end Encoder

trait EncoderInstances:

  given unitEncoder: Encoder[Unit] =
    Encoder.fromFunction(_ => Json.Null)

  given intEncoder: Encoder[Int] =
    Encoder.fromFunction(n => Json.Num(BigDecimal(n)))

  given stringEncoder: Encoder[String] =
    Encoder.fromFunction(n => Json.Str(n))

  given boolEncoder: Encoder[Boolean] =
    Encoder.fromFunction(n => Json.Bool(n))

  given listEncoder[A](using encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => Json.Arr(as.map(encoder.encode)))

end EncoderInstances

trait ObjectEncoder[-A] extends Encoder[A]:
  // Refines the encoding result to `Json.Obj`
  def encode(value: A): Json.Obj
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction { (a, b) =>
      Json.Obj(this.encode(a).fields ++ that.encode(b).fields)
    }
end ObjectEncoder

object ObjectEncoder:
  def fromFunction[A](f: A => Json.Obj): ObjectEncoder[A] = new ObjectEncoder[A] {
    def encode(value: A): Json.Obj = f(value)
  }

  def field[A](name: String)(using encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => Json.Obj(Map(name -> encoder.encode(a))))

end ObjectEncoder

trait Decoder[+A]:

  def decode(data: Json): Option[A]

  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction { json =>
      this.decode(json).zip(that.decode(json))
    }

  def transform[B](f: A => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))

end Decoder

object Decoder extends DecoderInstances:

  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)

end Decoder

trait DecoderInstances:

  given unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction { case Json.Null => () }

  given intDecoder: Decoder[Int] =
    Decoder.fromPartialFunction { case Json.Num(n) if n.isValidInt => n.toInt}

  given stringDecoder: Decoder[String] =
    Decoder.fromPartialFunction { case Json.Str(str) => str }

  given boolDecoder: Decoder[Boolean] =
    Decoder.fromPartialFunction { case Json.Bool(bool) => bool.booleanValue() }

  given listDecoder[A](using decoder: Decoder[A]): Decoder[List[A]] =

    // Decode the provided `item` with the provided `decoder`. If this succeeds,
    // return the decoded item **prepended** to the `previouslyDecodedItems`.
    def decodeAndPrepend(item: Json, previouslyDecodedItems: List[A]): Option[List[A]] = {
      val decItem = decoder.decode(item)
      if (decItem.isDefined)
        Some(decItem.get +: previouslyDecodedItems)
      else None
    }

    // Decode the provided `item` only if the previous items were successfully decoded.
    // In case `maybePreviouslyDecodedItems` is `None` (which means that at least
    // one of the previous items failed to be decoded), return `None`.
    // Otherwise, decode the provided `item` and prepend it to the previously
    // decoded items (use the method `decodeAndPrepend`).
    def processItem(item: Json, maybePreviouslyDecodedItems: Option[List[A]]): Option[List[A]] = {
      maybePreviouslyDecodedItems match  {
        case None => None
        case Some(items) => decodeAndPrepend(item, items)
      }
    }
    // Decodes all the provided JSON items. Fails if any item fails to
    // be decoded.
    // Iterates over the items, and tries to decode each item if the
    // previous items could be successfully decoded.
    def decodeAllItems(items: List[Json]): Option[List[A]] =
      items.foldRight(Some(List.empty[A]))(processItem)
    // Finally, write a decoder that checks whether the JSON value to decode
    // is a JSON array.
    //   - if it is the case, call `decodeAllItems` on the array items,
    //   - otherwise, return a failure (`None`)
    Decoder.fromFunction {
      case Json.Arr(arr) => decodeAllItems(arr)
      case _ => None
    }

  def field[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
    Decoder.fromFunction {
      case Json.Obj(x) if x.contains(name) => decoder.decode(x(name))
      case _ => None
    }

end DecoderInstances

case class Person(name: String, age: Int)

object Person extends PersonCodecs

trait PersonCodecs:

  /** The encoder for `Person` */
  given Encoder[Person] =
    ObjectEncoder.field[String]("name")
      .zip(ObjectEncoder.field[Int]("age"))
      .transform[Person](user => (user.name, user.age))

  given Decoder[Person] =
    Decoder.field[String]("name")
      .zip(Decoder.field[Int]("age"))
      .transform[Person](user => Person(user._1, user._2))

end PersonCodecs

case class Contacts(people: List[Person])

object Contacts extends ContactsCodecs

trait ContactsCodecs:

  given Encoder[Contacts] =
    ObjectEncoder.field[List[Person]]("people")
      .transform[Contacts](contact => contact.people)

  given Decoder[Contacts] =
    Decoder.field[List[Person]]("people")
      .transform[Contacts](contact => Contacts(contact))

end ContactsCodecs

import Util.*

@main def run(): Unit =
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

  val encoder = Encoder.fromFunction[List[Contacts]](as => Json.Arr(as.map(
    ObjectEncoder.field[List[Person]]("people")
      .transform[Contacts](contact => contact.people).encode
  )))

  val contacts = List(Person("dwd", 1), Person("dwd", 1))
  println(contacts)
  println(encoder.encode(List(Contacts(contacts))))
