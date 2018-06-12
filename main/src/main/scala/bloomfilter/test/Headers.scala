package bloomfilter.test

import shapeless._
import shapeless.::
import shapeless.tag.@@

trait Header[H] {
  def format(value: H): String
}

object Header {
  implicit val string: Header[String] =
    new Header[String] {
      def format(value: String): String = value
    }

  implicit def param[T](implicit p: Parameter[T]): Header[T] =
    new Header[T] {
      def format(value: T): String = p.name + " = " + p.format(value)
    }
}

trait Headers[HH <: HList] {
  def format(headers: HH): List[String]
}

object Headers {
  implicit val hnil: Headers[HNil] =
    new Headers[HNil] {
      def format(headers: HNil): List[String] = Nil
    }

  implicit def cons[H, T <: HList](implicit h: Header[H], hh: Headers[T]): Headers[H :: T] =
    new Headers[H :: T] {
      def format(headers: H :: T): List[String] = h.format(headers.head) :: hh.format(headers.tail)
    }
}
