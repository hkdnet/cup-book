package net.hkdnet.parser

import org.scalatest._
import net.hkdnet.scell._

class FormulaParsersSpec extends FlatSpec with Matchers {
  """""" should "return an Empty" in {
    FormulaParsers.parse("") shouldEqual Empty
  }
  """1""" should "return a Number" in {
    FormulaParsers.parse("1") shouldEqual Number(1)
  }
  """A1""" should "return a Textual" in {
    FormulaParsers.parse("A1") shouldEqual Textual("A1")
  }
  """=1""" should "return a Number" in {
    FormulaParsers.parse("=1") shouldEqual Number(1)
  }
  """=A0:A1""" should "return a Range" in {
    FormulaParsers.parse("=A0:A1") shouldEqual Range(Coord(0, 0), Coord(1, 0))
  }
  """=F(1)""" should "return a Application" in {
    FormulaParsers.parse("=F(1)") shouldEqual Application("F", List(Number(1)))
  }
}
