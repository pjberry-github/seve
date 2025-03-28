import scala.:+
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror


class TuplifierSpec extends munit.FunSuite {

  test("case class of primitives") {
    val caseClassOfPrimitives = CaseClassOfPrimitives(
      boolean = true,
      char = 'a',
      byte = -1,
      short = -2,
      int = 1000,
      long = 10_000_000_000L,
      float = 1.2F,
      double = 2.3,
      string = "abc"
    )

    val initialTuple = Tuple.fromProductTyped(caseClassOfPrimitives)
    val obtained = Tuplifier.deconstruct(Tuple(), initialTuple)

    val expected = (true, 'a', -1: Byte, -2: Short, 1000: Int, 10_000_000_000L, 1.2F, 2.3: Double, "abc")

    assertEquals(obtained, expected)
  }

  test("case class with a case class") {
    case class CaseClassOfCaseClass(caseClassOfPrimitives: CaseClassOfPrimitives)

    val caseClassOfPrimitives = CaseClassOfPrimitives(
      boolean = true,
      char = 'a',
      byte = -1,
      short = -2,
      int = 1000,
      long = 10_000_000_000L,
      float = 1.2F,
      double = 2.3,
      string = "abc"
    )

    val caseClassOfCaseClass = CaseClassOfCaseClass(caseClassOfPrimitives)

    val initialTuple = Tuple.fromProductTyped(caseClassOfCaseClass)
    val obtained = Tuplifier.deconstruct(Tuple(), initialTuple)
    val expected = Tuple((true, 'a', -1: Byte, -2: Short, 1000: Int, 10_000_000_000L, 1.2F, 2.3: Double, "abc"))

    assertEquals(obtained, expected)
  }

}


case class CaseClassOfPrimitives(
  boolean: Boolean,
  char: Char,
  byte: Byte,
  short: Short,
  int: Int,
  long: Long,
  float: Float,
  double: Double,
  string: String
)

object Tuplifier {

//  def tuplify[P <: Product](p: P): Tuple = {
//    Tuple.fromProduct(p)
//  }
//
//  private def deconstructProduct[P <: Product](product: P)(using Mirror.ProductOf[P]): Tuple = {
//    val x = Tuple.fromProductTyped(product)
//
//  }

//  Tuple.fromProductTyped(product) match {
//private def deconstruct[P <: Product](resultTuple: Tuple, parts: Tuple)(using mirror: Mirror.ProductOf[P]): Tuple = {
  def deconstruct(resultTuple: Tuple, parts: Tuple): Tuple = {
    parts match
      case EmptyTuple => resultTuple
      case head *: tail =>
        

        head match
          case p: Product =>
            val asTuple = deconstruct(Tuple(), Tuple.fromProduct(p))
            println(asTuple)
            deconstruct(resultTuple, asTuple *: tail)
          case _ =>
            deconstruct(resultTuple :* head, tail)

  }
}


  
