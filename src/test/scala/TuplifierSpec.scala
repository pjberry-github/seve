


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

    val obtained = Tuplifier.tuplify(caseClassOfPrimitives)
    val expected = (true, 'a', -1: Byte, -2: Short, 1000: Int, 10_000_000_000L, 1.2F, 2.3: Double, "abc")

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

  type Elem[X] = X match
    case String => Char



  def tuplify[P <: Product](p: P): Tuple = {
    Tuple.fromProduct(p)
  }
}
