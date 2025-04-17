import scala.compiletime.summonInline
import scala.deriving.Mirror

class TupleOperationsTestSuite extends munit.FunSuite {

  test("Simple Tuple Operations") {

    def processNonEmptyTuple(t: Tuple): String =
      t match
        case _: EmptyTuple => "Empty tuple"
        case tuple: NonEmptyTuple => s"Non-empty tuple $tuple"


    assertEquals(processNonEmptyTuple((1,2,3)), "Non-empty tuple (1,2,3)")
  }

  test("Simple Product to Tuple operation") {
    def processProduct[P <: Product](p: P)(using mirror: Mirror.ProductOf[P]): String =
      val tuple = Tuple.fromProductTyped(p)
      s"Non-empty tuple $tuple"


    case class SomeCaseClass(a: Int, b: Int, c: Int)
    assertEquals(processProduct(SomeCaseClass(1,2,3)), "Non-empty tuple (1,2,3)")
  }

  test("Less simple Product to Tuple operation") {
    def processProduct[P <: Product](p: P)(using mirror: Mirror.ProductOf[P]): String =
      val tuple = Tuple.fromProductTyped(p)
      tuple match
        case _: EmptyTuple => "Empty tuple"
        case tuple: NonEmptyTuple => s"Non-empty tuple $tuple"

    case class SomeCaseClass(a: Int, b: Int, c: Int)
    assertEquals(processProduct(SomeCaseClass(1, 2, 3)), "Non-empty tuple (1,2,3)")
  }

  test("Product to Tuple operation 1") {
    def processProduct[P <: Product](p: P)(using mirror: Mirror.ProductOf[P]): String =
      val tuple = Tuple.fromProductTyped(p)
      tuple match
        case _: EmptyTuple => "Empty tuple"
        case (h *: t) => s"Non-empty tuple with head $h and tail $t"

    case class SomeCaseClass(a: Int, b: Int, c: Int)
    assertEquals(processProduct(SomeCaseClass(1, 2, 3)), "Non-empty tuple with head 1 and tail (2,3)")
  }
}
