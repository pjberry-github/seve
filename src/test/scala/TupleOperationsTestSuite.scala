import scala.compiletime.{erasedValue, summonInline}
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

  test("Try AI") {
    import scala.deriving.Mirror

    /**
     * Recursively processes a tuple, converting case classes into tuples.
     * The result will contain only primitives and tuples.
     */
    def flattenCaseClasses(tuple: Tuple): Tuple = {
      tuple match {
        case EmptyTuple => EmptyTuple
        case (head *: tail) =>
          head match {
            case p: Product =>
              // Use the runtime productElement method to manually create a tuple
              val headElements = (0 until p.productArity).map(p.productElement)
              val headAsTuple = Tuple.fromArray(headElements.toArray)
              // Process the new tuple recursively
              val processedHead = flattenCaseClasses(headAsTuple)
              (processedHead *: flattenCaseClasses(tail))
            case _ =>
              // Head is not a Product, keep it as is
              (head *: flattenCaseClasses(tail))
          }
      }
    }


    // Example usage
    case class Person(name: String, age: Int)
    case class Department(id: Int, person: Person)

    // Example 1: Simple tuple with one case class
    val tuple1 = (1, "hello", Person("John", 30), true)
    val result1 = flattenCaseClasses(tuple1)
    assertEquals(result1, (1, "hello", ("John", 30), true))
    // Result: (1, "hello", ("John", 30), true)

    // Example 2: Nested case classes
    val tuple2 = (Department(1, Person("Jane", 25)), 42)
    val result2 = flattenCaseClasses(tuple2)
    assertEquals(result2, ((1, ("Jane", 25)), 42))


    case class SomeCaseClass(a: Int, b: Int, c: Int)
    case class SomeOtherCaseClass(someCaseClass: SomeCaseClass, e: Int)

    val someOtherCaseClass = SomeOtherCaseClass(SomeCaseClass(1, 2, 3), 4)
    val tuple = Tuple.fromProductTyped(someOtherCaseClass)
    assertEquals(flattenCaseClasses(tuple), ((1, 2, 3), 4))

  }

  
}
