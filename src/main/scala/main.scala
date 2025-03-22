import scala.deriving.Mirror
import scala.deriving.Mirror.ProductOf

@main
def main(): Unit =
  val caseClassProduct = CaseClassProduct("a", 1)

  val mirrorProductManual: ProductOf[CaseClassProduct]= new Mirror.Product {
    type MirroredType = CaseClassProduct
    type MirroredMonoType = CaseClassProduct
    type MirroredElemTypes <: Tuple

    override def fromProduct(p: Product): CaseClassProduct = {
      CaseClassProduct(p.productElement(0).asInstanceOf[String], p.productElement(1).asInstanceOf[Int])
    }
  }

  val x = Tuple.fromProductTyped(caseClassProduct)(using mirrorProductManual)
  println(x) //(a,1)

  // remember, Tuple and `case class` are both products
  val y = mirrorProductManual.fromProduct(x)
  print(y) //CaseClassProduct(a,1)


case class CaseClassProduct(parameterOne: String, parameterTwo: Int)


