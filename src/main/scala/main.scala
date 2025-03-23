import scala.deriving.Mirror
import scala.deriving.Mirror.ProductOf

@main
def main(): Unit =
  demoManualMirror()

  println()

  demoSummonMirror()


def demoSummonMirror(): Unit =
  val caseClassProduct = CaseClassProduct("a", 1)

  val mirror = summon[Mirror.ProductOf[CaseClassProduct]]

  val mirrorProductCompilerTuple = Tuple.fromProductTyped(caseClassProduct)(using mirror)
  val mirrorProductCompilerCaseClass = mirror.fromProduct(mirrorProductCompilerTuple) // remember, Tuple and `case class` are both products

  println("***  Summon  ***")
  println(mirrorProductCompilerTuple)     //(a,1)
  println(mirrorProductCompilerCaseClass) //CaseClassProduct(a,1)

def demoManualMirror(): Unit =
  val caseClassProduct = CaseClassProduct("a", 1)

  val mirrorProductManual: ProductOf[CaseClassProduct]= new Mirror.Product {
    type MirroredType = CaseClassProduct
    type MirroredMonoType = CaseClassProduct
    type MirroredElemTypes <: Tuple

    override def fromProduct(p: Product): CaseClassProduct = {
      CaseClassProduct(p.productElement(0).asInstanceOf[String], p.productElement(1).asInstanceOf[Int])
    }
  }

  val mirrorProductManualTuple = Tuple.fromProductTyped(caseClassProduct)(using mirrorProductManual)
  val mirrorProductManualCaseClass = mirrorProductManual.fromProduct(mirrorProductManualTuple) // remember, Tuple and `case class` are both products

  println("***  Manual  ***")
  println(mirrorProductManualTuple)     //(a,1)
  println(mirrorProductManualCaseClass) //CaseClassProduct(a,1)



case class CaseClassProduct(parameterOne: String, parameterTwo: Int)


