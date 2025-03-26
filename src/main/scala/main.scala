

@main
def main(): Unit =
  demonstrateMirrorProvided()
  println()
  demonstrateMirrorSummonedThenUsed()
  println()
  demonstrateWithoutSummoningMirror()
  println()
  demonstrateExtensionOnTuple()


def demonstrateMirrorProvided(): Unit =
  import scala.deriving.Mirror

  val caseClassProduct = CaseClassProduct("a", 1)

  val mirrorProductManual: Mirror.ProductOf[CaseClassProduct]= new Mirror.Product {
    type MirroredType = CaseClassProduct
    type MirroredMonoType = CaseClassProduct
    type MirroredElemTypes <: Tuple

    override def fromProduct(p: Product): CaseClassProduct = {
      CaseClassProduct(p.productElement(0).asInstanceOf[String], p.productElement(1).asInstanceOf[Int])
    }
  }

  val tuple = Tuple.fromProductTyped(caseClassProduct)(using mirrorProductManual)
  val caseClass = mirrorProductManual.fromProduct(tuple) // remember, Tuple and `case class` are both products

  println("***  Manual  ***")
  println(tuple)     //(a,1)
  println(caseClass) //CaseClassProduct(a,1)


def demonstrateMirrorSummonedThenUsed(): Unit =
  import scala.deriving.Mirror

  val caseClassProduct = CaseClassProduct("b", 2)

  val mirror = summon[Mirror.ProductOf[CaseClassProduct]]

  val tuple = Tuple.fromProductTyped(caseClassProduct)(using mirror)
  val caseClass = mirror.fromProduct(tuple) // remember, Tuple and `case class` are both products

  println("***  Summon  ***")
  println(tuple)     //(b,2)
  println(caseClass) //CaseClassProduct(b,2)


def demonstrateWithoutSummoningMirror(): Unit =
  import scala.deriving.Mirror
  val caseClassProduct = CaseClassProduct("c", 3)

  val tuple = Tuple.fromProductTyped(caseClassProduct)
  val caseClass = summon[Mirror.ProductOf[CaseClassProduct]].fromProduct(tuple)

  println("***  Without Summoning Mirror  ***")
  println(tuple)     //(c,3)
  println(caseClass) //CaseClassProduct(c,3)


def demonstrateExtensionOnTuple(): Unit =
  val caseClassProduct = CaseClassProduct("d", 4)

  val tuple = Tuple.fromProductTyped(caseClassProduct)
  val caseClass = tuple.as[CaseClassProduct]

  println("***  Without Summoning Mirror  ***")
  println(tuple)     //(d,4)
  println(caseClass) //CaseClassProduct(d,4)

@main
def demonstrateNested(): Unit =
  val caseClassProduct = CaseClassProduct("e", 5)
  val containerCaseClass = ContainerCaseClass(caseClassProduct)

  val tuple = Tuple.fromProductTyped(containerCaseClass)
  val caseClass = tuple.as[ContainerCaseClass]

  println("***  Nested  ***")
  println(tuple)     //(d,4)
  println(caseClass) //CaseClassProduct(d,4)

case class ContainerCaseClass(caseClassProduct: CaseClassProduct)
case class CaseClassProduct(parameterOne: String, parameterTwo: Int)

extension (t: Tuple) {
  def as[P <: Product](using m: scala.deriving.Mirror.ProductOf[P]): P = {
    summon[deriving.Mirror.ProductOf[P]].fromProduct(t)
  }
}



