

@main
def main(): Unit =
  val thing = Thing("a", 1)

  println(thing._1)
  println(thing._2)

  val tuple = Tuple.fromProductTyped(thing)
  println(tuple)


case class Thing(parameterOne: String, parameterTwo: Int)


