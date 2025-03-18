

@main
def main(): Unit =
  val thing = Thing("a", 1)

  println(thing._1)
  println(thing._2)


case class Thing(parameterOne: String, parameterTwo: Int)


