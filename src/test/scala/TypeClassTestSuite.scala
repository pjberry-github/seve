
class TypeClassTestSuite extends munit.FunSuite {

  /*
    - Type Classes are "things" that allow polymorphism without have to extend the "thing"
    - An implementation of the "thing" is done by Scala's `Given Instances`
    - A `Given Instance` defines "canonical" values of certain types and are used for providing values
      for `Using Clauses`
    - `Using Clauses`, sometimes called context parameters, are bits of code that can be filled given their
       context. That is, the compiler can figure out what the bits of code need to be in order for things
       to work.
   */

  test("Yelling about two different types.") {

    /** Consider something that takes a string and yells it  */
    object Yeller_String {
      def yell(string: String) =  string.toUpperCase + "!"
    }

    assertEquals(Yeller_String.yell("hey"), "HEY!")

    /** What about if we need to yell about Ints? */
    object Yeller_Int {
      def yell(i: Int) =  i.toString + "!"
    }

    assertEquals(Yeller_Int.yell(212), "212!")
  }

  test("Hey, we are going to need to yell a whole bunch of types!") {
    /** At this point, we can use adhoc polymorphism via function overloading */
    object Yeller {
      def yell(string: String) =  string.toUpperCase + "!"
      def yell(i: Int) =  i.toString + "!"
      def yell(boolean: Boolean) = boolean.toString.toUpperCase + "!"
    }

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
  }

  test("Hey, we are going to need yell even more and I don't know what the types are!") {
    /** We can genericize the yell method*/
    object Yeller {
      def yell[T](t: T, stringer: T => String) = stringer(t).toUpperCase + "!"
    }

    assertEquals(Yeller.yell("hey", identity), "HEY!")
    assertEquals(Yeller.yell(212, i => i.toString), "212!")
    assertEquals(Yeller.yell(true, b => b.toString), "TRUE!")

    /** What about some type we just ended up with? */
    case class SomeType(argument: String)
    assertEquals(Yeller.yell(SomeType("argument"), s => s.argument), "ARGUMENT!")
  }

  test("What about when things get nested?") {
    object Yeller {
      def yell[T](t: T, stringer: T => String) = stringer(t).toUpperCase + "!"
    }

    /** What about some type we just ended up with? */
    case class InnerCaseClass(argument: String)
    case class SomeType(argumentOne: InnerCaseClass, argumentTwo: Int)
    assertEquals(Yeller.yell(SomeType(InnerCaseClass("inner argument"), 212), s => s.argumentOne.argument + " " + s.argumentTwo.toString), "INNER ARGUMENT 212!")
  }

  test("Wait!  This is getting nasty and it looks like we could use some reuse") {
    object Yeller:
      def yell[T](t: T, stringer: T => String) = stringer(t).toUpperCase + "!"

    case class InnerCaseClass(argument: String)
    case class SomeType(argumentOne: InnerCaseClass, argumentTwo: Int)

    val stringStringer: String => String = s => identity(s)
    val intStringer: Int => String = i => i.toString
    val booleanString: Boolean => String = b => b.toString
    val innerCaseClassStringer: InnerCaseClass => String = ic => ic.argument
    val someTypeStringer: SomeType => String = st => innerCaseClassStringer(st.argumentOne) + " " + intStringer(st.argumentTwo)

    assertEquals(Yeller.yell("hey", stringStringer), "HEY!")
    assertEquals(Yeller.yell(212, intStringer), "212!")
    assertEquals(Yeller.yell(true, booleanString), "TRUE!")
    assertEquals(Yeller.yell(SomeType(InnerCaseClass("inner argument"), 212), someTypeStringer), "INNER ARGUMENT 212!")
  }

  test("Hmm... As there is more and different nesting going on, I'll want to compose my stringers from other stringers") {
    object Yeller:
      def yell[T](t: T, stringer: T => String) = stringer(t).toUpperCase + "!"

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    val stringStringer: String => String = s => identity(s)
    val intStringer: Int => String = i => i.toString
    val booleanString: Boolean => String = b => b.toString
    val innerCaseClassOneStringer : InnerCaseClassOne => String = innerCaseClassOne => innerCaseClassOne.argument
    val innerCaseClassTwoStringer: InnerCaseClassTwo => String = innerCaseClassTwo => innerCaseClassTwo.argument.toString
    val someTypeStringer: SomeType => String = st => innerCaseClassOneStringer(st.argumentOne) + " " + intStringer(st.argumentTwo) + " " +  innerCaseClassTwoStringer(st.argumentThree)

    assertEquals(Yeller.yell("hey", stringStringer), "HEY!")
    assertEquals(Yeller.yell(212, intStringer), "212!")
    assertEquals(Yeller.yell(true, booleanString), "TRUE!")
    assertEquals(Yeller.yell(SomeType(InnerCaseClassOne("inner argument"), 212, InnerCaseClassTwo(121)), someTypeStringer), "INNER ARGUMENT 212 121!")
  }

  test("Given/Using clauses") {
    trait Yellable[T]:
      def scream(t: T): String

    given stringYellable: Yellable[String] =
      new Yellable[String]:
        override def scream(t: String): String = t.toUpperCase() + "!"

    given intYellable: Yellable[Int] =
      new Yellable[Int]:
        override def scream(t: Int): String = t.toString + "!"

    def scream[T](t: T)(using screamable: Yellable[T]): String =
      screamable.scream(t)

    assertEquals(scream("hey"), "HEY!")
    assertEquals(scream(111), "111!")
  }
}


//    given stringScreamableAsFunc: Yellable[String] =
//      (t: String) => t.toUpperCase() + "!"
