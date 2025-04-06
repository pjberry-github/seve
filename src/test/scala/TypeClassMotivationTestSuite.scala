import scala.compiletime.summonInline
import scala.deriving.Mirror

class TypeClassMotivationTestSuite extends munit.FunSuite {

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
    /** At this point, we can use *adhoc polymorphism* via function overloading */
    object Yeller {
      def yell(string: String) =  string.toUpperCase + "!"
      def yell(i: Int) =  i.toString + "!"
      def yell(boolean: Boolean) = boolean.toString.toUpperCase + "!"
    }

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
  }

  test("Let's clean this up") {
    object Yeller {
      def yell(string: String) = performYell(string)
      def yell(i: Int) = performYell(i.toString)
      def yell(boolean: Boolean) = performYell(boolean.toString)

      private def performYell(s: String) = s.toUpperCase + "!"
    }

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
  }

  /**
   * We could continue down the path of adding in methods for new classes as the come up.  If there aren't more things
   * to yell, this could be fine.  If it there are more things to yell, and if we don't know what they are going to be,
   * we need a way to decouple the Yeller and what it yells about.
   *
   * One way we could do that is to use the OO-style adapter pattern.
   *
   * Our implementation is something that for a type T, we can get a string, which then can be yelled.  The adapter
   * will provide the String to the Yeller
   */
  test("What's it look like with an adapter?") {
    trait YellerAdapter {
      def asString: String
    }

    object Yeller {
      def yell(yellee: YellerAdapter) = yellee.asString.toUpperCase + "!"
    }

    case class StringYellerAdapter(string: String) extends YellerAdapter {
      def asString = string
    }

    case class IntYellerAdapter(int: Int) extends YellerAdapter{
      override def asString = int.toString
    }

    case class BooleanYellerAdapter(boolean: Boolean) extends YellerAdapter {
      override def asString = boolean.toString
    }

    /** What about some type we just ended up with? */
    case class SomeType(argument: String)

    case class SomeTypeYellerAdapter(someType: SomeType) extends YellerAdapter {
      override def asString = someType.argument
    }

    assertEquals(Yeller.yell( StringYellerAdapter("hey") ), "HEY!")
    assertEquals(Yeller.yell( IntYellerAdapter(212) ), "212!")
    assertEquals(Yeller.yell( BooleanYellerAdapter(true) ), "TRUE!")
    assertEquals(Yeller.yell( SomeTypeYellerAdapter(SomeType("argument")) ), "ARGUMENT!")
  }

  /**
   * What we are doing with YellerAdapter is old-fashioned polymorphism.  For the YellerAdapter, we have many different
   * implementations.
   *
   * We can use Scala's type classes to achieve the same result.
   *
   * Type Classes are "things" that allow polymorphism without have to extend the "thing".  In our case, it will be the
   * `YellerAdapter`
   *
   * An implementation of the "thing" is done by Scala's `Given Instances`.
   *
   * A `Given Instance` defines "canonical" values of certain types and are used for providing values for
   * `Using Clauses`.
   *
   * `Using Clauses`, sometimes called context parameters, are bits of code that can be filled given their context. That
   *  is, the compiler can figure out what the bits of code need to be in order for things to work.
   */
  test("What's it look like with a Type Class?") {
    trait YellerAdapter[T] {
      extension (t: T) def asString(): String
    }

    object Yeller {
      def yell[T](yellee: YellerAdapter[T]) = yellee.asString().toUpperCase + "!"
    }
//
//    case class SomeType(argument: String)
//
    given x: YellerAdapter[String] = YellerAdapter[String] with extension(s: String) {
      def asString(): String = s
    }

    //    given YellerAdapter[Int]:
//      extension (int: Int) def yell: String = int.toString
//
//    given YellerAdapter[Boolean]:
//      extension (boolean: Boolean) def yell: String = boolean.toString
//
//    given YellerAdapter[SomeType]:
//      extension (someType: SomeType) def yell: String = someType.argument
//
    assertEquals(Yeller.yell("hey"), "HEY!")
//    assertEquals(Yeller.yell(212), "212!")
//    assertEquals(Yeller.yell(true), "TRUE!")
//    assertEquals(Yeller.yell(SomeType("argument")), "ARGUMENT!")
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

  test("Can we make the composition part of the abstraction?") {
    trait Yeller[T](t: T, stringer: T => String):
      def yell() = stringer(t).toUpperCase + "!"

      /** this may be too "clever" and hacky */
      def add[V](yeller: Yeller[V]) = new Yeller[String](this.yell().replace("!", "") + " " + yeller.yell().replace("!", ""), identity) {}

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** Note how the value of T varies, but it's the same function T => String */
    val stringStringer: String => String = s => identity(s)
    case class StringYeller(t: String) extends Yeller[String](t, stringStringer)

    val intStringer: Int => String = i => i.toString
    case class IntYeller(t: Int) extends Yeller[Int](t, intStringer)

    val booleanStringer: Boolean => String = b => b.toString
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t, booleanStringer)

    val innerCaseClassOneStringer: InnerCaseClassOne => String = innerCaseClassOne => innerCaseClassOne.argument
    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t, innerCaseClassOneStringer)

    val innerCaseClassTwoStringer: InnerCaseClassTwo => String = innerCaseClassTwo => innerCaseClassTwo.argument.toString
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t, innerCaseClassTwoStringer)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("This isn't so bad.  We should clean this up.") {
    trait Yeller[T](t: T, stringer: T => String):
      def yell() = stringer(t).toUpperCase + "!"

      /** this may be too "clever" and hacky */
      def add[V](yeller: Yeller[V]) = {
        new Yeller[String](this.yell().replace("!", "") + " " + yeller.yell().replace("!", ""), identity) {}
      }

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t, identity)
    case class IntYeller(t: Int) extends Yeller[Int](t, _.toString)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t, _.toString)

    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t, _.argument)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t, _.argument.toString)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("Some more clean up.") {
    trait Yeller[T](t: T, stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString, identity) {}
      }

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t, identity)
    case class IntYeller(t: Int) extends Yeller[Int](t, _.toString)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t, _.toString)

    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t, _.argument)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t, _.argument.toString)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")
  }

  /** we note that the value of T isn't going to change how T is converted into a string (at least for what we are doing)
   *  and, we might want to have some convenient way of "storing" how a T is converted.
   * */
  test("Set up using") {
    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString) {}
      }

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t)(using identity)
    case class IntYeller(t: Int) extends Yeller[Int](t)(using _.toString)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t)(using _.toString)

    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t)(using _.argument)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t)(using _.argument.toString)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("Set up givens") {

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    given stringStringer: Function1[String, String] = identity
    given intStringer: (Int => String) = i => i.toString
    given booleanString: (Boolean => String) = b => b.toString
    given innerCaseClassOneStringer: (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    given innerCaseClassTwoStringer: (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString

    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString) {}
      }


    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t)
    case class IntYeller(t: Int) extends Yeller[Int](t)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t)
    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("Maybe it's a matter of defining a way for each class of interest to add or string?") {
    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    given stringStringer: Function1[String, String] = identity
    given intStringer: (Int => String) = i => i.toString
    given booleanString: (Boolean => String) = b => b.toString
    given innerCaseClassOneStringer: (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    given innerCaseClassTwoStringer: (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString

    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString) {}
      }

    object InnerCaseClassOne {
      extension (innerClassOne: InnerCaseClassOne)
        def asYeller: Yeller[String] = new Yeller(innerClassOne.argument) {}

        def add[T](t: T)(using stringer: T => String): Yeller[String] = innerClassOne.asYeller.add(new Yeller[T](t) {})
    }


    extension (innerClassTwo: InnerCaseClassTwo)
      def asYeller: Yeller[String] = new Yeller(innerClassTwo.argument.toString) {}

      def add[T](t: T)(using stringer: T => String): Yeller[String] = innerClassTwo.asYeller.add(new Yeller[T](t) {})


    InnerCaseClassOne("inner argument").add(212).add(InnerCaseClassTwo(121).asYeller)

    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t)
    case class IntYeller(t: Int) extends Yeller[Int](t)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t)
    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")

    /** it still has that asYeller...  I think the add is needs to be non-yeller in the trait */
    assertEquals(InnerCaseClassOne("inner argument").add(212).add(InnerCaseClassTwo(121).asYeller).yell(), "INNER ARGUMENT 212 121!")
  }

  test("Let's make the add in the trait not dependent upon a wrapped type") {
    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    given stringStringer: Function1[String, String] = identity

    given intStringer: (Int => String) = i => i.toString

    given booleanString: (Boolean => String) = b => b.toString

    given innerCaseClassOneStringer: (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument

    given innerCaseClassTwoStringer: (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString

    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString) {}
      }

      def add[V](v: V)(using stringerFunc: V => String) = {
        val vYeller = new Yeller[V](v){}
        val asString = stringer(this.value) + " " + vYeller.stringer(vYeller.value)
        new Yeller[String](asString) {}
      }

    object InnerCaseClassOne {
      extension (innerClassOne: InnerCaseClassOne)
        def asYeller: Yeller[String] = new Yeller(innerClassOne.argument) {}

        def add[T](t: T)(using stringer: T => String): Yeller[String] = innerClassOne.asYeller.add(new Yeller[T](t) {})
    }


    extension (innerClassTwo: InnerCaseClassTwo)
      def asYeller: Yeller[String] = new Yeller(innerClassTwo.argument.toString) {}

      def add[T](t: T)(using stringer: T => String): Yeller[String] = innerClassTwo.asYeller.add(new Yeller[T](t) {})


    InnerCaseClassOne("inner argument").add(212).add(InnerCaseClassTwo(121).asYeller)

    /** Note how the value of T varies, but it's the same function T => String */
    case class StringYeller(t: String) extends Yeller[String](t)
    case class IntYeller(t: Int) extends Yeller[Int](t)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t)
    case class InnerCaseClassOneYeller(t: InnerCaseClassOne) extends Yeller[InnerCaseClassOne](t)
    case class InnerCaseClassTwoYeller(t: InnerCaseClassTwo) extends Yeller[InnerCaseClassTwo](t)

    val someTypeYeller = InnerCaseClassOneYeller(InnerCaseClassOne("inner argument")).add(IntYeller(212)).add(InnerCaseClassTwoYeller(InnerCaseClassTwo(121)))

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")

    assertEquals(InnerCaseClassOne("inner argument").add(212).add(InnerCaseClassTwo(121)).yell(), "INNER ARGUMENT 212 121!")
  }

  test("Hot Dog!  That's something!  Let's clean it up.") {
    /** We want to be able to make a Yeller, add another Yeller to one we have, or add a non-Yeller to the one we have */
    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      /** this is actually letting us go from Yeller[V] => Yeller[String] */
      def add[V](yeller: Yeller[V]) = {
        val asString = stringer(this.value) + " " + yeller.stringer(yeller.value)
        new Yeller[String](asString) {}
      }

      /** this is actually letting us go from V => Yeller[String] */
      def add[V](v: V)(using stringerFunc: V => String) = {
        val vYeller = new Yeller[V](v) {}
        val asString = stringer(this.value) + " " + vYeller.stringer(vYeller.value)
        new Yeller[String](asString) {}
      }

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** We define ways to get a T to become a String */
    given stringStringer: Function1[String, String] = identity
    given intStringer: (Int => String) = i => i.toString
    given booleanString: (Boolean => String) = b => b.toString
    given innerCaseClassOneStringer: (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    given innerCaseClassTwoStringer: (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString

    /** We put them in objects here to prevent def name collisions */
    object InnerCaseClassOne {
      extension (innerClassOne: InnerCaseClassOne)
        def add[T](t: T)(using stringer: T => String): Yeller[String] = {
          val yeller = new Yeller(innerClassOne.argument) {}
          yeller.add(new Yeller[T](t) {})
        }
    }

    object InnerCaseClassTwo  {
      extension (innerClassTwo: InnerCaseClassTwo)
        def add[T](t: T)(using stringer: T => String): Yeller[String] = {
          val yeller = new Yeller(innerClassTwo.argument.toString) {}
          yeller.add(new Yeller[T](t) {})
        }
    }

    case class StringYeller(t: String) extends Yeller[String](t)
    case class IntYeller(t: Int) extends Yeller[Int](t)
    case class BooleanYeller(t: Boolean) extends Yeller[Boolean](t)

    assertEquals(StringYeller("hey").yell(), "HEY!")
    assertEquals(IntYeller(212).yell(), "212!")
    assertEquals(BooleanYeller(true).yell(), "TRUE!")

    assertEquals(InnerCaseClassOne("inner argument").add(212).add(InnerCaseClassTwo(121)).yell(), "INNER ARGUMENT 212 121!")
  }

  test("Seems like we just want a way to make a Yeller, and then add types to it.") {
    trait Yeller[T](t: T)(using stringerFunc: T => String):
      val value: T = t
      val stringer: T => String = stringerFunc

      def yell() = stringer(t).toUpperCase + "!"

      /** just add a thing to it */
      def add[V](v: V)(using stringerFunc: V => String) = {
        val vYeller = new Yeller[V](v) {}
        val asString = stringer(this.value) + " " + vYeller.stringer(vYeller.value)
        new Yeller[String](asString) {}
      }

    object Yeller {
      def apply[T](t: T)(using stringerFunc: T => String) = {
        val yeller = new Yeller[T](t) {}
        val asString = stringerFunc(t)
        new Yeller[String](asString) {}
      }
    }

    case class InnerCaseClassOne(argument: String)
    case class InnerCaseClassTwo(argument: Int)
    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** We define ways to get strings */
    given stringStringer: Function1[String, String] = identity
    given intStringer: (Int => String) = i => i.toString
    given booleanString: (Boolean => String) = b => b.toString
    given innerCaseClassOneStringer: (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    given innerCaseClassTwoStringer: (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString

    assertEquals(Yeller("hey").yell(), "HEY!")
    assertEquals(Yeller(212).yell(), "212!")
    assertEquals(Yeller(true).yell(), "TRUE!")

    val yeller = Yeller(InnerCaseClassOne("inner argument")).add(212).add(InnerCaseClassTwo(121))
    assertEquals(yeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("Seems like we can clean this up some more") {
    trait Yeller[T](t: T):
      val value: T = t

      def yell()(using stringerFunc: T => String) = stringerFunc(t).toUpperCase + "!"

      /** just add a thing to it, provided we know the stringer for T and V */
      def add[V](v: V)(using stringerFuncV: V => String, stringerFuncT: T => String) = {
        val vYeller = new Yeller[V](v) {}
        val asString = stringerFuncT(this.value) + " " + stringerFuncV(vYeller.value)
        new Yeller[String](asString) {}
      }

    object Yeller {

      /** some basic types */
      given (String => String) = s => identity(s)
      given (Int => String) = i => i.toString
      given (Boolean => String) = b => b.toString

      def apply[T](t: T)(using stringerFunc: T => String) = {
        val yeller = new Yeller[T](t) {}
        val asString = stringerFunc(t)
        new Yeller[String](asString) {}
      }
    }

    case class InnerCaseClassOne(argument: String)
    object InnerCaseClassOne {
      given (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    }

    case class InnerCaseClassTwo(argument: Int)
    object InnerCaseClassTwo {
      given (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString
    }

    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    /** really like the way this reads... */
    import Yeller.given
    assertEquals(Yeller("hey").yell(), "HEY!")
    assertEquals(Yeller(212).yell(), "212!")
    assertEquals(Yeller(true).yell(), "TRUE!")

    val yeller = Yeller(InnerCaseClassOne("inner argument")).add(212).add(InnerCaseClassTwo(121))
    assertEquals(yeller.yell(), "INNER ARGUMENT 212 121!")
  }


  test("Let's try some tuples!") {
    trait Yeller[T](t: T):
      val value: T = t

      def yell()(using stringerFunc: T => String) = stringerFunc(t).toUpperCase + "!"

      /** just add a thing to it, provided we know the stringer for T and V */
      def add[V](v: V)(using stringerFuncV: V => String, stringerFuncT: T => String) = {
        val vYeller = new Yeller[V](v) {}

        val stringT = stringerFuncT(this.value)
        val firstPart = if (stringT.nonEmpty) stringT + " " else ""
        val secondPart = stringerFuncV(vYeller.value)
        val asString = firstPart + secondPart
        new Yeller[String](asString) {}
      }

    object Yeller {

      /** some basic types */
      given (String => String) = s => identity(s)
      given (Int => String) = i => i.toString
      given (Boolean => String) = b => b.toString

      def apply[T](t: T)(using stringerFunc: T => String) = {
        val yeller = new Yeller[T](t) {}
        val asString = stringerFunc(t)
        new Yeller[String](asString) {}
      }
    }

    case class InnerCaseClassOne(argument: String)
    object InnerCaseClassOne {
      given (InnerCaseClassOne => String) = innerCaseClassOne => innerCaseClassOne.argument
    }

    case class InnerCaseClassTwo(argument: Int)
    object InnerCaseClassTwo {
      given (InnerCaseClassTwo => String) = innerCaseClassTwo => innerCaseClassTwo.argument.toString
    }

    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)

    val someType = SomeType(InnerCaseClassOne("inner argument"), 212, InnerCaseClassTwo(121))

    val tuple = Tuple.fromProductTyped(someType)

    import Yeller.given
    // https://rockthejvm.com/articles/scala-3-type-level-programming#user-content-fnref-patternmatching
    inline def yellTuple(tuple: Tuple, yeller: Yeller[String]): Yeller[String] = {
      inline tuple match {
        case EmptyTuple =>
          yeller
        case tup: (h *: t) =>
          val myGiven = compiletime.summonInline[h => String]
          yellTuple(tup.tail, yeller.add(tup.head)(using myGiven))
      }
    }

    val someTypeYeller = yellTuple(tuple, Yeller(""))
    assertEquals(someTypeYeller.yell(), "INNER ARGUMENT 212 121!")

    /** really like the way this reads... */
    import Yeller.given
    assertEquals(Yeller("hey").yell(), "HEY!")
    assertEquals(Yeller(212).yell(), "212!")
    assertEquals(Yeller(true).yell(), "TRUE!")

    val yeller = Yeller(InnerCaseClassOne("inner argument")).add(212).add(InnerCaseClassTwo(121))
    assertEquals(yeller.yell(), "INNER ARGUMENT 212 121!")
  }

  test("Wait, shouldn't these things live with their respective classes?") {
    object Yeller:
      def yell[T](t: T, stringer: T => String) = stringer(t).toUpperCase + "!"

    case class InnerCaseClassOne(argument: String)
    object InnerCaseClassOne {
      val stringer: InnerCaseClassOne => String = innerCaseClassOne => innerCaseClassOne.argument
    }

    case class StringWrapper(argument: String)
    object StringWrapper {
      val stringer: String => String = identity
    }

    case class IntWrapper(argument: Int)
    object IntWrapper {
      val stringer: Int => String = i => i.toString
    }

    case class BooleanWrapper(argument: Boolean)
    object BooleanWrapper {
      val stringer: Boolean => String = b => b.toString
    }

    case class InnerCaseClassTwo(argument: Int)
    object InnerCaseClassTwo {
      val stringer: InnerCaseClassTwo => String = innerCaseClassTwo => innerCaseClassTwo.argument.toString
    }

    case class SomeType(argumentOne: InnerCaseClassOne, argumentTwo: Int, argumentThree: InnerCaseClassTwo)
    object SomeType {
      val stringer: SomeType => String = someType => InnerCaseClassOne.stringer(someType.argumentOne) + " " + someType.argumentTwo + " " + InnerCaseClassTwo.stringer(someType.argumentThree)
    }

    assertEquals(Yeller.yell("hey", StringWrapper.stringer), "HEY!")
    assertEquals(Yeller.yell(212, IntWrapper.stringer), "212!")
    assertEquals(Yeller.yell(true, BooleanWrapper.stringer), "TRUE!")
    assertEquals(Yeller.yell(SomeType(InnerCaseClassOne("inner argument"), 212, InnerCaseClassTwo(121)), SomeType.stringer), "INNER ARGUMENT 212 121!")
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
