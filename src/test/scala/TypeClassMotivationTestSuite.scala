import scala.:+
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

    /** Consider something that takes a string and yells it */
    object Yeller_String:
      def yell(string: String): String = string.toUpperCase + "!"

    assertEquals(Yeller_String.yell("hey"), "HEY!")

    /** What about if we need to yell about Ints? */
    object Yeller_Int:
      def yell(i: Int): String = i.toString + "!"

    assertEquals(Yeller_Int.yell(212), "212!")
  }

  test("Hey, we are going to need to yell a whole bunch of types!") {
    /** At this point, we can use *adhoc polymorphism* via function overloading */
    object Yeller :
      def yell(string: String): String = string.toUpperCase + "!"
      def yell(i: Int): String = i.toString + "!"
      def yell(boolean: Boolean): String = boolean.toString.toUpperCase + "!"

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
  }

  test("Let's clean this up") {
    object Yeller:
      def yell(string: String): String = performYell(string)
      def yell(i: Int): String = performYell(i.toString)
      def yell(boolean: Boolean): String = performYell(boolean.toString)

      private def performYell(s: String) = s.toUpperCase + "!"

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
    trait YellerAdapter:
      def asString: String

    object Yeller:
      def yell(yellee: YellerAdapter): String = yellee.asString.toUpperCase + "!"

    case class StringYellerAdapter(string: String) extends YellerAdapter :
      def asString: String = string

    case class IntYellerAdapter(int: Int) extends YellerAdapter:
      override def asString: String = int.toString

    case class BooleanYellerAdapter(boolean: Boolean) extends YellerAdapter:
      override def asString: String = boolean.toString

    /** What about some type we just ended up with? */
    case class SomeType(argument: String)

    case class SomeTypeYellerAdapter(someType: SomeType) extends YellerAdapter:
      override def asString: String = someType.argument

    assertEquals(Yeller.yell(StringYellerAdapter("hey")), "HEY!")
    assertEquals(Yeller.yell(IntYellerAdapter(212)), "212!")
    assertEquals(Yeller.yell(BooleanYellerAdapter(true)), "TRUE!")
    assertEquals(Yeller.yell(SomeTypeYellerAdapter(SomeType("argument"))), "ARGUMENT!")
  }

  /**
   * What we are doing with YellerAdapter is old-fashioned polymorphism.  For the YellerAdapter, we have many different
   * implementations.
   *
   * We can use a type classes to achieve the same result.  Type Classes are "things" that allow polymorphism without
   * have to extend the "thing".  In our case, it will be the `YellerAdapter`
   *
   * We can use Scala's contextual abstractions--specifically, given and using statements--to achieve this.
   *
   * A `Given Instance` defines "canonical" values of certain types and are used for providing values for
   * `Using Clauses`.
   *
   * `Using Clauses`, sometimes called context parameters, are bits of code that can be filled given their context. That
   * is, the compiler can figure out what the bits of code need to be in order for things to work.
   */
  test("What's it look like with a Type Class?") {
    /** We make the type (string, int, whatever) for the adapter part of the signature */
    trait YellerAdapter[T]:
      def asString(t: T): String

    /** If it's on the adapter, it needs to be on the Yeller, too.  Notice how the explicit argument t is the "primary"
     * argument and the contextual argument is what was the only argument--the YellerAdapter.
     *
     * Essentially, we are saying for a T, there's something in the contextual soup that can tell you how to make a
     * string.  If there's no ambiguity, the compiler uses it to make it work.
     * */
    object Yeller:
      def yell[T](t: T)(using yellerAdapter: YellerAdapter[T]): String = yellerAdapter.asString(t).toUpperCase + "!"


    case class SomeType(argument: String)

    given stringYellerAdapter: YellerAdapter[String] = new YellerAdapter[String]:
      override def asString(t: String): String = t

    given intYellerAdapter: YellerAdapter[Int] = new YellerAdapter[Int]:
      override def asString(t: Int): String = t.toString

    given booleanYellerAdapter: YellerAdapter[Boolean] = new YellerAdapter[Boolean]:
      override def asString(t: Boolean): String = t.toString

    given someTypeYellerAdapter: YellerAdapter[SomeType] = new YellerAdapter[SomeType]:
      override def asString(t: SomeType): String = t.argument

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
    assertEquals(Yeller.yell(SomeType("argument")), "ARGUMENT!")
  }

  test("You don't even have to name the givens...") {
    trait YellerAdapter[T]:
      def asString(t: T): String

    object Yeller:
      def yell[T](t: T)(using yellerAdapter: YellerAdapter[T]): String = yellerAdapter.asString(t).toUpperCase + "!"

    case class SomeType(argument: String)

    /**
     * This makes sense.  Really, what we are doing is saying there's a way to make a given T into a string for our purposes.
     * And, that holds for *any value* of type T.  We don't need to have a label for it if we don't want it.
     * */
    given YellerAdapter[String] = new YellerAdapter[String]:
      override def asString(t: String): String = t

    given YellerAdapter[Int] = new YellerAdapter[Int]:
      override def asString(t: Int): String = t.toString

    given YellerAdapter[Boolean] = new YellerAdapter[Boolean]:
      override def asString(t: Boolean): String = t.toString

    given YellerAdapter[SomeType] = new YellerAdapter[SomeType]:
      override def asString(t: SomeType): String = t.argument


    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
    assertEquals(Yeller.yell(SomeType("argument")), "ARGUMENT!")
  }

  test("You can even just give the canonical implementation for asString for a T...") {
    trait YellerAdapter[T]:
      def asString(t: T): String

    object Yeller:
      def yell[T](t: T)(using yellerAdapter: YellerAdapter[T]): String = yellerAdapter.asString(t).toUpperCase + "!"

    case class SomeType(argument: String)

    /**
     * The compiler can figure out that we are giving an implementation for asString(t: T) and use that for to create
     * (synthesize) the class for use!
     * */
    given YellerAdapter[String] = (t: String) => t
    given YellerAdapter[Int] = (t: Int) => t.toString
    given YellerAdapter[Boolean] = (t: Boolean) => t.toString
    given YellerAdapter[SomeType] = (t: SomeType) => t.argument

    assertEquals(Yeller.yell("hey"), "HEY!")
    assertEquals(Yeller.yell(212), "212!")
    assertEquals(Yeller.yell(true), "TRUE!")
    assertEquals(Yeller.yell(SomeType("argument")), "ARGUMENT!")
  }

  /**
   * So, that's nice and all, but it isn't really more compelling than just making adapter, innit?
   *
   * Where the Type Classes really start to shine is when we let it figure out how to make an instance.
   *
   * Before we do that, let's work on what it means to compose YellAdapters
   *
   What should a given case class's YellerAdapter look like?
   *
   * For our purposes, let's define it such for any case class, it's
   * 1) the first argument as string then concatenated with a space and the second argument, ..., concatenated with a
   *    space and the Nth argument.
   * 2) the result of (1) uppercased
   *
   * What if an argument itself has arguments?
   *
   * Good question.  We'll just recursively apply the rules.
   *
   * We'd want something like this:
   *
   * case class SomeType(argument: String)
   * case class AnotherType(someType: SomeType, int: Int)
   *
   * val anotherType = AnotherType(SomeType("argument"), 212)
   *
   * YellerAdapter[AnotherType] =  compose(YellerAdapter[SomeType], YellerAdapter[Int])
   *
   * We will defer on the implementation of 'compose' for just a moment.  But we want to say that
   * in English, if we have a way to describe all the individual types, we have a way to describe the composite type
   *
   * One way describe this (and definitely not the only) way is
   * that for a type T, which has an U and V we want to have
   * YellerAdapter[T] = compose(YellerAdapter[U], YellerAdapter[V])
   *
   * or
   *
   * YellerAdapter[T] = compose[U, V](YellerAdapter[U], YellerAdapter[V])
   *
   */
  test("Let's see what compose looks like") {
    trait YellerAdapter[T]:
      def asString(t: T): String

      /**
       * I'm going to use an existential type because I have no way of saying right now what a U and a V make when they
       * are in a YellerAdpater.
       */

      def compose[U, V](uYellerAdapter: YellerAdapter[U], vYellerAdapter: YellerAdapter[V]): YellerAdapter[_] =
        ???
  }

  test("Let's flesh this out some more") {
    trait YellerAdapter[T]:
      def asString(t: T): String

    /**
     * What does it look like if we try to implement the steps?
     */
    def compose[U, V](uYellerAdapter: YellerAdapter[U], vYellerAdapter: YellerAdapter[V]): YellerAdapter[_] =
      val string = uYellerAdapter.asString(???) + " " + vYellerAdapter.asString(???)
      val upperCased = string.toUpperCase
      ???
  }

  test("how do we get the values for the U and V?") {
    /** Our trait needs to change some */
    trait YellerAdapter[T]:
      val t: T
      def asString(t: T): String

    /**
     * What does it look like if we try to implement the steps?
     */
    def compose[U, V](uYellerAdapter: YellerAdapter[U], vYellerAdapter: YellerAdapter[V]): YellerAdapter[_] =
      val string = uYellerAdapter.asString(uYellerAdapter.t) + " " + vYellerAdapter.asString(vYellerAdapter.t)
      val upperCased = string.toUpperCase
      new YellerAdapter[String]():
        val t: String = upperCased
        def asString(t: String): String = t

  }

  test("Well that looks stupid.") {
    /** Our trait needs to change some */
    trait YellerAdapter[T]:
      val t: T
      def asString(): String

    /**
     * What does it look like if we try to implement the steps?
     */
    def compose[U, V](uYellerAdapter: YellerAdapter[U], vYellerAdapter: YellerAdapter[V]): YellerAdapter[_] =
      val string = uYellerAdapter.asString() + " " + vYellerAdapter.asString()
      val upperCased = string.toUpperCase
      new YellerAdapter[String]():
        val t: String = upperCased
        def asString(): String = t
  }

  test("Let's try it for real") {
    trait YellerAdapter[T]:
      val t: T
      def asString(): String

    def compose[U, V](uYellerAdapter: YellerAdapter[U], vYellerAdapter: YellerAdapter[V]): YellerAdapter[_] =
      val string = uYellerAdapter.asString() + " " + vYellerAdapter.asString()
      val upperCased = string.toUpperCase
      new YellerAdapter[String]():
        val t: String = upperCased
        def asString(): String = t

    case class StringYellerAdapter(t: String) extends YellerAdapter[String]:
      override def asString(): String = t

    case class BooleanYellerAdapter(t: Boolean) extends YellerAdapter[Boolean]:
      override def asString(): String = t.toString

    val composed = compose(StringYellerAdapter("hey"), BooleanYellerAdapter(true))

    assertEquals(composed.asString(), "HEY TRUE")
  }

  test("Wait. That's dumb looking, too.  Let's make compose a member of the trait.") {
    trait YellerAdapter[T]:
      val t: T
      def asString(): String
      def compose[U](u: YellerAdapter[U]) = {
        val string = this.asString() + " " + u.asString()
        val upperCased = string.toUpperCase

        new YellerAdapter[String]():
          val t: String = upperCased
          def asString(): String = t
      }

    case class StringYellerAdapter(t: String) extends YellerAdapter[String]:
      override def asString(): String = t

    case class BooleanYellerAdapter(t: Boolean) extends YellerAdapter[Boolean]:
      override def asString(): String = t.toString


    val composed = StringYellerAdapter("hey").compose(BooleanYellerAdapter(true))

    assertEquals(composed.asString(), "HEY TRUE")
  }

  test("Still not pleased with this.  The t is only used to make a new adapter. Let's change it.") {
    trait YellerAdapter[T]:
      def asString(): String

      def compose[U](u: YellerAdapter[U]) =
        val string = this.asString() + " " + u.asString()
        val upperCased = string.toUpperCase
        YellerAdapter(upperCased)


    object YellerAdapter:
      def apply[T](value: T)(using f: T => String): YellerAdapter[String] =
        new YellerAdapter[String]():
          def asString(): String = f(value)


    case class StringYellerAdapter(t: String) extends YellerAdapter[String]:
      override def asString(): String = t

    case class BooleanYellerAdapter(t: Boolean) extends YellerAdapter[Boolean]:
      override def asString(): String = t.toString

    val composed = StringYellerAdapter("hey").compose(BooleanYellerAdapter(true))

    assertEquals(composed.asString(), "HEY TRUE")
  }

  test("Can we possibly make this nicer?") {
    trait YellerAdapter[T]:
      def asString(): String

      def compose[U](u: YellerAdapter[U]) =
        val string = this.asString() + " " + u.asString()
        val upperCased = string.toUpperCase
        YellerAdapter(upperCased)

    object YellerAdapter:
      def apply[T](value: T)(using f: T => String): YellerAdapter[String] =
        new YellerAdapter[String]():
          def asString(): String = f(value)

    object Yeller:
      def yell[T](t: T)(using f: T => String): String =
        YellerAdapter(t).asString() + "!"

      def yell[T, V](t: T, v: V)(using f: T => String, g: V => String): String =
        YellerAdapter(t).compose(YellerAdapter(v)).asString() + "!"

    given (Boolean => String) = (t: Boolean) => t.toString

    val composed = Yeller.yell("hey", true)

    assertEquals(composed, "HEY TRUE!")
  }

  test("We can't keep going with T, V, etc.  We need to take something that is itself composed") {
    trait YellerAdapter[T]:
      def asString(): String

      def compose[U](u: YellerAdapter[U]) =
        val string = this.asString() + " " + u.asString()
        val upperCased = string.toUpperCase
        YellerAdapter(upperCased)

    object YellerAdapter:
      def apply[T](value: T)(using f: T => String): YellerAdapter[String] =
        new YellerAdapter[String]():
          def asString(): String = f(value)

    object Yeller:
      def yell[T](t: T)(using f: T => String): String =
        YellerAdapter(t).asString() + "!"

      inline def yell(tuple: Tuple): String =
        yellTuple(tuple, YellerAdapter("")).asString() + "!"

      inline def yellTuple(tuple: Tuple, yellerAdapter: YellerAdapter[String]): YellerAdapter[String] = {
        inline tuple match {
          case EmptyTuple =>
            yellerAdapter
          case tup: (h *: t) =>
            val myGiven = compiletime.summonInline[h => String]
            val hAdapter = YellerAdapter(tup.head)(using myGiven)
            yellTuple(tup.tail, yellerAdapter.compose(hAdapter))
        }
      }


    given (Boolean => String) = (t: Boolean) => t.toString

    val yelled = Yeller.yell(("hey", true))

    assertEquals(yelled, " HEY TRUE!")  // We don't want this!
  }

  test("Let's handle the initial case.") {
    trait YellerAdapter[T]:
      def asString(): String

      def compose[U](u: YellerAdapter[U]): YellerAdapter[String] =
        val string = this.asString() + " " + u.asString()
        val upperCased = string.toUpperCase
        YellerAdapter(upperCased)

    object YellerAdapter:
      def apply[T](value: T)(using f: T => String): YellerAdapter[String] =
        new YellerAdapter[String]():
          def asString(): String = f(value)

    object Yeller:
      def yell[T](t: T)(using f: T => String): String =
        YellerAdapter(t).asString() + "!"

      inline def yell(tuple: Tuple): String =
        yellTuple(tuple, None).map(_.asString() + "!").getOrElse("")

      inline def yellTuple(tuple: Tuple, yellerAdapterOption: Option[YellerAdapter[String]] = None): Option[YellerAdapter[String]] = {
        inline tuple match {
          case EmptyTuple =>
            yellerAdapterOption
          case tup: (h *: t) =>
            val myGiven = compiletime.summonInline[h => String]
            val hAdapter = YellerAdapter(tup.head)(using myGiven)
            yellerAdapterOption match
              case Some(yellerAdapter) =>
                yellTuple(tup.tail, Some(yellerAdapter.compose(hAdapter)))
              case None =>
                yellTuple(tup.tail, Some(hAdapter))
        }
      }


    given (Boolean => String) = (t: Boolean) => t.toString

    val yelled = Yeller.yell(("hey", true))

    assertEquals(yelled, "HEY TRUE!")
  }

  test("We don't want tuples all the time.  We want to go from a product type, to a tuple, to the string!  Let's try that.") {
    inline def productToTupleSafe[P <: Product](p: P): Tuple =
      inline summonInline[Mirror.ProductOf[P]] match
        case m: Mirror.ProductOf[P] =>
          val elements = Tuple.fromProduct(p)
          elements

    case class SomeType(argument: String)
    case class AnotherType(someType: SomeType, int: Int)

    val tuple = productToTupleSafe(AnotherType(SomeType("argument"), 212))

    assertEquals(tuple, (SomeType("argument"), 212))
  }

}
