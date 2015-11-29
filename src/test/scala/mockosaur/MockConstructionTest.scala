package mockosaur

import mockosaur.MockConstructionTest._

object MockConstructionTest {
  class TheTestValueClass(val s: String) extends AnyVal
  class TheTestClassWithDefaultConstructor {}
  trait TheTestTrait {}
  abstract class TheAbstractClass { def abc: Int }
  final class TheTestFinalClass {}
  class TheTestClassWithNonDefaultConstructor(a: String, b: Object, c: Int, d: Any) {}
  class TheTestClassWithPrivateConstructor private (a: String, b: Object, c: Int, d: Any) {}
  object TheTestObject
}

class MockConstructionTest extends MockosaurTest {

  "Mockosaur should" - {

    "Mock traits" in {

      val theMock = mock[TheTestTrait]

      classOf[TheTestTrait].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with default constructors" in {

      val theMock = mock[TheTestClassWithDefaultConstructor]

      classOf[TheTestClassWithDefaultConstructor].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock final classes" in { pending // CGLIB can't mock final classes

      val theMock = mock[TheTestFinalClass]

      classOf[TheTestFinalClass].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock abstract classes" in {

      val theMock = mock[TheAbstractClass]

      classOf[TheAbstractClass].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with non-default constructors" in {

      val theMock = mock[TheTestClassWithNonDefaultConstructor]

      classOf[TheTestClassWithNonDefaultConstructor].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with private constructors" in { pending // CGLIB can't mock classes with private constructors

      val theMock = mock[TheTestClassWithPrivateConstructor]

      classOf[TheTestClassWithPrivateConstructor].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock objects" in {

      val theMock = mock(TheTestObject)

      TheTestObject.getClass.isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock value classes" in { pending // CGLIB can't mock final classes

      val theMock = mock[TheTestValueClass]

      classOf[TheTestValueClass].isAssignableFrom(theMock.getClass) shouldBe true

    }
  }
}
