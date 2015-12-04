package mockosaur

import mockosaur.MockConstructionTest._

object MockConstructionTest {
  class TheTestValueClass(val s: String) extends AnyVal
  class TheTestClassWithDefaultConstructor {}
  trait TheTestTrait {}
  trait TheTestTraitWithTypeParameters[T <: Seq[Set[String]]] {}
  abstract class TheAbstractClass { def abc: Int }
  final class TheTestFinalClass {}
  class TheTestClassWithNonDefaultConstructor(a: String, b: Object, c: Int, d: Any) {}
  private class TheTestPrivateClass {}
  class TheTestClassWithPrivateConstructor private (a: String, b: Object, c: Int, d: Any) {}
  object TheTestObject
}

class MockConstructionTest extends MockosaurTest {

  "Mockosaur should" - {

    "Mock traits" in {

      val theMock = mock[TheTestTrait]

      classOf[TheTestTrait].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock traits with type parameters" in {

      val theMock = mock[TheTestTraitWithTypeParameters[Vector[Set[String]]]]

      classOf[TheTestTraitWithTypeParameters[Vector[Set[String]]]].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock java interfaces" in {

      val theMock = mock[java.io.Serializable]

      classOf[java.io.Serializable].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock scala java interface wrappers" in {

      val theMock = mock[Serializable]

      classOf[Serializable].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with default constructors" in {

      val theMock = mock[TheTestClassWithDefaultConstructor]

      classOf[TheTestClassWithDefaultConstructor].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock final classes" in { pending // can't mock final classes

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

    "Mock private classes" in {

      val theMock = mock[TheTestPrivateClass]

      classOf[TheTestPrivateClass].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with private constructors" in {

      val theMock = mock[TheTestClassWithPrivateConstructor]

      classOf[TheTestClassWithPrivateConstructor].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock objects" in {

      val theMock = mock(TheTestObject)

      TheTestObject.getClass.isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock value classes" in { pending // can't mock final classes

      val theMock = mock[TheTestValueClass]

      classOf[TheTestValueClass].isAssignableFrom(theMock.getClass) shouldBe true

    }
  }
}
