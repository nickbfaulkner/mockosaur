package mockosaur

class MockConstructionTest extends MockosaurTest {

  "Mockosaur should" - {

    "Mock classes with default constructors" in {

      class TheTestClass {}

      val theMock = mock[TheTestClass]

      classOf[TheTestClass].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock final classes" in { pending // CGLIB can't mock final classes

      final class TheTestClass {}

      val theMock = mock[TheTestClass]

      classOf[TheTestClass].isAssignableFrom(theMock.getClass) shouldBe true

    }

    "Mock classes with non-default constructors" in {

      class TheTestClass(a: String, b: Object, c: Int, d: Any) {}

      val theMock = mock[TheTestClass]

      classOf[TheTestClass].isAssignableFrom(theMock.getClass) shouldBe true

    }
  }
}
