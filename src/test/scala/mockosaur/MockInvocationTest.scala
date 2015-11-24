package mockosaur

import mockosaur.exceptions.{MockosaurNoOngoingRecordException, MockosaurRecordAlreadyOngoingException, MockosaurUnexpectedFunctionCallException}
import mockosaur.impl.MockState

class MockInvocationTest extends MockosaurTest {


  "A Mockosaur mock should" - {

    class TheTestClass {
      def theFunc(): Unit = ???
      def theNoArgStringFunc(): String = ???
      def theNoParensStringFunc: String = ???
    }

    trait Scope {
      MockState.reset()
    }

    "throw an exception if an unexpected function is called - no args param unit return type" in new Scope {
      val theMock = mock[TheTestClass]

      val thrown = intercept[MockosaurUnexpectedFunctionCallException] {
        theMock.theFunc()
      }

      thrown.called.function.getName shouldBe "theFunc"
      thrown.called.args shouldBe Seq.empty
    }

    "return unit if expected to do so - no args param unit return type" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theFunc().returns(())

      theMock.theFunc() shouldBe ()
    }

    "return value if expected to do so - no args param" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("Another String")

      theMock.theNoArgStringFunc() shouldBe "Another String"
    }

    "return value if expected to do so - no parens" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoParensStringFunc.returns("Another String")

      theMock.theNoParensStringFunc shouldBe "Another String"
    }

    "throw an exception if a return value is called when no mock record is ongoing" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        "a string".length.returns(9)
      }
    }

    "throw an exception if a return value is called when mock record is already complete" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        calling(theMock).theFunc().returns(1).returns(2)
      }
    }

    "throw an exception if multiple mocks are recording" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1)

      intercept[MockosaurRecordAlreadyOngoingException] {
        calling(theMock2)
      }
    }

    "throw an exception if a mock is recording multiple times" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock)

      intercept[MockosaurRecordAlreadyOngoingException] {
        calling(theMock)
      }
    }

    "throw an exception if multiple mocks are recording at once" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1)

      intercept[MockosaurRecordAlreadyOngoingException] {
        calling(theMock2)
      }
    }

    "throw an exception if a mocked call is not used" in pending
    "multiple calls" in pending
    "no parens functions" in pending
    "implicits" in pending

  }
}
