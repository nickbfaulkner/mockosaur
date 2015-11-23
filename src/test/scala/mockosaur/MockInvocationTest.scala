package mockosaur

import mockosaur.exceptions.{MockosaurRecordAlreadyOngoingException, MockosaurNoOngoingRecordException, MockosaurRecordAlreadyCompleteException, MockosaurUnexpectedFunctionCallException}
import mockosaur.impl.MockState

class MockInvocationTest extends MockosaurTest {


  "A Mockosaur mock should" - {

    class TheTestClass {
      def theFunc(): Unit = ???
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

      calling(theMock).theFunc().returning(())

      theMock.theFunc() shouldBe ()
    }

    "throw an exception if a return value is called when no mock record is ongoing" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        "a string".length.returning(9)
      }
    }

    "throw an exception if a return value is called when mock record is already complete" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurRecordAlreadyCompleteException] {
        calling(theMock).theFunc().returning(1).returning(2)
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

    "throw an exception if a mocked call is not used" in pending
    "throw an exception if a call is unexpected" in pending

    "return values" in pending
    "multiple calls" in pending
    "implicits" in pending

  }
}
