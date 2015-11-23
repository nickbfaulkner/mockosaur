package mockosaur

import mockosaur.exceptions.{MockosaurNoOngoingRecordException, MockosaurUnexpectedFunctionCallException}
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

    "throw an exception if a return value is called when mock record is already complete" in pending
    "throw an exception if a mocked call is incomplete - calling only" in pending
    "throw an exception if a mocked call is incomplete - calling and function call specified" in pending
    "throw an exception if multiple mocks are recording" in pending
    "throw an exception if a mock is recording multiple times" in pending
    "multiple calls" in pending
    "matches implicit" in pending

  }
}
