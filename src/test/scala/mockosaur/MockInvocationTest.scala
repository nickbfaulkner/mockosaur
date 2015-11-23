package mockosaur

import mockosaur.exceptions.MockosaurUnexpectedFunctionCallException

class MockInvocationTest extends MockosaurTest {

  class TheTestClass {
    def theFunc(): Unit = ???
  }

  "A Mockosaur mock should" - {

    "throw an exception if an unexpected function is called - no args param unit return type" in {

      val theMock = mock[TheTestClass]

      val thrown = intercept[MockosaurUnexpectedFunctionCallException] {
        theMock.theFunc()
      }

      thrown.called.function.getName shouldBe "theFunc"
      thrown.called.args shouldBe Seq.empty
    }

    "return unit if expected to do so - no args param unit return type" in {

      val theMock = mock[TheTestClass]

      calling(theMock).theFunc().returning(())

      theMock.theFunc() shouldBe ()
    }

    "throw an exception if a return value is called when no mock record is ongoing" in pending
    "throw an exception if a return value is called when mock record is already complete" in pending
    "throw an exception if a mocked call is incomplete - calling only" in pending
    "throw an exception if a mocked call is incomplete - calling and function call specified" in pending
    "throw an exception if multiple mocks are recording" in pending
    "throw an exception if a mock is recording multiple times" in pending


    "multiple calls" in pending
    "matches implicit" in pending

  }
}
