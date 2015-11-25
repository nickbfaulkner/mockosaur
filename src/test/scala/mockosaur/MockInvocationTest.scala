package mockosaur

import java.util.concurrent.atomic.AtomicInteger
import mockosaur.exceptions._
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

    "return value if expected to do so - with params" in pending

    "return value if expected to do so - no parens" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoParensStringFunc.returns("Another String")

      theMock.theNoParensStringFunc shouldBe "Another String"
    }

    "verify all calls were made to a mock" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("Another String")

      theMock.theNoArgStringFunc()

      verifyAllCallsWereMadeTo(theMock)
    }

    "verify all calls were made to multiple mocks" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1).theNoArgStringFunc().returns("Another String")
      calling(theMock2).theNoArgStringFunc().returns("Another String")

      theMock1.theNoArgStringFunc()
      theMock2.theNoArgStringFunc()

      verifyAllCallsWereMadeTo(theMock1, theMock2)
    }

    "throw an exception if a mocked call is not used" in new Scope() {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("Another String")

      intercept[MockosaurUnmetExpectationException] {
        verifyAllCallsWereMadeTo(theMock)
      }
    }

    "throw an exception if a partially mocked call is not used" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc() // no returns specified

      intercept[MockosaurIncompleteMockException] {
        verifyAllCallsWereMadeTo(theMock)
      }
    }

    "throw an exception if no mocks are passed when verifying all calls were made" in new Scope {
      intercept[IllegalArgumentException] {
        verifyAllCallsWereMadeTo()
      }
    }

    "multiple calls" in pending
    "implicits" in pending
    "returning functions" in pending

    "throw an exception if returns is called when no mock record is ongoing" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        "a string".length.returns(9)
      }
    }

    "throw an exception if returns is not called" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theFunc()

      intercept[MockosaurReturnsRequiredException] {
        theMock.theFunc()
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

    "throw an exception if multiple mocks are recording at once on the same thread" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1)

      intercept[MockosaurRecordAlreadyOngoingException] {
        calling(theMock2)
      }
    }

    "clear out expectations when resetting state (internal util)" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1).theNoArgStringFunc().returns("Another String")
      calling(theMock2).theNoArgStringFunc().returns("Another String")

      MockState.reset()

      verifyAllCallsWereMadeTo(theMock1, theMock2)
    }

    "work as expected if multiple mocks are recording at once on different threads" in new Scope {
      val threadCount = 10
      val complete = new AtomicInteger(0)

      val threads = (1 to threadCount) map { _ =>
        new Thread(new Runnable() {
          override def run(): Unit = {
            val theMock = mock[TheTestClass]
            calling(theMock).theFunc().returns(())
            theMock.theFunc() shouldBe ()
            complete.incrementAndGet()
          }
        })
      }

      threads.foreach(_.start())
      Thread.sleep(500)
      threads.foreach(_.join())

      complete.get() shouldBe threadCount
    }
  }
}
