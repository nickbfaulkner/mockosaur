package mockosaur

import java.io.IOException
import java.util.concurrent.atomic.AtomicInteger

import mockosaur.MockInvocationTest.AnInt
import mockosaur.impl.MockState

object MockInvocationTest {
  final case class AnInt(theInt: Int) extends AnyVal
}

class MockInvocationTest extends MockosaurTest {

  "A Mockosaur mock should" - {

    class TheTestClass {
      def theFunc(): Unit = ???
      def theNoArgStringFunc(): String = ???
      def theNoArgIntFunc(): Int = ???
      def theNoArgValueClassFunc(): AnInt = ???
      def theNoParensStringFunc: String = ???
      def theArgStringFunc(one: String, two: AnyRef): String = ???
      def theAnyValArgStringFunc(one: String, two: AnyRef, three: Int, four: AnyVal): String = ???
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

      calling(theMock).theNoArgStringFunc().returns("A String")

      theMock.theNoArgStringFunc() shouldBe "A String"
    }

    "return AnyVal value if expected to do so - primitive" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgIntFunc().returns(1)

      theMock.theNoArgIntFunc() shouldBe 1
    }

    "return AnyVal value if expected to do so - value class" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgValueClassFunc().returns(AnInt(1))

      theMock.theNoArgValueClassFunc() shouldBe AnInt(1)
    }

    "return value if expected to do so - with params" in new Scope {
      val theMock = mock[TheTestClass]

      val two = new Object()

      calling(theMock).theArgStringFunc("one", two).returns("A String")

      theMock.theArgStringFunc("one", two) shouldBe "A String"
    }

    "return value if expected to do so - with AnyVal params" in new Scope {
      val theMock = mock[TheTestClass]

      val two = new Object()

      calling(theMock).theAnyValArgStringFunc("one", two, 3, 4).returns("A String")

      theMock.theAnyValArgStringFunc("one", two, 3, 4) shouldBe "A String"
    }

    "throw an exception if function is called with unexpected params" in new Scope {
      val theMock = mock[TheTestClass]

      val two = new Object()

      calling(theMock).theArgStringFunc("one", two).returns("A String")

      intercept[MockosaurUnexpectedFunctionParamsException] {
        theMock.theArgStringFunc("seven", two)
      }
    }

    "return value if expected to do so - no parens" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoParensStringFunc.returns("A String")

      theMock.theNoParensStringFunc shouldBe "A String"
    }

    "returns the expected results for multiple expectations in the order they were specified - specified separately" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("A String")
      calling(theMock).theNoArgStringFunc().returns("Another String")

      theMock.theNoArgStringFunc() shouldBe "A String"
      theMock.theNoArgStringFunc() shouldBe "Another String"

      intercept[MockosaurUnexpectedFunctionCallException] {
        theMock.theNoArgStringFunc()
      }
    }

    "returns the expected results for multiple expectations in the order they were specified - specified together" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returnsSequentially("A String", "Another String")

      theMock.theNoArgStringFunc() shouldBe "A String"
      theMock.theNoArgStringFunc() shouldBe "Another String"

      intercept[MockosaurUnexpectedFunctionCallException] {
        theMock.theNoArgStringFunc()
      }
    }

    "throws an exception when instructed to do so" in new Scope {
      val theMock = mock[TheTestClass]

      val exception = new IOException("TEST")
      calling(theMock).theNoArgStringFunc().throws(exception)

      val caught = intercept[IOException] {
        theMock.theNoArgStringFunc()
      }

      caught shouldBe exception
    }

    "throw a syxtem exception if throws is called when no mock record is ongoing" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        "a string".length.throws(new RuntimeException("TEST"))
      }
    }

    "implicits" in pending
    "returning functions" in pending
    "wildcards" in pending

    "verify all calls were made to a mock" in new Scope {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("A String")

      theMock.theNoArgStringFunc()

      verifyAllCallsWereMadeTo(theMock)
    }

    "verify all calls were made to multiple mocks" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1).theNoArgStringFunc().returns("A String")
      calling(theMock2).theNoArgStringFunc().returns("A String")

      theMock1.theNoArgStringFunc()
      theMock2.theNoArgStringFunc()

      verifyAllCallsWereMadeTo(theMock1, theMock2)
    }

    "throw an exception if a mocked call is not used" in new Scope() {
      val theMock = mock[TheTestClass]

      calling(theMock).theNoArgStringFunc().returns("A String")

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

    "throw an exception if returns is called when mock record is already complete - after returns" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        calling(theMock).theFunc().returns(1).returns(2)
      }
    }

    "throw an exception if returns is called when mock record is already complete - after returns sequentially" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        calling(theMock).theFunc().returns(1).returns(2)
      }
    }

    "throw an exception if returnsSequentially is called when mock record is already complete - after returns" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        calling(theMock).theFunc().returns(1).returnsSequentially(2)
      }
    }

    "throw an exception if returnsSequentially is called when mock record is already complete - after returns sequentially" in new Scope {
      val theMock = mock[TheTestClass]

      intercept[MockosaurNoOngoingRecordException] {
        calling(theMock).theFunc().returnsSequentially(1).returnsSequentially(2)
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

    "clear out expectations when resetting state (internal util)" in new Scope {
      val theMock1 = mock[TheTestClass]
      val theMock2 = mock[TheTestClass]

      calling(theMock1).theNoArgStringFunc().returns("A String")
      calling(theMock2).theNoArgStringFunc().returns("A String")

      MockState.reset()

      verifyAllCallsWereMadeTo(theMock1, theMock2)
    }
  }
}
