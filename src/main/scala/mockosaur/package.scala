
import java.lang.reflect.Method

import mockosaur.impl.{MockBuilder, MockFunctionHandler, MockState}
import mockosaur.model.{FunctionArg, FunctionCall, FunctionReturnValue, Mock}

import scala.reflect.ClassTag

package object mockosaur {

  def mock[T <: AnyRef](implicit tag: ClassTag[T]): T = {
    MockBuilder.build(tag.runtimeClass.asInstanceOf[Class[T]], new MockFunctionHandler[T] {
      override def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef = {
        println("--- Called " + method + " with args " + args)
        val result = MockState.processFunctionCall(Mock(mock), FunctionCall(method, args.map(FunctionArg.apply)))
        result.value.asInstanceOf[AnyRef]
      }
    })
  }

  def mock[T <: AnyRef](obj: T)(implicit tag: ClassTag[T]): T = {
    this.mock(tag)
  }

  def calling[T <: AnyRef](mock: T): T = {
    MockState.recordCalling(Mock(mock))
    mock
  }

  implicit class MockCallResultAnyRefReturning[T](val ref: T) extends AnyVal {
    def returns(toReturn: T): T = {
      MockState.recordingReturn(FunctionReturnValue(toReturn.asInstanceOf[AnyRef]))
      toReturn
    }
  }

  def verifyAllCallsWereMadeTo(mocks: AnyRef*): Unit = {
    require(mocks.nonEmpty)
    MockState.verifyNothingOutstanding(mocks.map(Mock.apply))
  }
}
