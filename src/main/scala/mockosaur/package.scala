
import java.lang.reflect.Method

import mockosaur.impl.{MockBuilder, MockFunctionHandler, MockState}
import mockosaur.model.{FunctionArg, FunctionCall, FunctionReturnValue, Mock}

import scala.reflect.ClassTag

package object mockosaur {

  def mock[T](implicit tag: ClassTag[T]): T = {
    MockBuilder.build(tag.runtimeClass.asInstanceOf[Class[T]], new MockFunctionHandler[T] {
      override def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef = {
        val result = MockState.processFunctionCall(Mock(mock), FunctionCall(method, args.map(FunctionArg.apply)))
        result.value.asInstanceOf[AnyRef]
      }
    })
  }

  def mock[T](obj: T)(implicit tag: ClassTag[T]): T = {
    this.mock(tag)
  }

  def calling[T](mock: T): T = {
    MockState.recordCalling(Mock(mock))
    mock
  }

  implicit class MockCallResultAnyRefReturning[T](val ref: T) extends AnyVal {
    def returns(toReturn: T): Unit = {
      MockState.recordingReturn(FunctionReturnValue(toReturn.asInstanceOf[AnyRef]))
    }

    def returnsSequentially(toReturn: T*): Unit = {
      MockState.recordingReturnSequentially(toReturn.map(value => FunctionReturnValue(value.asInstanceOf[AnyRef])))
    }
  }

  def verifyAllCallsWereMadeTo(mocks: AnyRef*): Unit = {
    require(mocks.nonEmpty)
    MockState.verifyNothingOutstanding(mocks.map(Mock.apply))
  }
}
