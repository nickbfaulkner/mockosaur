
import java.lang.reflect.Method

import mockosaur.impl.{MockBuilder, MockFunctionHandler, MockState}
import mockosaur.model.{FunctionArg, FunctionCall, FunctionReturnValue, Mock}

import scala.reflect.ClassTag

package object mockosaur {

  def mock[T <: AnyRef](implicit tag: ClassTag[T]): T = {

    MockBuilder.build(tag.runtimeClass.asInstanceOf[Class[T]], new MockFunctionHandler[T] {
      override def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef = {
        println("============================")
        println(method)
        println(args)
        println("============================")
        val result = MockState.processFunctionCall(Mock(mock), FunctionCall(method, args.map(FunctionArg.apply)))
        result.value.asInstanceOf[AnyRef]
      }
    })
  }

  def calling[T <: AnyRef](mock: T): T = {
    MockState.recordingCall(Mock(mock))
    mock
  }

  implicit class MockCallResultAnyRefReturning[T](val ref: T) extends AnyVal {
    def returning(toReturn: T): T = {
      MockState.recordingReturn(FunctionReturnValue(toReturn.asInstanceOf[AnyRef]))
      toReturn
    }
  }

}
