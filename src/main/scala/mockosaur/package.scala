
import java.lang.reflect.Method

import mockosaur.exceptions.MockosaurSystemException
import mockosaur.impl.{MockBuilder, MockFunctionHandler, MockState, ReflectionUtils}
import mockosaur.model._

import scala.reflect.ClassTag
import scala.util.{Failure, Success}

package object mockosaur {

  def mock[T](implicit tag: ClassTag[T]): T = {
    MockBuilder.build(tag.runtimeClass.asInstanceOf[Class[T]], new MockFunctionHandler[T] {
      override def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef = {
        val result = MockState.processFunctionCall(Mock(mock), FunctionCall(method, args.map(FunctionArg.apply)))
        result match {
          case FunctionResult.Return(toReturn) => forceAnyToExpectedReturnType(toReturn, method.getReturnType)
          case FunctionResult.Throw(toThrow) => throw toThrow
        }
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

  def verifyAllCallsWereMadeTo(mocks: AnyRef*): Unit = {
    require(mocks.nonEmpty)
    MockState.verifyNothingOutstanding(mocks.map(Mock.apply))
  }

  implicit class MockCallResultAnyRefResults[T](val ref: T) extends AnyVal {
    def returns(toReturn: T): Unit = {
      MockState.recordingReturn(FunctionResult.Return(toReturn.asInstanceOf[AnyRef]))
    }

    def returnsSequentially(toReturn: T*): Unit = {
      MockState.recordingReturnSequentially(toReturn.map(value => FunctionResult.Return(value.asInstanceOf[AnyRef])))
    }

    def throws(t: Throwable): Unit = {
      MockState.recordingThrow(FunctionResult.Throw(t))
    }
  }

  private def forceAnyToExpectedReturnType(value: Any, _expectedReturnType: Class[_]): AnyRef = {
    val expectedReturnType = primitiveToBoxedType(_expectedReturnType)

    if (value.getClass == expectedReturnType || value.isInstanceOf[Unit]) {
      value.asInstanceOf[AnyRef]
    } else {
      // we've type checked at compile time and boxed the expectation but
      // the class still doesn't match... assume that the value is a value class
      ReflectionUtils.unwrapValueClass(value) match {
        case Success(result) => result
        case Failure(t: Throwable) => throw new MockosaurSystemException(s"Could not process the return type of ${value.getClass}, expected $expectedReturnType", t)
      }
    }

  }

  private def primitiveToBoxedType(value: Class[_]): Class[_] = {
    value match {
      case java.lang.Boolean.TYPE   => classOf[java.lang.Boolean]
      case java.lang.Byte.TYPE      => classOf[java.lang.Byte]
      case java.lang.Character.TYPE => classOf[java.lang.Character]
      case java.lang.Double.TYPE    => classOf[java.lang.Double]
      case java.lang.Float.TYPE     => classOf[java.lang.Float]
      case java.lang.Integer.TYPE   => classOf[java.lang.Integer]
      case java.lang.Long.TYPE      => classOf[java.lang.Long]
      case java.lang.Short.TYPE     => classOf[java.lang.Short]
      case other                    => other
    }
  }

}
