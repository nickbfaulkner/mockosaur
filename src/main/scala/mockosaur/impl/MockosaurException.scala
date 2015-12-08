package mockosaur.impl

import mockosaur._
import mockosaur.impl.MockosaurException._

class MockosaurException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this()                = this(null, null)
  def this(message: String) = this(message, null)
}

class MockosaurSystemException(message: String, cause: Throwable) extends MockosaurException(message, cause)

case class MockosaurUnexpectedFunctionCallException(called: FunctionCall) extends MockosaurException()
case class MockosaurUnexpectedFunctionParamsException(called: FunctionCall, expected: FunctionCall) extends MockosaurException("qqqqq") {
  override def getMessage: String =
    s"""
       |
       | Unexpected call to: ${prettySignature(expected)}
       |
       |   Expected: ${prettyCall(expected)}
       |     Called: ${prettyCall(called)}
       |
     """.stripMargin
}

case class MockosaurUnmetExpectationException(unmet: Map[Mock, Seq[FunctionCallChain]]) extends MockosaurException()
case class MockosaurIncompleteMockException() extends MockosaurException()

class MockosaurUsageException extends MockosaurException()
case class MockosaurNoOngoingRecordException() extends MockosaurUsageException
case class MockosaurRecordAlreadyOngoingException() extends MockosaurUsageException
case class MockosaurReturnsRequiredException() extends MockosaurUsageException
case class MockosaurWildcardTypeNotSpecifiedException() extends MockosaurUsageException

private object MockosaurException {
  def prettySignature(call: FunctionCall) = s"""${call.function.getName}(${call.args.map(_.value.getClass.getSimpleName).mkString(", ")})"""
  def prettyCall(call: FunctionCall) = s"""${call.function.getName}(${call.args.map(arg => stringOrWildcardPlaceholder(arg)).mkString(", ")})"""
  def stringOrWildcardPlaceholder(arg: FunctionArg) = if (arg.isWildcard) s"Any ${arg.value.getClass.getSimpleName}" else prettyValue(arg.value)

  def prettyValue(value: AnyRef): String = {
    val clazz = value.getClass

    if (clazz == classOf[String]) s""""$value""""
    else value.toString
  }
}
