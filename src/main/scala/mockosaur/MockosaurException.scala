package mockosaur

class MockosaurException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this()                = this(null, null)
  def this(message: String) = this(message, null)
}

class MockosaurSystemException(message: String, cause: Throwable) extends MockosaurException(message, cause)

case class MockosaurUnexpectedFunctionCallException(called: FunctionCall) extends MockosaurException()
case class MockosaurUnexpectedFunctionParamsException(called: FunctionCall) extends MockosaurException()

case class MockosaurUnmetExpectationException(unmet: Map[Mock, Seq[FunctionCallChain]]) extends MockosaurException()
case class MockosaurIncompleteMockException() extends MockosaurException()

class MockosaurUsageException extends MockosaurException()
case class MockosaurNoOngoingRecordException() extends MockosaurUsageException
case class MockosaurRecordAlreadyOngoingException() extends MockosaurUsageException
case class MockosaurReturnsRequiredException() extends MockosaurUsageException
