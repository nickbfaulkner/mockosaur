package mockosaur.exceptions

import mockosaur.model.{FunctionCallChain, Mock, FunctionCall}

class MockosaurException extends RuntimeException

case class MockosaurUnexpectedFunctionCallException(called: FunctionCall) extends MockosaurException
case class MockosaurUnexpectedFunctionParamsException(called: FunctionCall) extends MockosaurException

case class MockosaurUnmetExpectationException(unmet: Map[Mock, Seq[FunctionCallChain]]) extends MockosaurException
case class MockosaurIncompleteMockException() extends MockosaurException

class MockosaurUsageException extends MockosaurException
case class MockosaurNoOngoingRecordException() extends MockosaurUsageException
case class MockosaurRecordAlreadyOngoingException() extends MockosaurUsageException
case class MockosaurReturnsRequiredException() extends MockosaurUsageException