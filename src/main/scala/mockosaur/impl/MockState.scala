package mockosaur.impl

import mockosaur._
import mockosaur.impl.MockExpectationsState.MockCallResult._

private[mockosaur] object MockState {

  def processFunctionCall(mock: Mock,
                          call: FunctionCall): FunctionResult = {

    val zombie = FunctionResult.Return(MockBuilder.buildZombie(call.function.getReturnType))

    if (MockRecordingState.isRecording(mock)) {
      if (MockExpectationsState.isCallRecordOngoing(mock, call)) {
        throw MockosaurReturnsRequiredException()
      }
      MockExpectationsState.appendRecordedCallForMock(mock, call)
      zombie
    } else {
      MockExpectationsState.appendActualCallForMock(mock, call) match {
        case Result(result)                 => result
        case ContinueChain                  => zombie
        case UnexpectedParams(expectedCall) => throw MockosaurUnexpectedFunctionParamsException(call, expectedCall)
        case UnexpectedCall                 => throw MockosaurUnexpectedFunctionCallException(call)
      }
    }
  }

  def recordCalling(mock: Mock): Unit = {
    if (MockRecordingState.getRecordingMock().isDefined) {
      throw MockosaurRecordAlreadyOngoingException()
    } else {
      MockRecordingState.startRecording(mock)
    }
  }

  def recordingReturn(toReturn: FunctionResult.Return): Unit = {
    recordingReturnSequentially(Seq(toReturn))
  }

  def recordingReturnSequentially(toReturn: Seq[FunctionResult.Return]): Unit = {
    MockRecordingState.getRecordingMock() match {
      case None       => throw MockosaurNoOngoingRecordException()
      case Some(mock) => MockExpectationsState.completeCallChain(mock, toReturn)
                         MockRecordingState.stopRecording()
    }
  }

  def recordingThrow(toThrow: FunctionResult.Throw): Unit = {
    MockRecordingState.getRecordingMock() match {
      case None       => throw MockosaurNoOngoingRecordException()
      case Some(mock) => MockExpectationsState.completeCallChainWithThrow(mock, toThrow)
                         MockRecordingState.stopRecording()
    }
  }

  def verifyNothingOutstanding(mocks: Seq[Mock]): Unit = {
    if (mocks.exists(MockExpectationsState.hasOngoingRecordingState)) {
      throw MockosaurIncompleteMockException()
    } else {
      val unmet = mocks.map(m => m -> MockExpectationsState.getUnmetExpectations(m)).toMap
      if (unmet.values.exists(_.nonEmpty)) {
        throw MockosaurUnmetExpectationException(unmet)
      }
    }
  }

  def newWildcardValue[T](ofType: Class[T]): T = {
    if (ofType == classOf[Nothing]) {
      throw new MockosaurWildcardTypeNotSpecifiedException()
    }
    val wildcardPlaceholder = MockBuilder.buildZombie(ofType)
    MockExpectationsState.addWildcardPlaceholder(wildcardPlaceholder)
    wildcardPlaceholder
  }

  // this is required because the tests explicitly leave mocks in a bad state
  private[mockosaur] def reset(): Unit = {
    MockExpectationsState.clear()
    MockRecordingState.stopRecording()
  }

}
