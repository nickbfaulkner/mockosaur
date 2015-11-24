package mockosaur.impl

import mockosaur.exceptions.{MockosaurNoOngoingRecordException, MockosaurRecordAlreadyOngoingException, MockosaurUnexpectedFunctionCallException}
import mockosaur.impl.MockExpectationsState.MockCallResult.{ContinueChain, Return, Unexpected}
import mockosaur.model.{FunctionCall, FunctionReturnValue, Mock}

object MockState {

  def processFunctionCall(mock: Mock,
                          call: FunctionCall): FunctionReturnValue = {

    val zombie = FunctionReturnValue(MockBuilder.buildZombie(call.function.getReturnType))

    if (MockRecordingState.isRecording(mock)) {
      // todo - check if call matches one already in chain (MockosaurReturnsRequiredException)
      MockExpectationsState.appendRecordedCallForMock(mock, call)
      zombie
    } else {
      MockExpectationsState.appendActualCallForMock(mock, call) match {
        case Return(toReturn) => toReturn
        case ContinueChain    => zombie
        case Unexpected       => throw MockosaurUnexpectedFunctionCallException(call)
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

  def recordingReturn(toReturn: FunctionReturnValue): Unit = {
    MockRecordingState.getRecordingMock() match {
      case None       => throw MockosaurNoOngoingRecordException()
      case Some(mock) => MockExpectationsState.completeCallChain(mock, toReturn)
                         MockRecordingState.stopRecording()
    }
  }

  // this is required because the tests explicitly leave mocks in a bad state
  private[mockosaur] def reset(): Unit = {
    MockExpectationsState.clear()
    MockRecordingState.stopRecording()
  }
}
