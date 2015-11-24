package mockosaur.impl

import mockosaur.exceptions.{MockosaurNoOngoingRecordException, MockosaurRecordAlreadyOngoingException, MockosaurUnexpectedFunctionCallException}
import mockosaur.impl.MockState.ExpectedFunctionCall.{Complete, PreReturnValueSpecification}
import mockosaur.model.{FunctionCall, FunctionReturnValue, Mock}

import scala.collection.mutable

object MockState {
  import MockRecordingState._

  case class IndividualMockState(expectations: Seq[ExpectedFunctionCall])
  object IndividualMockState {
    val zero = IndividualMockState(expectations = Seq.empty)
  }

  sealed abstract class ExpectedFunctionCall {
    val call: FunctionCall
  }

  object ExpectedFunctionCall {
    case class Complete(call: FunctionCall, returnValue: FunctionReturnValue) extends ExpectedFunctionCall
    case class PreReturnValueSpecification(call: FunctionCall) extends ExpectedFunctionCall {
      def complete(returnValue: FunctionReturnValue) = Complete(call, returnValue)
    }
  }

  private val globalState: mutable.Map[Mock, IndividualMockState] =
    mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)

  // this is required because the tests explicitly leave mocks in a bad state
  private[mockosaur] def reset(): Unit = globalState.synchronized {
    globalState.clear()
    stopRecording()
  }

  private[mockosaur] def printDebugString() = {
    println("=================================================")
    println("= Mock State ====================================")
    println("=================================================")
    globalState.foreach { case (mock, state) =>
        println(s"$mock - $state")
    }
    println("=================================================")
  }

  def processFunctionCall(mock: Mock,
                          call: FunctionCall): FunctionReturnValue = globalState.synchronized {
    val state: IndividualMockState = globalState(mock)

    if (isRecording(mock)) {
      val newState = state.copy(expectations = state.expectations :+ ExpectedFunctionCall.PreReturnValueSpecification(call))

      globalState.update(mock, newState)

      FunctionReturnValue(MockBuilder.buildZombie(call.function.getReturnType))
    } else {
      findExpectedFunctionCall(mock, call) match {
        case Some(expectedCall: Complete) => expectedCall.returnValue
        case Some(expectedCall: PreReturnValueSpecification) => ??? // todo
        case None => throw MockosaurUnexpectedFunctionCallException(call)
      }
    }
  }

  def recordingCall(mock: Mock): Unit = globalState.synchronized {
    getRecordingMock() match {
      case Some(existing) => throw MockosaurRecordAlreadyOngoingException()
      case None => startRecording(mock)
                   globalState.update(mock, globalState(mock))
    }
  }

  def recordingReturn(toReturn: FunctionReturnValue): Unit = globalState.synchronized {
    getRecordingMock() map (m => m -> globalState(m)) match {
      case Some((mock, state)) => state.expectations.last match {
        case _: ExpectedFunctionCall.Complete => throw MockosaurNoOngoingRecordException()
        case spec: ExpectedFunctionCall.PreReturnValueSpecification =>
          stopRecording()
          val completedExpectations = state.expectations.dropRight(1) :+ spec.complete(toReturn)
          globalState.update(mock, state.copy(expectations = completedExpectations))
      }
      case None => throw MockosaurNoOngoingRecordException()
    }
  }

  private def findExpectedFunctionCall(mock: Mock, call: FunctionCall): Option[ExpectedFunctionCall] = globalState.synchronized {
    globalState(mock).expectations.find(_.call == call)
  }

}
