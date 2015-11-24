package mockosaur.impl

import mockosaur.exceptions.{MockosaurNoOngoingRecordException, MockosaurRecordAlreadyOngoingException, MockosaurUnexpectedFunctionCallException}
import mockosaur.impl.MockState.ExpectedFunctionCall.{Complete, PreReturnValueSpecification}
import mockosaur.model.{FunctionCall, FunctionReturnValue, Mock}

import scala.collection.mutable

object MockState {

  case class IndividualMockState(isRecording: Boolean, expectations: Seq[ExpectedFunctionCall])
  object IndividualMockState {
    val zero = IndividualMockState(isRecording = false, expectations = Seq.empty)
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

  // todo - remove
  private[mockosaur] def reset(): Unit = globalState.synchronized {
    globalState.clear()
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

    if (state.isRecording) {
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
    findRecordingMock() match {
      case Some(existing) => throw MockosaurRecordAlreadyOngoingException()
      case None => globalState.update(mock, globalState(mock).copy(isRecording = true))
    }
  }

  def recordingReturn(toReturn: FunctionReturnValue): Unit = globalState.synchronized {
    findRecordingMock() match {
      case Some((mock, state)) => state.expectations.last match {
        case _: ExpectedFunctionCall.Complete => throw MockosaurNoOngoingRecordException()
        case spec: ExpectedFunctionCall.PreReturnValueSpecification =>
          val completedExpectations = state.expectations.dropRight(1) :+ spec.complete(toReturn)
          globalState.update(mock, state.copy(isRecording = false, expectations = completedExpectations))
      }
      case None => throw MockosaurNoOngoingRecordException()
    }
  }

  private def findExpectedFunctionCall(mock: Mock, call: FunctionCall): Option[ExpectedFunctionCall] = globalState.synchronized {
    globalState(mock).expectations.find(_.call == call)
  }

  private def findRecordingMock(): Option[(Mock, IndividualMockState)] = {
    globalState find { case (_, state) => state.isRecording }
  }
}
