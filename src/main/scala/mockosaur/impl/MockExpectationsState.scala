package mockosaur.impl

import mockosaur.model.{FunctionCall, FunctionReturnValue, Mock}

import scala.collection.mutable

private[mockosaur] object MockExpectationsState {

  case class IndividualMockState(inProgressRecording: Seq[FunctionCall],
                                 inProgressCallChain: Seq[FunctionCall],
                                 expectations: Seq[FunctionCallChain])
  object IndividualMockState {
    val zero = IndividualMockState(Seq.empty, Seq.empty, Seq.empty)
  }

  case class FunctionCallChain(calls: Seq[FunctionCall], toReturn: FunctionReturnValue)

  sealed trait MockCallResult
  object MockCallResult {
    case class Return(functionReturnValue: FunctionReturnValue) extends MockCallResult
    case object ContinueChain extends MockCallResult
    case object Unexpected extends MockCallResult
  }

  private val globalState: mutable.Map[Mock, IndividualMockState] =
    mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)

  def appendRecordedCallForMock(mock: Mock, call: FunctionCall) = globalState.synchronized {
    val oldState = globalState(mock)
    val newState = oldState.copy(inProgressRecording = oldState.inProgressRecording :+ call)
    globalState.update(mock, newState)
  }

  def isCallRecordOngoing(mock: Mock, call: FunctionCall): Boolean = globalState.synchronized {
    globalState(mock).inProgressRecording.headOption.contains(call)
  }

  def appendActualCallForMock(mock: Mock, call: FunctionCall): MockCallResult = globalState.synchronized {
    val oldState = globalState(mock)
    val stateWithCall = oldState.copy(inProgressCallChain = oldState.inProgressCallChain :+ call)

    ((stateWithCall.expectations.find(_.calls == stateWithCall.inProgressCallChain) match {

      case Some(FunctionCallChain(_, toReturn)) => // if call chain matches an expectation
        MockCallResult.Return(toReturn) -> stateWithCall.copy(inProgressCallChain = Seq.empty)

      case None =>
        val callsMatchStartOfCallChain = stateWithCall.expectations.exists(_.calls.startsWith(stateWithCall.inProgressCallChain))

        if (callsMatchStartOfCallChain) {
          MockCallResult.ContinueChain -> stateWithCall
        } else {
          MockCallResult.Unexpected -> stateWithCall
        }

    }): (MockCallResult, IndividualMockState)) match {
      case (result: MockCallResult, newState: IndividualMockState) =>
        globalState.update(mock, newState)
        result
    }
  }

  def completeCallChain(mock: Mock, toReturn: FunctionReturnValue) = globalState.synchronized {
    val oldState = globalState(mock)
    val newExpectation = FunctionCallChain(oldState.inProgressRecording, toReturn)
    val newState = oldState.copy(inProgressRecording = Seq.empty,
                                 expectations = oldState.expectations:+ newExpectation)
    globalState.update(mock, newState)
  }

  def clear() = globalState.clear()

  def printDebugString() = {
    println("=================================================")
    println("= Mock State ====================================")
    println("=================================================")
    globalState.foreach { case (mock, state) =>
      println(s"$mock - $state")
    }
    println("=================================================")
  }

}
