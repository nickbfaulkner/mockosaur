package mockosaur.impl

import mockosaur.model.{FunctionCallChain, FunctionCall, FunctionReturnValue, Mock}

import scala.collection.mutable

private[mockosaur] object MockExpectationsState {

  case class IndividualMockState(inProgressRecording: Seq[FunctionCall],
                                 inProgressCallChain: Seq[FunctionCall],
                                 pendingExpectations: Seq[FunctionCallChain],
                                 invokedExpectations: Seq[FunctionCallChain])
  object IndividualMockState {
    val zero = IndividualMockState(Seq.empty, Seq.empty, Seq.empty, Seq.empty)
  }

  sealed trait MockCallResult
  object MockCallResult {
    case class Return(functionReturnValue: FunctionReturnValue) extends MockCallResult
    case object ContinueChain extends MockCallResult
    case object Unexpected extends MockCallResult
  }

  private val globalState: mutable.Map[Mock, IndividualMockState] =
    mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)

  def appendRecordedCallForMock(mock: Mock, call: FunctionCall) = MockExpectationsState.synchronized {
    val oldState = globalState(mock)
    val newState = oldState.copy(inProgressRecording = oldState.inProgressRecording :+ call)
    globalState.update(mock, newState)
  }

  def isCallRecordOngoing(mock: Mock, call: FunctionCall): Boolean = MockExpectationsState.synchronized {
    globalState(mock).inProgressRecording.headOption.contains(call)
  }

  def appendActualCallForMock(mock: Mock, call: FunctionCall): MockCallResult = MockExpectationsState.synchronized {
    val oldState = globalState(mock)
    val stateWithCall = oldState.copy(inProgressCallChain = oldState.inProgressCallChain :+ call)

    ((stateWithCall.pendingExpectations.find(_.calls == stateWithCall.inProgressCallChain) match {

      case Some(chain) => // if call chain matches an expectation

        val newPending = stateWithCall.pendingExpectations.diff(Seq(chain))
        val newInvoked = stateWithCall.invokedExpectations :+ chain
        val newState = stateWithCall.copy(inProgressCallChain = Seq.empty,
                                          pendingExpectations = newPending,
                                          invokedExpectations = newInvoked)

        MockCallResult.Return(chain.toReturn) -> newState

      case None =>

        val callsMatchStartOfCallChain = stateWithCall.pendingExpectations.exists(_.calls.startsWith(stateWithCall.inProgressCallChain))

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

  def completeCallChain(mock: Mock, toReturn: FunctionReturnValue) = MockExpectationsState.synchronized {
    val oldState = globalState(mock)
    val newExpectation = FunctionCallChain(oldState.inProgressRecording, toReturn)
    val newState = oldState.copy(inProgressRecording = Seq.empty,
                                 pendingExpectations = oldState.pendingExpectations:+ newExpectation)
    globalState.update(mock, newState)
  }

  def getUnmetExpectations(mock: Mock): Seq[FunctionCallChain] = MockExpectationsState.synchronized {
    globalState(mock).pendingExpectations
  }

  def hasOngoingRecordingState(mock: Mock): Boolean = MockExpectationsState.synchronized {
    globalState(mock).inProgressRecording.nonEmpty
  }

  def clear(): Unit = MockExpectationsState.synchronized {
    globalState.clear()
  }

  def printDebugString(): Unit = MockExpectationsState.synchronized {
    println("=================================================")
    println("= Mock State ====================================")
    println("=================================================")
    globalState.foreach { case (mock, state) =>
      println(s"$mock - $state")
    }
    println("=================================================")
  }

}
