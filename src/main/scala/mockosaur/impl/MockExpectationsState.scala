package mockosaur.impl

import java.util.concurrent.atomic.AtomicReference

import mockosaur.model._

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
    case class Result(functionResult: FunctionResult) extends MockCallResult
    case object ContinueChain extends MockCallResult
    case object UnexpectedParams extends MockCallResult
    case object UnexpectedCall extends MockCallResult
  }

  private def buildEmptyState() = mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)
  private val globalStateRef: AtomicReference[mutable.Map[Mock, IndividualMockState]] = new AtomicReference(buildEmptyState())
  private def globalState: mutable.Map[Mock, IndividualMockState] = globalStateRef.get

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

        MockCallResult.Result(chain.result) -> newState

      case None =>

        val callsMatchStartOfCallChain = stateWithCall.pendingExpectations.exists(_.calls.startsWith(stateWithCall.inProgressCallChain))

        if (callsMatchStartOfCallChain) {
          MockCallResult.ContinueChain -> stateWithCall
        } else {
          val methodMatchStartOfCallChain = stateWithCall.pendingExpectations.exists(_.calls.map(_.function).startsWith(stateWithCall.inProgressCallChain.map(_.function)))
          val rejectionType =
            if (methodMatchStartOfCallChain) {
              MockCallResult.UnexpectedParams
            } else {
              MockCallResult.UnexpectedCall
            }
          rejectionType -> stateWithCall
        }

    }): (MockCallResult, IndividualMockState)) match {
      case (result: MockCallResult, newState: IndividualMockState) =>
        globalState.update(mock, newState)
        result
    }
  }

  def completeCallChain(mock: Mock, toReturn: Seq[FunctionResult.Return]) = MockExpectationsState.synchronized {
    val oldState = globalState(mock)
    val newExpectations = toReturn.map(FunctionCallChain(oldState.inProgressRecording, _))
    val newState = oldState.copy(inProgressRecording = Seq.empty,
                                 pendingExpectations = oldState.pendingExpectations ++ newExpectations)
    globalState.update(mock, newState)
  }

  def completeCallChainWithThrow(mock: Mock, toThrow: FunctionResult.Throw) = MockExpectationsState.synchronized {
    val oldState = globalState(mock)
    val newExpectation = FunctionCallChain(oldState.inProgressRecording, toThrow)
    val newState = oldState.copy(inProgressRecording = Seq.empty,
                                 pendingExpectations = oldState.pendingExpectations :+ newExpectation)
    globalState.update(mock, newState)
  }

  def getUnmetExpectations(mock: Mock): Seq[FunctionCallChain] = MockExpectationsState.synchronized {
    globalState(mock).pendingExpectations
  }

  def hasOngoingRecordingState(mock: Mock): Boolean = MockExpectationsState.synchronized {
    globalState(mock).inProgressRecording.nonEmpty
  }

  def clear(): Unit = MockExpectationsState.synchronized {
    globalStateRef.set(buildEmptyState())
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
