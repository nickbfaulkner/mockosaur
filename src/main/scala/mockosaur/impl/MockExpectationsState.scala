package mockosaur.impl

import java.util.concurrent.atomic.AtomicReference

import mockosaur.{FunctionCall, FunctionCallChain, FunctionResult, Mock}

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
    case class UnexpectedParams(expectedCall: FunctionCall) extends MockCallResult
    case object UnexpectedCall extends MockCallResult
  }

  private sealed trait CallChainMatch
  private object CallChainMatch {
    case class Match(chain: FunctionCallChain) extends CallChainMatch
    case object NoMatch extends CallChainMatch
    case class ArgMismatch(expectedCall: FunctionCall) extends CallChainMatch
    case object ChainPrefixMatch extends CallChainMatch
  }

  private def buildEmptyState() = mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)
  private val globalStateRef: AtomicReference[mutable.Map[Mock, IndividualMockState]] = new AtomicReference(buildEmptyState())
  private def globalState: mutable.Map[Mock, IndividualMockState] = globalStateRef.get

  // todo - convert to a weak identity collection
  private val wildcardPlaceholders = mutable.ListBuffer[Any]()

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

    findCallChainMatch(stateWithCall.inProgressCallChain, stateWithCall.pendingExpectations) match {
      case CallChainMatch.ChainPrefixMatch          => MockCallResult.ContinueChain
      case CallChainMatch.ArgMismatch(expectedCall) => MockCallResult.UnexpectedParams(expectedCall)
      case CallChainMatch.NoMatch                   => MockCallResult.UnexpectedCall
      case CallChainMatch.Match(matchedChain)       =>

        // move expectation from pending to invoked so it's not a candidate next time
        val newPending = stateWithCall.pendingExpectations.diff(Seq(matchedChain))
        val newInvoked = stateWithCall.invokedExpectations :+ matchedChain
        val newState = stateWithCall.copy(inProgressCallChain = Seq.empty,
                                          pendingExpectations = newPending,
                                          invokedExpectations = newInvoked)

        globalState.update(mock, newState)

        MockCallResult.Result(matchedChain.result)
    }
  }

  def addWildcardPlaceholder(wildcardPlaceholder: Any) = MockExpectationsState.synchronized {
    wildcardPlaceholders += wildcardPlaceholder
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

  private def findCallChainMatch(needle: Seq[FunctionCall], haystack: Seq[FunctionCallChain]): CallChainMatch = {
    // todo - next - has a placeholder and doesn't consult wildcard list
    haystack.find(_.calls == needle) match {
      case Some(chain) => CallChainMatch.Match(chain)
      case None =>

        if (haystack.exists(_.calls.startsWith(needle))) {
          CallChainMatch.ChainPrefixMatch
        } else {

          val expectedCallWithArgMismatch: Option[FunctionCall] = ???
          expectedCallWithArgMismatch match {
            case Some(mismatch) => CallChainMatch.ArgMismatch(mismatch)
            case None           => CallChainMatch.NoMatch
          }

        }

    }
  }

}
