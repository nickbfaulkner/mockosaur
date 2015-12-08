package mockosaur.impl

import java.util.concurrent.atomic.AtomicReference

import mockosaur._

import scala.collection.mutable

private[mockosaur] object MockExpectationsState {

  case class IndividualMockState(inProgressRecording: Seq[FunctionCall],
                                 inProgressCallChain: Seq[FunctionCall],
                                 pendingExpectations: Seq[FunctionCallChain],
                                 invokedExpectations: Seq[FunctionCallChain])

  private case class ArgumentMismatch(expected: FunctionCall, actual: FunctionCall)

  object IndividualMockState {
    val zero = IndividualMockState(Seq.empty, Seq.empty, Seq.empty, Seq.empty)
  }

  sealed trait MockCallResult
  object MockCallResult {
    case class Result(functionResult: FunctionResult) extends MockCallResult
    case object ContinueChain extends MockCallResult
    case class UnexpectedParams(expectedCall: FunctionCall, actualCall: FunctionCall) extends MockCallResult
    case object UnexpectedCall extends MockCallResult
  }

  private sealed trait CallChainMatch
  private object CallChainMatch {
    case class Match(chain: FunctionCallChain) extends CallChainMatch
    case object NoMatch extends CallChainMatch
    case class ArgMismatch(expectedCall: FunctionCall, actualCall: FunctionCall) extends CallChainMatch
    case object ChainPrefixMatch extends CallChainMatch
  }

  private def buildEmptyState() = mutable.WeakHashMap[Mock, IndividualMockState]().withDefaultValue(IndividualMockState.zero)
  private val globalStateRef: AtomicReference[mutable.Map[Mock, IndividualMockState]] = new AtomicReference(buildEmptyState())
  private def globalState: mutable.Map[Mock, IndividualMockState] = globalStateRef.get

  // todo - convert to a weak identity collection
  private val wildcardPlaceholders = mutable.ListBuffer[Any]()

  def appendRecordedCallForMock(mock: Mock, call: FunctionCall) = MockExpectationsState.synchronized {

    def swapForWildcard(arg: FunctionArg): FunctionArg = {
      val isWildcard = wildcardPlaceholders.exists(_.asInstanceOf[AnyRef] eq arg.value.asInstanceOf[AnyRef])

      if (isWildcard) FunctionWildcardArgument(arg.value)
      else arg
    }

    val oldState = globalState(mock)
    val callWithWildcards = call.copy(args = call.args.map(swapForWildcard))
    val newState = oldState.copy(inProgressRecording = oldState.inProgressRecording :+ callWithWildcards)
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

      case CallChainMatch.NoMatch                   => MockCallResult.UnexpectedCall

      case mismatch: CallChainMatch.ArgMismatch     => MockCallResult.UnexpectedParams(expectedCall = mismatch.expectedCall,
                                                                                       actualCall = mismatch.actualCall)
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

    haystack.find(h => consideringWildcards.isTotalMatch(needle, h.calls)) match {
      case Some(chain) => CallChainMatch.Match(chain)
      case None =>

        if (haystack.exists(h => consideringWildcards.isPrefixMatch(needle, h.calls))) {
          CallChainMatch.ChainPrefixMatch
        } else {

          consideringWildcards.findArgumentMismatch(needle, haystack.map(_.calls)) match {
            case Some(mismatch) => CallChainMatch.ArgMismatch(expectedCall = mismatch.expected,
                                                              actualCall   = mismatch.actual)
            case None           => CallChainMatch.NoMatch
          }

        }

    }
  }

  private object consideringWildcards {

    def isTotalMatch(needleSeq: Seq[FunctionCall], haystackSeq: Seq[FunctionCall]): Boolean = {
      (needleSeq.size == haystackSeq.size) && isPrefixMatch(needleSeq, haystackSeq)
    }

    def isPrefixMatch(needleSeq: Seq[FunctionCall], haystackSeq: Seq[FunctionCall]): Boolean = {
      if (needleSeq.size != haystackSeq.size) {
        false
      } else {
        needleSeq.zip(haystackSeq) forall { case (needleCall, haystackCall) =>
          this.isCallMatch(call = needleCall, expectedCall = haystackCall)
        }
      }
    }

    def findArgumentMismatch(needle: Seq[FunctionCall], haystack: Seq[Seq[FunctionCall]]): Option[ArgumentMismatch] = {

      needle.headOption match {
        case None => None
        case Some(currentCall) =>

          haystack.filter(chain => chain.headOption.exists(_.function == currentCall.function)) match {
            case Nil => None
            case haystackWithMethodMatch =>

              val candiateCalls = haystackWithMethodMatch.flatMap(_.headOption.toSeq)
              candiateCalls.find(candidateCall => !isCallMatch(currentCall, candidateCall)) match {
                case Some(mismatch) => Some(ArgumentMismatch(expected = mismatch, actual = currentCall))
                case None           => findArgumentMismatch(needle.tail, haystackWithMethodMatch.map(_.tail))
              }

          }
      }
    }

    private def isCallMatch(call: FunctionCall, expectedCall: FunctionCall): Boolean = {

      if ((call.function != expectedCall.function) || (call.args.size != expectedCall.args.size)) {
        false
      } else {
        call.args.zip(expectedCall.args) forall { case (callArg, expectedArg) =>
          expectedArg.isWildcard || (callArg == expectedArg)
        }
      }

    }
  }

}
