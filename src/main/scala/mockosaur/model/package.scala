package mockosaur

import java.lang.reflect.Method

package object model {
  final case class FunctionCall(function: Method, args: Seq[FunctionArg])
  final case class FunctionCallChain(calls: Seq[FunctionCall], result: FunctionResult)

  final case class Mock(ref: Any)

  sealed trait FunctionResult
  object FunctionResult {
    final case class Return(value: Any) extends FunctionResult
    final case class Throw(toThrow: Throwable) extends FunctionResult
  }

  final case class FunctionArg(value: AnyRef)
}
