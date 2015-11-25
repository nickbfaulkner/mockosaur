package mockosaur

import java.lang.reflect.Method

package object model {
  final case class FunctionCall(function: Method, args: Seq[FunctionArg])
  final case class FunctionCallChain(calls: Seq[FunctionCall], toReturn: FunctionReturnValue)

  final case class Mock(ref: AnyRef)
  final case class FunctionReturnValue(value: Any)
  final case class FunctionArg(value: AnyRef)
}
