package mockosaur

import java.lang.reflect.Method

final case class FunctionCall(function: Method, args: Seq[FunctionArg])
final case class FunctionCallChain(calls: Seq[FunctionCall], result: FunctionResult)
final case class Mock(ref: Any)

private[mockosaur] sealed trait FunctionResult
private[mockosaur] object FunctionResult {
  final case class Return(value: Any) extends FunctionResult
  final case class Throw(toThrow: Throwable) extends FunctionResult
}

private[mockosaur] sealed trait FunctionArg {
  val value: AnyRef
  val isWildcard: Boolean
}
private[mockosaur] final case class FunctionArgument(value: AnyRef) extends FunctionArg {
  override val isWildcard = false
}
private[mockosaur] final case class FunctionWildcardArgument(value: AnyRef) extends FunctionArg {
  override val isWildcard = true
}
