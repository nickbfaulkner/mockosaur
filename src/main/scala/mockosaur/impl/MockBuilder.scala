package mockosaur.impl

import java.lang.reflect.{InvocationHandler, Method}

import net.bytebuddy.ByteBuddy
import net.bytebuddy.dynamic.loading.ClassLoadingStrategy
import net.bytebuddy.implementation.InvocationHandlerAdapter
import net.bytebuddy.matcher.ElementMatchers._
import org.objenesis.ObjenesisStd

private[mockosaur] object MockBuilder {
  def build[T](target: Class[T], handler: MockFunctionHandler[T]): T = {

    val proxiedClass = new ByteBuddy()
      .subclass(target)
      .method(not(isDeclaredBy(classOf[Object])))
      .intercept(InvocationHandlerAdapter.of(new InvocationHandler {
        override def invoke(proxy: scala.Any, method: Method, args: Array[AnyRef]): AnyRef = {
          handler.functionCall(proxy.asInstanceOf[T], method, args)
        }
      }))
      .make()
      .load(this.getClass.getClassLoader, ClassLoadingStrategy.Default.WRAPPER)
      .getLoaded

    val objenesis = new ObjenesisStd()
    val instantiator = objenesis.getInstantiatorOf(proxiedClass)
    instantiator.newInstance()
  }

  def buildZombie[T](target: Class[T]): T = {
    (target match {
      case java.lang.Void.TYPE      => ()
      case java.lang.Boolean.TYPE   => true
      case java.lang.Byte.TYPE      => Byte.MaxValue
      case java.lang.Character.TYPE => 'a'
      case java.lang.Double.TYPE    => Double.MaxValue
      case java.lang.Float.TYPE     => Float.MaxValue
      case java.lang.Integer.TYPE   => Int.MaxValue
      case java.lang.Long.TYPE      => Long.MaxValue
      case java.lang.Short.TYPE     => Short.MaxValue
      case _ =>
        val objenesis = new ObjenesisStd()
        val instantiator = objenesis.getInstantiatorOf(target)
        instantiator.newInstance()
    }).asInstanceOf[T]
  }
}

private[mockosaur] trait MockFunctionHandler[T] {
  def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef
}
