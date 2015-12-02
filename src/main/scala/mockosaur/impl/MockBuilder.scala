package mockosaur.impl

import java.lang.reflect.Method

import net.sf.cglib.proxy.{Enhancer, MethodInterceptor, MethodProxy}
import org.objenesis.ObjenesisStd

private[mockosaur] object MockBuilder {
  def build[T](target: Class[T], handler: MockFunctionHandler[T]): T = {

    val interceptor = new MethodInterceptor() {
      override def intercept(obj: AnyRef, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
        if (method.getDeclaringClass == classOf[Object]) {
          proxy.invokeSuper(obj, args)
        } else {
          handler.functionCall(obj.asInstanceOf[T], method, args)
        }
      }
    }

    val enhancer = new Enhancer()
    enhancer.setSuperclass(target)
    enhancer.setCallbackType(interceptor.getClass)
    val proxiedClass = enhancer.createClass()
    Enhancer.registerCallbacks(proxiedClass, Array(interceptor))
    enhancer.setClassLoader(getClass.getClassLoader)

    val objenesis = new ObjenesisStd()
    val instantiator = objenesis.getInstantiatorOf(proxiedClass)
    instantiator.newInstance().asInstanceOf[T]
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
