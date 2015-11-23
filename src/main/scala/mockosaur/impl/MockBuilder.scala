package mockosaur.impl

import java.lang.reflect.Method

import net.sf.cglib.proxy.{Enhancer, MethodInterceptor, MethodProxy}
import org.objenesis.ObjenesisStd

object MockBuilder {
  def build[T](target: Class[T], handler: MockFunctionHandler[T]): T = {

    val interceptor = new MethodInterceptor() {
      override def intercept(obj: AnyRef, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
        if (method.getDeclaringClass == classOf[Object]) {
          println("ignoring intercept")
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
}

trait MockFunctionHandler[T] {
  def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef
}
