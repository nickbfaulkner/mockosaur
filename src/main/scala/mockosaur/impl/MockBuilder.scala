package mockosaur.impl

import java.lang.reflect.Method

import net.sf.cglib.proxy.{Enhancer, MethodInterceptor, MethodProxy}
import org.objenesis.ObjenesisStd

object MockBuilder {
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
    if (target.isAssignableFrom(Void.TYPE)) {
      ().asInstanceOf[T]
    } else {
      val objenesis = new ObjenesisStd()
      val instantiator = objenesis.getInstantiatorOf(target)
      instantiator.newInstance()
    }
  }
}

trait MockFunctionHandler[T] {
  def functionCall(mock: T, method: Method, args: Seq[AnyRef]): AnyRef
}
