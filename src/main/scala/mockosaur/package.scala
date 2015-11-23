
import java.lang.reflect.Method
import net.sf.cglib.proxy._
import org.objenesis._
import scala.reflect.ClassTag

package object mockosaur {

  def mock[T](implicit tag: ClassTag[T]): T = {

    createProxiedMock(tag.runtimeClass.asInstanceOf[Class[T]], new InvocationHandler {
      override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = {
        println("==============================")
        println(s"invoked $proxy   $method   $args")
        println("==============================")
        null
      }
    })
  }

  def createProxiedMock[T](target: Class[T], invocationHandler: InvocationHandler): T = {
    val interceptor = new MethodInterceptor() {
      override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
        invocationHandler.invoke(obj, method, args)
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
