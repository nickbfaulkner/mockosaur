import scala.reflect.ClassTag
import org.objenesis._

package object mockosaur {
  def mock[T](implicit tag: ClassTag[T]): T = {
    val objenesis = new ObjenesisStd()
    val instantiator = objenesis.getInstantiatorOf(tag.runtimeClass)
    instantiator.newInstance().asInstanceOf[T]
  }
}
