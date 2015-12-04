
import mockosaur.impl._

import scala.reflect.ClassTag

package object mockosaur {

  def mock[T <: Any](implicit tag: ClassTag[T]): T = MockosaurOperations.mock[T]

  def mock[T](obj: T)(implicit tag: ClassTag[T]): T = MockosaurOperations.mock(obj)

  def calling[T](mock: T): T = MockosaurOperations.calling(mock)

  def verifyAllCallsWereMadeTo(mocks: AnyRef*): Unit = MockosaurOperations.verifyAllCallsWereMadeTo(mocks)

  def any[T](implicit tag: ClassTag[T]): T = MockosaurOperations.newWildcardValue[T]

  implicit class TheseAreUsedToSpecifyFunctionCallResults[T](val ref: T) extends AnyVal {

    def returns(toReturn: T): Unit = MockosaurOperations.functionResultReturn(toReturn)

    def returnsSequentially(toReturn: T*): Unit = MockosaurOperations.functionResultReturnSequentially(toReturn)

    def throws(t: Throwable): Unit = MockosaurOperations.functionResultThrow(t)

  }

}
