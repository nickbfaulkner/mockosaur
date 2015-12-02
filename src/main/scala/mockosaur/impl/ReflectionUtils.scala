package mockosaur.impl

import java.lang.reflect.Method

import scala.util.{Failure, Success, Try}

object ReflectionUtils {

  // takes the class of a value class and returns method references that have explicitly been added or
  // are the accessor for the underlying value
  def getValueClassDeclaredFunctions(clazz: Class[_]): Set[Method] = {
    clazz.getDeclaredMethods.toSet.filter(m => {
      val isStandardFunction = Set("equals",
                                   "toString",
                                   "hashCode",
                                   "copy",
                                   "canEqual",
                                   "productPrefix",
                                   "productArity",
                                   "productElement",
                                   "productIterator").contains(m.getName)
      (!isStandardFunction) && (!m.getName.contains("copy$default"))
    })
  }

  def unwrapValueClass(value: Any): Try[AnyRef] = {
    // even though we type checked, the return type does not match the expectation
    // assuming it's a value class, let's get the underlying value

    val constructors = value.getClass.getConstructors
    if (constructors.length != 1) {
      return Failure(new RuntimeException("Expected value class to have one constructor"))
    }

    val constructor = constructors.head
    val constructorParams = constructor.getParameters

    if (constructorParams.length != 1) {
      return Failure(new RuntimeException("Expected value class constructor to take one argument"))
    }

    val expectedFieldType = constructorParams.head.getType

    val underlyingValueAccessor = {
      // we can't rely on there being a single accessor with the right return
      // type so we have to create one and find a way to get the zombie back

      val zombie = MockBuilder.buildZombie(expectedFieldType).asInstanceOf[AnyRef]
      val instance = constructor.newInstance(zombie)

      val methods = ReflectionUtils.getValueClassDeclaredFunctions(value.getClass)

      methods.filter(_.getParameterCount == 0).find(m => m.invoke(instance) == zombie) match {
        case Some(method) => Success(method)
        case None         => Failure(new RuntimeException("Could not find accessor method for underlying value"))
      }
    }

    underlyingValueAccessor.flatMap(method => Try(method.invoke(value)))
  }
}
