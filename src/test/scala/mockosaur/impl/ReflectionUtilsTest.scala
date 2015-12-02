package mockosaur.impl

import mockosaur.MockInvocationTest.AnInt
import mockosaur.MockosaurTest
import mockosaur.impl.ReflectionUtilsTest.{AString, AnIntWithMoreFields}

import scala.util.Success

object ReflectionUtilsTest {
  final case class AnInt(theInt: Int) extends AnyVal
  final case class AString(theString: String) extends AnyVal
  final case class AnIntWithMoreFields(theInt: Int) extends AnyVal {
    def another() = theInt * 2
    def andAnother() = theInt.toString
  }
}

class ReflectionUtilsTest extends MockosaurTest {
  "Reflection utils should" - {

    "take the class and strip all but the interesting methods" - {

      "for simple value classes" in {
        val functions = ReflectionUtils.getValueClassDeclaredFunctions(classOf[AnInt])
        functions.map(f => f.getName) shouldBe Set("theInt")
      }

      "for value classes with additional fields" in {
        val functions = ReflectionUtils.getValueClassDeclaredFunctions(classOf[AnIntWithMoreFields])
        functions.map(f => f.getName) shouldBe Set("theInt", "another", "andAnother")
      }

    }

    "unwrap value classes" - {

      "that wrap an Int" in {
        ReflectionUtils.unwrapValueClass(AnInt(88)) shouldBe Success(88)
      }

      "that wrap a String" in {
        ReflectionUtils.unwrapValueClass(AString("88")) shouldBe Success("88")
      }

      "that contain extra functions" in {
        ReflectionUtils.unwrapValueClass(AnIntWithMoreFields(88)) shouldBe Success(88)
      }

    }
  }
}
