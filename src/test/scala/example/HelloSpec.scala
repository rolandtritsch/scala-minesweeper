package example

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  val expectedGreeting = "Hello World"
  "The Hello object" should s"say ${expectedGreeting}" in {
    Hello.greeting shouldEqual expectedGreeting
  }
}
