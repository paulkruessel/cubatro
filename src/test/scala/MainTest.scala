import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import java.io.ByteArrayInputStream

class MainTest extends AnyWordSpec with Matchers {

  "Main" should {
    "start and quit" in {
      val input = ByteArrayInputStream("quit\n".getBytes("UTF-8"))

      Console.withIn(input) {
        noException should be thrownBy main()
      }
    }
  }
}
