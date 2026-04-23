import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class MainTests extends AnyWordSpec {
  "Main" should {
    "start the game loop and allow quitting immediately" in {
      val in = new java.io.ByteArrayInputStream("quit\n".getBytes("UTF-8"))
      val out = new java.io.ByteArrayOutputStream()

      Console.withIn(in) {
        Console.withOut(out) {
          main()
        }
      }

      val printed = out.toString("UTF-8")
      printed.should(include("CUBATRO"))
      printed.should(include("Game stopped by player"))
    }
  }
}
