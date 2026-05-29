import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import util.*

class ObserverTest extends AnyWordSpec with Matchers {

  "Observer system" should {

    "add and notify observers" in {
      val observable = new Observable {}
      var updated = false

      val observer = new Observer {
        override def update(): Unit =
          updated = true
      }

      observable.add(observer)
      observable.notifyObservers()

      updated shouldBe true
    }

    "remove observers correctly" in {
      val observable = new Observable {}
      var updates = 0

      val observer = new Observer {
        override def update(): Unit =
          updates += 1
      }

      observable.add(observer)
      observable.remove(observer)
      observable.notifyObservers()

      updates shouldBe 0
    }
  }
}
