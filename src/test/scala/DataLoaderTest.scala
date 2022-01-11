import com.lib.DataManagment.DataLoader
import com.lib.Model.{Unit, User}
import org.scalatest.{FunSuite, PrivateMethodTester}

import scala.collection.mutable

class DataLoaderTest extends FunSuite with PrivateMethodTester{
  test("DataLoader.loadUsers") {
    val expected = Seq(User("user0", "ddd1", 10, mutable.Seq(1), blackList = false, "librarian"))

    val loadUsers = PrivateMethod[Seq[User]]('loadUsers)
    val actual = DataLoader invokePrivate loadUsers("src/test/resources/users.csv")

    assert(expected == actual)
  }
  test("DataLoader.loadUnits") {
    val expected = Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book"))

    val loadUnits = PrivateMethod[Seq[Unit]](Symbol("loadUnits"))
    val actual = DataLoader invokePrivate loadUnits("src/test/resources/units.csv")

    assert(expected == actual)
  }
}

