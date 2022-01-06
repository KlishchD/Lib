class DataLoaderTest {
  import com.lib.DataManagment.DataLoader
  import com.lib.Model.{User, Unit}
  import org.scalatest.{FunSuite, PrivateMethodTester}

  class DataUpdaterTest extends FunSuite with PrivateMethodTester {
    test("DataLoader.loadUsers") {
      val expected = Seq(User("user0", "ddd1", 10, Seq(1), blackList = false, "librarian"))

      val loadUsers = PrivateMethod[Seq[User]](Symbol("loadUsers"))
      val actual = DataLoader invokePrivate loadUsers("src/test/resources/users.csv")

      assert(expected == actual)
    }
    test("DataLoader.loadUnits") {
      val expected = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book"))

      val loadUnits = PrivateMethod[Seq[Unit]](Symbol("loadUnits"))
      val actual = DataLoader invokePrivate loadUnits("src/test/resources/units.csv")

      assert(expected == actual)
    }
  }
}
