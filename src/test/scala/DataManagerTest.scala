import com.lib.DataManagment.{DataManager, IllegalMaxUnitsNumberValueException}
import com.lib.Exceptions.{AttemptToTakeSecondCopyException, NoSuchUnitException, NoSuchUserException, OutOfUnitCopiesException}
import com.lib.Model.{Author, Unit, User}
import org.scalatest.{BeforeAndAfter, FunSuite, PrivateMethodTester}

class DataManagerTest extends FunSuite with BeforeAndAfter with PrivateMethodTester {
  test("getUser_WrongUsername_ReturnsNone") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.getUser("asda").isEmpty)
  }
  test("getUser_CorrectUsername_ReturnsUser") {
    val user = User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")
    val dataManager = DataManager(Seq(user),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.getUser("user0").get == user)
  }
  test("takeUnit_wrongTitle_NoSuchUnitException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUnitException] {
      dataManager.takeUnit("user0", "Power of random :)")
    }
  }
  test("takeUnit_UserAlreadyHasUnit_AttemptToTakeSecondCopyException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[AttemptToTakeSecondCopyException] {
      dataManager.takeUnit("user0", "Geek Heresy")
    }
  }
  test("takeUnit_LibHaveNoCopies_OutOfUnitCopiesException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 0, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[OutOfUnitCopiesException] {
      dataManager.takeUnit("user0", "Geek Heresy")
    }
  }
  test("takeUnit_CorrectData_UserTakesUnit") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.takeUnit("user0", "Geek Heresy")
    assert(dataManager.getUserUnits("user0").map(_.id).contains(0))
  }
  test("takeUnit_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.takeUnit("user034", "Geek Heresy"))
  }
  test("returnUnit_CorrectUserName_UserReturnsUnit") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.returnUnit("user0", Seq(0, 1))
    assert(dataManager.getUserUnits("user0").isEmpty)
  }
  test("returnUnit_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.returnUnit("user42340", Seq(0, 1)))
  }
  test("logIn_WrongUserData_ReturnsNone") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.logIn("user1", "ddd1").isEmpty)
  }
  test("logIn_CorrectUserData_ReturnsUser") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.logIn("user0", "ddd1").nonEmpty)
  }
  test("removeUserUnits_RemovesUnits") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    val removeUserUnits = PrivateMethod[scala.Unit](Symbol("removeUserUnits"))
    dataManager invokePrivate removeUserUnits("user0", Seq(0))
    assert(!dataManager.getUserUnits("user0").exists(_.id == 0))
  }
  test("getUserUnits_CorrectUsername_ReturnsUnitSequence") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    val expected = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book"))
    val actual = dataManager.getUserUnits("user0")
    assert(expected == actual)
  }
  test("getUserUnits_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.getUserUnits("user42340"))
  }
  test("updateUserMaxUnitsNumber_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.updateUserMaxUnitsNumber("fskfjsndvkl", 3))
  }
  test("updateUserMaxUnitsNumber_newMaxUnitsNumberLesserThanCurrent_IllegalMaxUnitsNumberValueException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[IllegalMaxUnitsNumberValueException](dataManager.updateUserMaxUnitsNumber("user0", 3))
  }
  test("updateUserMaxUnitsNumber_CorrectData_ChangesMaxUnitsNumber") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.updateUserMaxUnitsNumber("user0", 100)
    assert(dataManager.getUser("user0").get.maxUnitsNumber == 100)
  }
  test("getUserMaxUnitsNumber_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException] (dataManager.getUserMaxUnitsNumber("aflndgfakjf"))
  }
  test("getUserMaxUnitsNumber_CorrectUsername_ReturnsUserMaxUnitNumber") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.getUserMaxUnitsNumber("user0") == 10)
  }

}
