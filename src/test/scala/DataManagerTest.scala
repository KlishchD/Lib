import com.lib.DataManagment.DataManager
import com.lib.Exceptions.{AttemptToTakeSecondCopyException, IllegalMaxUnitsNumberValueException, NoSuchUnitException, NoSuchUserException, OutOfUnitCopiesException, UserIsAlreadyInBlackListException, UserIsNotInBlackListException}
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

  test("getUnit_WrongUnitId_ReturnsNone") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.getUnit(19).isEmpty)
  }
  test("getUnit_CorrectUnitId_ReturnsUnit") {
    val unit = Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 3, "Book")
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(unit), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.getUnit(0).get == unit)
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

  test("removeUser_WrongUser_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.removeUser(User("name", "pass", 1, Seq(), blackList = false, "user")))
  }
  test("removeUser_CorrectUser_RemovesUser") {
    val user = User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")
    val dataManager = DataManager(Seq(user), Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.removeUser(dataManager.getUsers.head)
    assert(!dataManager.getUsers.contains(user))
  }

  test("addUserInBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.addUserInBlackList("bbb"))
  }
  test("addUserInBlackList_CorrectUserInBlackList_UserIsAlreadyInBlackListException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = true, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[UserIsAlreadyInBlackListException](dataManager.addUserInBlackList("user0"))
  }
  test("addUserInBlackList_CorrectUserNotInBlackList_PutsUserInBlackList") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.addUserInBlackList("user0")
    assert(dataManager.getUser("user0").get.blackList)
  }

  test("removeUserFromBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = true, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.removeUserFromBlackList("bbb"))
  }
  test("removeUserFromBlackList_CorrectUserNotInBlackList_UserIsNotInBlackListException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[UserIsNotInBlackListException](dataManager.removeUserFromBlackList("user0"))
  }
  test("removeUserFromBlackList_CorrectUsername_RemovesUserFromBlackList") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = true, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.removeUserFromBlackList("user0")
    assert(!dataManager.getUser("user0").get.blackList)
  }

  test("isUserInBlackList_CorrectUserIsInBlackList_ReturnsTrue") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = true, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(dataManager.isUserInBlackList("user0"))
  }
  test("isUserInBlackList_CorrectUserIsNotInBlackList_ReturnsTrue") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assert(!dataManager.isUserInBlackList("user0"))
  }
  test("isUserInBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0, 1), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.isUserInBlackList("ddd"))
  }

  test("addUnitsToUser_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUserException](dataManager.addUnitsToUser("user213121312", Seq(1)))
  }
  test("addUnitsToUser_CorrectUserButWrongUnitsId_NoSuchUnitException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(0), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.addUnitsToUser("user0", Seq(1213)))
  }
  test("addUnitsToUser_CorrectUserAndUnitsId_addUnits") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.addUnitsToUser("user0", Seq(0, 1))
    assert(dataManager.getUser("user0").get.unitsId == Seq(0, 1))
  }

  test("addCopies_WrongUnitId_NoSuchUnitException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.addCopies(100, 20))
  }
  test("addCopies_CorrectUnitIdButWrongNumber_IllegalArgumentException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[IllegalArgumentException](dataManager.addCopies(1, -20))
  }
  test("addCopies_CorrectUnitIdAndNumber_addsCopies") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.addCopies(0, 20)
    assert(dataManager.getUnit(0).get.amount == 21)
  }

  test("removeCopies_WrongUnitId_NoSuchUnitException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.removeCopies(100, 20))
  }
  test("removeCopies_CorrectUnitIdButWrongNumber_IllegalArgumentException") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    assertThrows[IllegalArgumentException](dataManager.removeCopies(1, -20))
  }
  test("removeCopies_CorrectUnitIdAndNumber_removesCopies") {
    val dataManager = DataManager(Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian")),
      Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"), Unit(1, "Geek", 2015, Seq("Kentaro Toyama"), 1, "Book")), Seq(Author("Kentaro Toyama", Seq(0))))
    dataManager.removeCopies(0, 20)
    assert(dataManager.getUnit(0).get.amount == 80)
  }

  test("getDeficitUnits_returnsUnitsInDeficit") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GH", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = Seq(Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"))
    val actual = dataManager.getDeficitUnits

    assert(actual == expected)
  }
  test("getAvailableUnits_returnsAvailableUnits") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GH", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "GH", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val actual = dataManager.getAvailableUnits

    assert(actual == expected)
  }

  test("getUnitsByTitle_MatchesNothing_ReturnsEmptySequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByTitle("Physics").isEmpty)
  }
  test("getUnitsByTitle_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("B"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("A"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("C"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = Seq(Unit(1, "Geek", 2016, Seq("A"), 0, "Book"),
      Unit(0, "Geek Heresy", 2015, Seq("B"), 100, "Book"),
      Unit(1, "GE", 2017, Seq("C"), 12, "Book"))
    assert(dataManager.getUnitsByTitle("Ge").zip(expected).exists(x => x._1 != x._2))
  }

  test("getUnitsByYear_MatchesNothing_ReturnsEmptySequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByYear(2022).isEmpty)
  }
  test("getUnitsByTitle_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = Seq(Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"))
    assert(dataManager.getUnitsByTitle("Ge") == expected)
  }
/*
  test("getUnitsByTitle_MatchesNothing_ReturnsEmptySequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByTitle("Physics").isEmpty)
  }
  test("getUnitsByTitle_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, Seq("Kentaro Toyama"), 121, "Book"))
    val users = Seq(User("user0", "ddd1", 10, Seq(), blackList = false, "librarian"))
    val authors = Seq(Author("Kentaro Toyama", Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = Seq(Unit(0, "Geek Heresy", 2015, Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, Seq("Kentaro Toyama"), 12, "Book"))
    assert(dataManager.getUnitsByTitle("Ge") == expected)
  }*/



}
