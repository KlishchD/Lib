import com.lib.DataManagment.DataManager
import com.lib.Exceptions._
import com.lib.Model.{Author, Unit, User}
import org.scalatest.{BeforeAndAfter, FunSuite, PrivateMethodTester}

import scala.collection.mutable

class DataManagerTest extends FunSuite with BeforeAndAfter with PrivateMethodTester {

  test("getUser_WrongUsername_ReturnsNone") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.getUser("asda").isEmpty)
  }
  test("getUser_CorrectUsername_ReturnsUser") {
    val user = User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")
    val dataManager = DataManager(mutable.Seq(user),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.getUser("user0").get == user)
  }

  test("getUnit_WrongUnitId_ReturnsNone") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.getUnit(19).isEmpty)
  }
  test("getUnit_CorrectUnitId_ReturnsUnit") {
    val unit = Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(unit), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.getUnit(0).get == unit)
  }

  test("addUser_UserExists_UserAlreadyExistsException") {
    val user = User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")
    val dataManager = DataManager(mutable.Seq(user),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[UserAlreadyExistsException](dataManager.addUser(user))
  }
  test("addUser_UserDoesntExists_AddsUser") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")),
      mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.addUser(User("user1", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian"))
    assert(dataManager.getUser("user1").nonEmpty)
  }

  test("addUnit_UnitExists_UnitAlreadyExistsException") {
    val unit = Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(unit), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[UnitAlreadyExistsException](dataManager.addUnit(unit))
  }
  test("addUnit_UnitDoesntExists_AddsUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.addUnit(Unit(1, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book"))
    assert(dataManager.getUnit(1).nonEmpty)
  }

  test("removeUser_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.removeUser("name"))
  }
  test("removeUser_CorrectUsername_RemovesUser") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")),
      mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.removeUser("user0")
    assert(dataManager.getUser("user0").isEmpty)
  }

  test("removeUnit_WrongUnitId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.removeUnit(213))
  }
  test("removeUnit_CorrectUnitId_RemovesUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")),
      mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.removeUnit(0)
    assert(dataManager.getUnit(0).isEmpty)
  }

  test("takeUnitByTitle_wrongTitle_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException] {
      dataManager.takeUnitByTitle("user0", "Power of random :)")
    }
  }
  test("takeUnitByTitle_UserAlreadyHasUnit_AttemptToTakeSecondCopyException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[AttemptToTakeSecondCopyException] {
      dataManager.takeUnitByTitle("user0", "Geek Heresy")
    }
  }
  test("takeUnitByTitle_LibHaveNoCopies_OutOfUnitCopiesException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 0, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[OutOfUnitCopiesException] {
      dataManager.takeUnitByTitle("user0", "Geek Heresy")
    }
  }
  test("takeUnitByTitle_CorrectData_UserTakesUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.takeUnitByTitle("user0", "Geek Heresy")
    assert(dataManager.getUserUnits("user0").map(_.id).contains(0))
  }
  test("takeUnitByTitle_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.takeUnitByTitle("user034", "Geek Heresy"))
  }

  test("takeUnitById_wrongId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException] {
      dataManager.takeUnitById("user0", 324234)
    }
  }
  test("takeUnitById_UserAlreadyHasUnit_AttemptToTakeSecondCopyException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[AttemptToTakeSecondCopyException] {
      dataManager.takeUnitById("user0", 0)
    }
  }
  test("takeUnitById_LibHaveNoCopies_OutOfUnitCopiesException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 0, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[OutOfUnitCopiesException] {
      dataManager.takeUnitById("user0", 0)
    }
  }
  test("takeUnitById_CorrectData_UserTakesUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.takeUnitById("user0", 0)
    assert(dataManager.getUserUnits("user0").map(_.id).contains(0))
  }
  test("takeUnitById_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.takeUnitById("user034", 0))
  }

  test("takeUnitsByIds_wrongId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException] {
      dataManager.takeUnitsByIds("user0", mutable.Seq(324234))
    }
  }
  test("takeUnitsByIds_UserAlreadyHasUnit_AttemptToTakeSecondCopyException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 3, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[AttemptToTakeSecondCopyException] {
      dataManager.takeUnitsByIds("user0", mutable.Seq(0))
    }
  }
  test("takeUnitsByIds_LibHaveNoCopies_OutOfUnitCopiesException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 0, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[OutOfUnitCopiesException] {
      dataManager.takeUnitsByIds("user0", mutable.Seq(0))
    }
  }
  test("takeUnitsByIds_CorrectData_UserTakesUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.takeUnitsByIds("user0", mutable.Seq(0))
    assert(dataManager.getUserUnits("user0").map(_.id).contains(0))
  }
  test("takeUnitsByIds_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.takeUnitsByIds("user034", mutable.Seq(0)))
  }

  test("returnUnits_CorrectUserName_UserReturnsUnit") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.returnUnits("user0", mutable.Seq(0, 1))
    assert(dataManager.getUserUnits("user0").isEmpty)
  }
  test("returnUnits_WrongUserName_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.returnUnits("user42340", mutable.Seq(0, 1)))
  }

  test("logIn_WrongUserData_ReturnsNone") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.logIn("user1", "ddd1").isEmpty)
  }
  test("logIn_CorrectUserData_ReturnsUser") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.logIn("user0", "ddd1").nonEmpty)
  }

  test("getUserUnits_CorrectUsername_ReturnsUnitSequence") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    val expected = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"))
    val actual = dataManager.getUserUnits("user0")
    assert(expected == actual)
  }
  test("getUserUnits_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.getUserUnits("user42340"))
  }

  test("updateUserMaxUnitsNumber_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.updateUserMaxUnitsNumber("fskfjsndvkl", 3))
  }
  test("updateUserMaxUnitsNumber_newMaxUnitsNumberLesserThanCurrent_IllegalMaxUnitsNumberValueException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[IllegalMaxUnitsNumberValueException](dataManager.updateUserMaxUnitsNumber("user0", 3))
  }
  test("updateUserMaxUnitsNumber_CorrectData_ChangesMaxUnitsNumber") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.updateUserMaxUnitsNumber("user0", 100)
    assert(dataManager.getUser("user0").get.maxUnitsNumber == 100)
  }

  test("getUserMaxUnitsNumber_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.getUserMaxUnitsNumber("aflndgfakjf"))
  }
  test("getUserMaxUnitsNumber_CorrectUsername_ReturnsUserMaxUnitNumber") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.getUserMaxUnitsNumber("user0") == 10)
  }


  test("addUserInBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.addUserInBlackList("bbb"))
  }
  test("addUserInBlackList_CorrectUserInBlackList_UserIsAlreadyInBlackListException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = true, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[UserIsAlreadyInBlackListException](dataManager.addUserInBlackList("user0"))
  }
  test("addUserInBlackList_CorrectUserNotInBlackList_PutsUserInBlackList") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.addUserInBlackList("user0")
    assert(dataManager.getUser("user0").get.blacklist)
  }

  test("removeUserFromBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = true, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.removeUserFromBlackList("bbb"))
  }
  test("removeUserFromBlackList_CorrectUserNotInBlackList_UserIsNotInBlackListException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[UserIsNotInBlackListException](dataManager.removeUserFromBlackList("user0"))
  }
  test("removeUserFromBlackList_CorrectUsername_RemovesUserFromBlackList") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = true, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.removeUserFromBlackList("user0")
    assert(!dataManager.getUser("user0").get.blacklist)
  }

  test("isUserInBlackList_CorrectUserIsInBlackList_ReturnsTrue") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = true, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(dataManager.isUserInBlackList("user0"))
  }
  test("isUserInBlackList_CorrectUserIsNotInBlackList_ReturnsTrue") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assert(!dataManager.isUserInBlackList("user0"))
  }
  test("isUserInBlackList_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0, 1), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.isUserInBlackList("ddd"))
  }

  test("addUnitsToUser_WrongUsername_NoSuchUserException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUserException](dataManager.addUnitsToUser("user213121312", mutable.Seq(1)))
  }
  test("addUnitsToUser_CorrectUserButWrongUnitsId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(0), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.addUnitsToUser("user0", mutable.Seq(1213)))
  }
  test("addUnitsToUser_CorrectUserAndUnitsId_addUnits") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.addUnitsToUser("user0", mutable.Seq(0, 1))
    assert(dataManager.getUser("user0").get.unitsId == mutable.Seq(0, 1))
  }

  test("addUnitsCopies_WrongUnitId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.addUnitsCopies(100, 20))
  }
  test("addUnitsCopies_CorrectUnitIdButWrongNumber_IllegalArgumentException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[IllegalArgumentException](dataManager.addUnitsCopies(1, -20))
  }
  test("addUnitsCopies_CorrectUnitIdAndNumber_addsCopies") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.addUnitsCopies(0, 20)
    assert(dataManager.getUnit(0).get.number == 21)
  }

  test("removeUnitsCopies_WrongUnitId_NoSuchUnitException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[NoSuchUnitException](dataManager.removeUnitsCopies(100, 20))
  }
  test("removeUnitsCopies_CorrectUnitIdButWrongNumber_IllegalArgumentException") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    assertThrows[IllegalArgumentException](dataManager.removeUnitsCopies(1, -20))
  }
  test("removeUnitsCopies_CorrectUnitIdAndNumber_removesCopies") {
    val dataManager = DataManager(mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian")),
      mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"), Unit(1, "Geek", 2015, mutable.Seq("Kentaro Toyama"), 1, "Book")), mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0))))
    dataManager.removeUnitsCopies(0, 20)
    assert(dataManager.getUnit(0).get.number == 80)
  }

  test("getDeficitUnits_returnsUnitsInDeficit") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GH", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = mutable.Seq(Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"))
    val actual = dataManager.getDeficitUnits

    assert(actual == expected)
  }
  test("getAvailableUnits_returnsAvailableUnits") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GH", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "GH", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val actual = dataManager.getAvailableUnits

    assert(actual == expected)
  }

  test("getUnitsByTitle_MatchesNothing_ReturnsEmptySequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByTitle("Physics").isEmpty)
  }
  test("getUnitsByTitle_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("A"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("B"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("C"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("A"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("B"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("C"), 12, "Book"))
    assert(!dataManager.getUnitsByTitle("Ge").zip(expected).exists(x => x._1 != x._2))
  }

  test("getUnitsByYear_MatchesNothing_ReturnsEmptySequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByYear(2022).isEmpty)
  }
  test("getUnitsByYear_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("Kentaro Toyama"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Kentaro Toyama"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = mutable.Seq(Unit(1, "Geek", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"),
      Unit(1, "GK", 2016, mutable.Seq("Kentaro Toyama"), 0, "Book"))
    assert(!dataManager.getUnitsByYear(2016).zip(expected).exists(x => x._1 != x._2))
  }

  test("getUnitsByAuthorName_MatchesNothing_ReturnsEmptySequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Bridget Sheppard"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("Miguel Rivers"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Jayson Dennis"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Stacie Weston"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Stacie Weston"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    assert(dataManager.getUnitsByAuthorName("NONAME").isEmpty)
  }
  test("getUnitsByAuthorName_MatchesSomeUnits_ReturnsMatchedUnitsSequence") {
    val units = mutable.Seq(Unit(0, "Geek Heresy", 2015, mutable.Seq("Kentaro Toyama"), 100, "Book"),
      Unit(1, "Geek", 2016, mutable.Seq("Bridget Sheppard"), 0, "Book"),
      Unit(1, "GE", 2017, mutable.Seq("Miguel Rivers"), 12, "Book"),
      Unit(1, "GM", 2018, mutable.Seq("Jayson Dennis"), 0, "Book"),
      Unit(1, "GK", 2019, mutable.Seq("Stacie Weston"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Stacie Weston"), 121, "Book"))
    val users = mutable.Seq(User("user0", "ddd1", 10, mutable.Seq(), blackList = false, "librarian"))
    val authors = mutable.Seq(Author("Kentaro Toyama", mutable.Seq(0)))

    val dataManager = DataManager(users, units, authors)

    val expected = mutable.Seq(Unit(1, "GK", 2019, mutable.Seq("Stacie Weston"), 0, "Book"),
      Unit(1, "GP", 2020, mutable.Seq("Stacie Weston"), 121, "Book"))

    assert(!dataManager.getUnitsByAuthorName("wes").zip(expected).exists(x => x._1 != x._2))
  }


}
