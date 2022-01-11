package com.lib.DataManagment

import com.lib.Exceptions._
import com.lib.Model.{Author, Unit, User}

import scala.collection.mutable
import scala.math.Ordering.Implicits.seqOrdering


case class DataManager(private var _users: mutable.Seq[User], private var _units: mutable.Seq[Unit], private var _authors: mutable.Seq[Author]) {

  def authors: mutable.Seq[Author] = _authors

  def getUser(username: String): Option[User] = _users.find(_.username == username)

  def addUser(user: User): scala.Unit = {
    if (_users.map(_.username).contains(user.username)) throw new UserAlreadyExistsException
    _users = _users :+ user
  }

  def removeUser(username: String): scala.Unit = {
    if (!_users.exists(_.username == username)) throw new NoSuchUserException
    _users = _users.filter(_.username != username)
  }

  def takeUnitByTitle(username: String, title: String): scala.Unit = {
    val unitOption = _units.find(_.title == title)
    if (unitOption.isEmpty) throw new NoSuchUnitException

    takeUnitById(username, unitOption.get.id)
  }

  def takeUnitsByIds(username: String, unitIds: mutable.Seq[Int]): scala.Unit = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException
    if (unitIds.exists(id => !_units.map(_.id).contains(id))) throw new NoSuchUnitException
    val user = _users(userIndexOption.get)
    if (user.unitsId.size + unitIds.size > user.maxUnitsNumber) throw new AttemptToExceedUnitLimitException
    if (user.unitsId.exists(id => unitIds.contains(id))) throw new AttemptToTakeSecondCopyException
    if (units.exists(unit => unitIds.contains(unit.id) && unit.number == 0)) throw new OutOfUnitCopiesException

    _units.indices.zip(_units.map(_.id)).filter(pair => unitIds.contains(pair._2)).map(_._1).foreach(unitIndex => takeUnit(unitIndex, userIndexOption.get))
  }

  def takeUnitById(username: String, unitId: Int): scala.Unit = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException

    val user = _users(userIndexOption.get)
    if (user.unitsId.size == user.maxUnitsNumber) throw new AttemptToExceedUnitLimitException

    val unitIndexOption = getUnitIndexFromId(unitId)
    if (unitIndexOption.isEmpty) throw new NoSuchUnitException

    val unit = _units(unitIndexOption.get)
    if (unit.number == 0) throw new OutOfUnitCopiesException
    if (user.unitsId.contains(unit.id)) throw new AttemptToTakeSecondCopyException

    takeUnit(userIndexOption.get, unitIndexOption.get)
  }

  def returnUnits(username: String, unitsId: mutable.Seq[Int]): scala.Unit = {
    if (!_users.exists(_.username == username)) throw new NoSuchUserException

    removeUserUnits(username, unitsId)
    unitsId.foreach(_units(_).number += 1)
  }

  private def removeUserUnits(username: String, unitsId: mutable.Seq[Int]): scala.Unit = {
    val index = getUserIndexFromUsername(username).get
    _users(index).unitsId = _users(index).unitsId.filter(id => !unitsId.contains(id))
  }

  def logIn(username: String, password: String): Option[User] = _users.find(user => user.username == username && user.password == password)

  def getUserUnits(username: String): mutable.Seq[Unit] = {
    if (!_users.exists(_.username == username)) throw new NoSuchUserException
    val unitsIds = _users(getUserIndexFromUsername(username).get).unitsId
    _units.filter(x => unitsIds.contains(x.id))
  }

  def updateUserMaxUnitsNumber(username: String, maxUnitsNumber: Int): scala.Unit = {
    if (!_users.exists(_.username == username)) throw new NoSuchUserException
    val index = getUserIndexFromUsername(username).get
    if (_users(index).maxUnitsNumber > maxUnitsNumber) throw new IllegalMaxUnitsNumberValueException
    _users(index).maxUnitsNumber = maxUnitsNumber
  }

  def getUserMaxUnitsNumber(username: String): Int = {
    val userOption = _users.find(_.username == username)
    if (userOption.isEmpty) throw new NoSuchUserException
    userOption.get.maxUnitsNumber
  }

  def addUnit(unit: Unit): scala.Unit = {
    if (_units.map(_.id).contains(unit.id)) throw new UnitAlreadyExistsException
    _units = _units :+ unit
  }

  def removeUnit(unitId: Int): scala.Unit = {
    if (!_units.exists(_.id == unitId)) throw new NoSuchUnitException
    _units = _units.filter(_.id != unitId)
  }

  def addUserInBlackList(username: String): scala.Unit = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException
    val userIndex = userIndexOption.get
    if (_users(userIndex).blacklist) throw new UserIsAlreadyInBlackListException
    _users(userIndex).blacklist = true
  }

  def blackList: mutable.Seq[User] = _users.filter(_.blacklist)

  def removeUserFromBlackList(username: String): scala.Unit = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException
    val userIndex = userIndexOption.get
    if (!_users(userIndex).blacklist) throw new UserIsNotInBlackListException
    _users(userIndex).blacklist = false
  }

  private def getUserIndexFromUsername(username: String): Option[Int] =
    users.indices.zip(users).find(_._2.username == username).map(_._1)

  def users: mutable.Seq[User] = _users

  def isUserInBlackList(username: String): Boolean = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException
    _users(userIndexOption.get).blacklist
  }

  def addUnitsToUser(username: String, unitsId: mutable.Seq[Int]): scala.Unit = {
    val userIndexOption = getUserIndexFromUsername(username)
    if (userIndexOption.isEmpty) throw new NoSuchUserException
    if (!unitsId.forall(_units.map(_.id).contains(_))) throw new NoSuchUnitException
    _users(userIndexOption.get).unitsId = _users(userIndexOption.get).unitsId ++ unitsId
  }

  def addUnitsCopies(unitId: Int, number: Int): scala.Unit = {
    if (!_units.map(_.id).contains(unitId)) throw new NoSuchUnitException
    if (number <= 0) throw new IllegalArgumentException

    _units(getUnitIndexFromId(unitId).get).number += number
  }

  private def getUnitIndexFromId(id: Int): Option[Int] =
    units.indices.zip(units).find(_._2.id == id).map(_._1)

  def units: mutable.Seq[Unit] = _units

  def removeAllCopies(unitId: Int): scala.Unit = removeUnitsCopies(unitId, getUnit(unitId).get.number)

  def getUnit(unitId: Int): Option[Unit] = _units.find(_.id == unitId)

  def removeUnitsCopies(unitId: Int, number: Int): scala.Unit = {
    val unitIdOption = getUnitIndexFromId(unitId)
    if (unitIdOption.isEmpty) throw new NoSuchUnitException
    if (number <= 0 || _units(unitIdOption.get).number < number) throw new IllegalArgumentException

    _units(unitIdOption.get).number -= number
  }

  def getDeficitUnits: mutable.Seq[Unit] = _units.filter(_.number == 0)

  def getAvailableUnits: mutable.Seq[Unit] = _units.filter(_.number != 0)

  def getUnitsByTitle(title: String): mutable.Seq[Unit] = _units.filter(_.title.matches("(?i).*" + title + ".*")).sorted(Ordering.by[Unit, mutable.Seq[String]](_.authors))

  def getUnitsByYear(year: Int): mutable.Seq[Unit] = _units.filter(_.year == year).sorted(Ordering.by[Unit, String](_.title.toLowerCase()))

  def getUnitsByAuthorName(authorName: String): mutable.Seq[Unit] = _units.filter(_.authors.exists(_.matches("(?i).*" + authorName + ".*"))).sorted(Ordering.by[Unit, Int](_.year))

  private def takeUnit(userIndex: Int, unitIndex: Int): scala.Unit = {
    _units(unitIndex).number -= 1
    _users(userIndex).unitsId = _users(userIndex).unitsId.appended(_units(unitIndex).id)
  }

}
