package com.lib.DataManagment

import com.lib.Exceptions._
import com.lib.Model.{Author, Unit, User}

import scala.math.Ordering.Implicits.seqOrdering


case class DataManager(private var users: Seq[User], private var units: Seq[Unit], private var authors: Seq[Author]) {

  def getUsers: Seq[User] = users

  def getUnits: Seq[Unit] = units

  def getAuthors: Seq[Author] = authors

  def getBlackList: Seq[User] = users.filter(_.blackList)

  def getUser(username: String): Option[User] = users.find(_.username == username)

  def getUnit(unitId: Int): Option[Unit] = units.find(_.id == unitId)

  def takeUnit(username: String, title: String): scala.Unit = {
    val userOption = users.find(_.username == username)
    if (userOption.isEmpty) throw new NoSuchUserException

    val user = userOption.get

    val unitOption = units.find(_.title == title)
    if (unitOption.isEmpty) throw new NoSuchUnitException

    val unit = unitOption.get

    if (user.unitsId.size == user.maxUnitsNumber) throw new AttemptToExceedUnitLimitException
    if (user.unitsId.contains(unit.id)) throw new AttemptToTakeSecondCopyException
    if (unit.amount == 0) throw new OutOfUnitCopiesException

    units = units.map { x =>
      if (x == unit) x.copy(amount = x.amount - 1) else x
    }
    users = users.map { x =>
      if (x == user) x.copy(unitsId = x.unitsId :+ unit.id) else x
    }
  }

  def returnUnit(username: String, unitsId: Seq[Int]): scala.Unit = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException

    removeUserUnits(username, unitsId)
    units = units.map { unit =>
      if (unitsId.contains(unit.id)) unit.copy(amount = unit.amount + 1) else unit
    }
  }

  private def removeUserUnits(username: String, unitsId: Seq[Int]): scala.Unit = users = users.map { x =>
    if (x.username == username) x.copy(unitsId = x.unitsId.filter(!unitsId.contains(_))) else x
  }

  def logIn(username: String, password: String): Option[User] = users.find(user => user.username == username && user.password == password)

  def getUserUnits(username: String): Seq[Unit] = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException
    val unitsIds = users.filter(_.username == username).flatMap(x => x.unitsId)
    units.filter(x => unitsIds.contains(x.id))
  }

  def updateUserMaxUnitsNumber(username: String, maxUnitsNumber: Int): scala.Unit = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException
    users = users.map { x =>
      if (x.username == username) {
        if (x.maxUnitsNumber > maxUnitsNumber) throw new IllegalMaxUnitsNumberValueException
        x.copy(maxUnitsNumber = maxUnitsNumber)
      } else x
    }
  }

  def getUserMaxUnitsNumber(username: String): Int = {
    val userOption = users.find(_.username == username)
    if (userOption.isEmpty) throw new NoSuchUserException
    userOption.get.maxUnitsNumber
  }

  def addUser(user: User): scala.Unit = {
    if (users.map(_.username).contains(user.username)) throw new UserAlreadyExistsException
    users = users :+ user
  }

  def removeUser(username: String): scala.Unit = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException
    users = users.filter(_.username != username)
  }

  def addUnit(unit: Unit): scala.Unit = {
    if (units.map(_.id).contains(unit.id)) throw new UnitAlreadyExistsException
    units = units :+ unit
  }

  def removeUnit(unitId: Int): scala.Unit = {
    if (!units.exists(_.id == unitId)) throw new NoSuchUnitException
    units = units.filter(_.id != unitId)
  }

  def addUserInBlackList(username: String): scala.Unit = {
    val userOption = users.find(_.username == username)
    if (userOption.isEmpty) throw new NoSuchUserException
    if (userOption.get.blackList) throw new UserIsAlreadyInBlackListException
    users = users.map { x =>
      if (x.username == username) x.copy(blackList = true) else x
    }
  }

  def removeUserFromBlackList(username: String): scala.Unit = users = users.map { x =>
    val userOption = users.find(_.username == username)
    if (userOption.isEmpty) throw new NoSuchUserException
    if (!userOption.get.blackList) throw new UserIsNotInBlackListException
    if (x.username == username) x.copy(blackList = false) else x
  }

  def isUserInBlackList(username: String): Boolean = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException
    users.filter(x => x.username == username).head.blackList
  }

  def addUnitsToUser(username: String, unitsId: Seq[Int]): scala.Unit = {
    if (!users.exists(_.username == username)) throw new NoSuchUserException
    if (!unitsId.forall(units.map(_.id).contains(_))) throw new NoSuchUnitException
    users = users.map { x =>
      if (x.username == username) x.copy(unitsId = x.unitsId ++ unitsId) else x
    }
  }

  def addCopies(unitId: Int, number: Int): scala.Unit = {
    if (!units.map(_.id).contains(unitId)) throw new NoSuchUnitException
    if (number <= 0) throw new IllegalArgumentException

    units = units.map { x =>
      if (x.id == unitId) x.copy(amount = x.amount + number) else x
    }
  }

  def removeCopies(unitId: Int, number: Int): scala.Unit = units = {
    if (!units.map(_.id).contains(unitId)) throw new NoSuchUnitException
    if (number <= 0) throw new IllegalArgumentException

    units.map { x =>
      if (x.id == unitId) x.copy(amount = x.amount - number) else x
    }
  }

  def getDeficitUnits: Seq[Unit] = units.filter(_.amount == 0)

  def getAvailableUnits: Seq[Unit] = units.filter(_.amount != 0)

  def getUnitsByTitle(title: String): Seq[Unit] = units.filter(_.title.matches("(?i).*" + title + ".*")).sorted(Ordering.by[Unit, Seq[String]](_.authors))

  def getUnitsByYear(year: Int): Seq[Unit] = units.filter(_.year == year).sorted(Ordering.by[Unit, String](_.title.toLowerCase()))

  def getUnitsByAuthorName(authorName: String): Seq[Unit] = units.filter(_.authors.exists(_.matches("(?i).*" + authorName + ".*"))).sorted(Ordering.by[Unit, Int](_.year))

}
