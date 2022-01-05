package com.lib.DataManagment

import com.lib.Model.{Author, Unit, User}

import scala.io.Source

object DataLoader {
  def loadData(usersFilePath: String, unitsFilePath: String): DataManager = {
    val units = loadUnits(unitsFilePath)
    DataManager(loadUsers(usersFilePath), units, getAuthors(units))
  }

  private def loadUsers(filePath: String): Seq[User] = {
    val source = Source.fromFile(filePath)

    val users = for {
      line <- source.getLines().toSeq
      values = line.split(",").map(_.trim)
    } yield User(values(0), values(1), values(2).toInt, parseUnits(values(3)), values(4).toBoolean, values(5))

    source.close()
    users
  }
  private def parseUnits(line: String): Seq[Int] = line.split("\\|").map(_.trim.toInt)

  private def loadUnits(filePath: String): Seq[Unit] = {
    val source = Source.fromFile(filePath)

    val units = for {
      line <- source.getLines().toSeq
      values = line.split(",").map(_.trim)
    } yield Unit(values(0).toInt, values(1), values(2).toInt, parseAuthors(values(3)), values(4).toInt, values(5))

    source.close()
    units
  }
  private def parseAuthors(authors: String): Seq[String] = authors.split("\\|").map(_.trim)

  private def getAuthors(units: Seq[Unit]): Seq[Author] = {
    for {
      name <- units.flatMap(_.authors)
      createdUnits = units.filter(_.authors.contains(name)).map(x => x.id)
    } yield Author(name, createdUnits)
  }

}
