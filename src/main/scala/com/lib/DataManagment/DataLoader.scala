package com.lib.DataManagment

import com.lib.Model.{Author, Unit, User}

import scala.collection.mutable
import scala.io.Source

object DataLoader {
  def loadData(usersFilePath: String, unitsFilePath: String): DataManager = {
    val units = loadUnits(unitsFilePath)
    DataManager(loadUsers(usersFilePath), units, getAuthors(units))
  }

  private def loadUsers(filePath: String): mutable.Seq[User] = {
    val source = Source.fromFile(filePath)

    val users = for {
      line <- source.getLines().toSeq
      values = line.split(",").map(_.trim)
    } yield User(values(0), values(1), values(2).toInt, parseUnits(values(3)), values(4).toBoolean, values(5).toLowerCase())

    source.close()
    mutable.Seq(users:_*)
  }
  private def parseUnits(line: String): mutable.Seq[Int] = if (line.trim == "") mutable.Seq()
  else line.split("\\|").map(_.trim.toInt)

  private def loadUnits(filePath: String): mutable.Seq[Unit] = {
    val source = Source.fromFile(filePath)

    val units = for {
      line <- source.getLines().toSeq
      values = line.split(",").map(_.trim)
    } yield Unit(values(0).toInt, values(1), values(2).toInt, parseAuthors(values(3)), values(4).toInt, values(5).toLowerCase())

    source.close()
    mutable.Seq(units:_*)
  }
  private def parseAuthors(authors: String): mutable.Seq[String] = authors.split("\\|").map(_.trim)

  private def getAuthors(units: mutable.Seq[Unit]): mutable.Seq[Author] = {
    for {
      name <- units.flatMap(_.authors)
      createdUnits = units.filter(_.authors.contains(name)).map(x => x.id)
    } yield Author(name, createdUnits)
  }

}
