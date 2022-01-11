package com.lib.Model

import scala.collection.mutable

class Unit(private var _id: Int, private var _title: String, private var _year: Int, private var _authors: mutable.Seq[String],
           private var _number: Int, private var _unitType: String) extends Writable {

  def id: Int = _id

  def id_=(id: Int): scala.Unit = _id = id

  def title: String = _title

  def title_=(title: String): scala.Unit = _title = title


  def year: Int = _year

  def year_=(year: Int): scala.Unit = _year = year

  def authors: mutable.Seq[String] = _authors

  def authors_=(authors: mutable.Seq[Author]): mutable.Seq[String] = _authors


  def number: Int = _number

  def number_=(number: Int): scala.Unit = _number = number

  def unitType: String = _unitType

  def unitType_=(unitType: String): scala.Unit = _unitType = unitType

  def getStringForUploading: String = {
    _id + "," + _title + "," + _year + "," +
      _authors.mkString("|") + "," + _number +
      "," + _unitType
  }

  override def toString: String = _title + " " + _year + " " +
    _authors.mkString("|") + " " + _unitType

  override def equals(obj: Any): Boolean = obj match {
    case Unit(id) => id == _id
    case _ => false
  }
}

object Unit {
  def apply(id: Int, title: String, year: Int, authors: mutable.Seq[String], number: Int, unitType: String): Unit = {
    new Unit(id, title, year, authors, number, unitType)
  }

  def unapply(unit: Unit): Option[Int] = Some(unit.id)

}