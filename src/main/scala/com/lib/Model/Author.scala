package com.lib.Model

import scala.collection.mutable

class Author(private var _name: String, private var _units: mutable.Seq[Int]) {
  def name: String = _name
  def name_=(name: String): scala.Unit = _name = name

  def units: mutable.Seq[Int] = _units
  def units_=(units: mutable.Seq[Int]): scala.Unit = _units = units


  override def equals(obj: Any): Boolean = {
    obj match {
      case Author(name, units) => name == _name && _units == units
      case _ => false
    }
  }
}

object Author {
  def apply(name: String, units: mutable.Seq[Int]): Author = new Author(name, units)
  def unapply(author: Author): Option[(String, mutable.Seq[Int])] = Some(author._name, author._units)

}