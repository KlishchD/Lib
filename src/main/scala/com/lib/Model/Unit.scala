package com.lib.Model

case class Unit(id: Int, title: String, year: Int, authors: Seq[String], amount: Int, unitType: String) extends Writable {
  def getStringForUploading: String = {
    id + ", " + title + ", " + year + ", " +
      authors.fold("")((a : String, b : String) => a + " | " + b).substring(3) + ", " + amount +
      ", " +  unitType
  }

  override def toString: String = title + " " + year + " " +
    authors.fold("")((a : String, b : String) => a + " | " + b).substring(3) + " " + unitType
}