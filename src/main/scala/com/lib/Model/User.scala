package com.lib.Model


case class User(username: String, password: String, maxUnitsNumber: Int, unitsId: Seq[Int], blackList: Boolean, userType: String) extends Writable {
  def getStringForUploading: String = {
    username + ", " + password + ", " + maxUnitsNumber + ", " +
      unitsId.map(x => x.toString).fold("")((a : String, b : String) => a + " | " + b).substring(3) + ", " +
      blackList + ", " + userType
  }
}