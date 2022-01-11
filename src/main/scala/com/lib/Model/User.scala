package com.lib.Model

import scala.collection.mutable


class User(private var _username: String, private var _password: String, private var _maxUnitsNumber: Int,
           private var _unitsId: mutable.Seq[Int], private var _blacklist: Boolean, private var _userType: String) extends Writable {

  def username: String = _username

  def username_=(username: String): scala.Unit = _username = username


  def password: String = _password

  def password_=(password: String): scala.Unit = _password = password

  def maxUnitsNumber: Int = _maxUnitsNumber

  def maxUnitsNumber_=(maxUnitsNumber: Int): scala.Unit = _maxUnitsNumber = maxUnitsNumber


  def unitsId: mutable.Seq[Int] = _unitsId

  def unitsId_=(unitsId: mutable.Seq[Int]): scala.Unit = _unitsId = unitsId

  def blacklist: Boolean = _blacklist

  def blacklist_=(blacklist: Boolean): scala.Unit = _blacklist = blacklist

  def userType: String = _userType

  def userType_=(userType: String): scala.Unit = _userType = userType

  def getStringForUploading: String = {
    _username + "," + _password + "," + _maxUnitsNumber + "," +
      _unitsId.map(x => x.toString).mkString("|") + "," +
      _blacklist + "," + _userType
  }

  override def equals(obj: Any): Boolean = obj match {
    case User(username) => username == _username
    case _ => false
  }
}

object User {
  def apply(username: String, password: String, maxUnitsNumber: Int, unitsId: mutable.Seq[Int], blackList: Boolean, userType: String): User =
    new User(username, password, maxUnitsNumber, unitsId, blackList, userType)

  def unapply(user: User): Option[String] = Some(user.username)
}