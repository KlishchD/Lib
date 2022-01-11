package com.lib.DataManagment

import com.lib.Model.Writable

import java.io.{File, FileWriter}
import scala.collection.mutable

object DataUpdater {
  def update(writable: mutable.Seq[Writable], filePath: String): scala.Unit = {
    val writer = new FileWriter(new File(filePath))
    writable.foreach(x => writer.write(x.getStringForUploading + "\n"))
    writer.close()
  }
}
