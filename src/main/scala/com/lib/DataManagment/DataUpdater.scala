package com.lib.DataManagment

import com.lib.Model.Writable

import java.io.{File, FileWriter}

object DataUpdater {
  def update(writable: Seq[Writable], filePath: String): scala.Unit = {
    val writer = new FileWriter(new File(filePath))
    writable.foreach(x => writer.write(x.getStringForUploading + "\n"))
    writer.close()
  }
}
