// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


package daqcore.util

import java.io.File
import java.io.IOException


object fileops {
  class FileOps(val file:File) {
    def copyTo(to: File) : Unit = {
      import java.io.{FileInputStream, FileOutputStream}
      import java.nio.channels.FileChannel
      
      if (! to.exists) { to.createNewFile() }

      var src: Option[FileChannel] = None
      var dst: Option[FileChannel] = None
      try {
        src = Option(new FileInputStream(file).getChannel())
        dst = Option(new FileOutputStream(to).getChannel())
        val count = dst.get.transferFrom(src.get, 0, src.get.size())
        if (count != src.get.size()) throw new IOException("File size mismatch after copying")
      }
      finally {
        src map (_.close())
        dst map (_.close())
      }
    }

    def moveTo(to: File) : Unit = {
      val renameOK = file.renameTo(to)
      if (!renameOK) {
        copyTo(to)
        val deleted = file.delete()
        if (!deleted) throw new IOException("Could not delete file \"!" + file.getPath + "\"")
      }
    }

    def writer : java.io.Writer = {
      import java.io.{OutputStreamWriter,FileOutputStream,File}

      val stream = new java.io.FileOutputStream(file)
      val writer = new java.io.OutputStreamWriter(stream, "UTF-8")

      writer
    }
    
    def write(s: String) : Unit = {
      val w = writer
      w.write(s); w.close()
    }
    
    def read : String = {
      io.Source.fromFile(file).mkString
    }
    
    def getLines : Iterator[String] = {
      io.Source.fromFile(file).getLines()
    }
    
    def /(path: String) : File = new File(file.getPath + File.separator + path)
  }


  implicit def apply(file: File) = new FileOps(file)
  implicit def apply(path: String) = new FileOps(new File(path))

  def homeDir = new java.io.File(System.getProperty("user.home"))
}
