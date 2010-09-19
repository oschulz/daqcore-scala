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

import java.io.{File, InputStream, OutputStream, BufferedInputStream, BufferedOutputStream}
import java.io.{FileInputStream, FileOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}


abstract class Compression {
  def compressed: Boolean
  def inputStream(file: File): InputStream
  def outputStream(file: File): OutputStream
}


case object Uncompressed extends Compression {
  def compressed = false
  
  def inputStream(file: File) =
    new BufferedInputStream(new FileInputStream(file))
  
  def outputStream(file: File) =
    new BufferedOutputStream(new FileOutputStream(file))
}


abstract class Compressed extends Compression {
  def compressed = true
}


case class Gzip(level: Int = java.util.zip.Deflater.DEFAULT_COMPRESSION) extends Compressed {
  def inputStream(file: File) = new GZIPInputStream(new FileInputStream(file), 16384)
  
  def outputStream(file: File) = new GZIPOutputStream(new FileOutputStream(file), 16384) {
    `def`.setLevel(level);
  }
}
