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

import daqcore.util


object fileops {
  implicit def toFileOps(file: File) = new FileOps(file)
  implicit def toFileOps(path: String) = new FileOps(new File(path))

  class FileOps(val file:File) {
    def copyTo(that: File) : Unit = {
      if (! that.exists) that.createNewFile()

      using (
        this.getIStream.getChannel(),
        that.getOStream.getChannel()
      ) { (src, dst) => {
        val count = dst.transferFrom(src, 0, src.size())
        if (count != src.size()) throw new IOException("File size mismatch after copying")
      } }
    }

    def moveTo(that: File) : Unit = {
      val renameOK = file.renameTo(that)
      if (!renameOK) {
        this copyTo that
        val deleted = file.delete()
        if (!deleted) throw new IOException("Could not delete file \"!" + file.getPath + "\"")
      }
    }

    def getIStream : java.io.FileInputStream =
      new java.io.FileInputStream(file)

    def getOStream : java.io.FileOutputStream =
      new java.io.FileOutputStream(file)

    def getWriter : java.io.Writer =
      new java.io.OutputStreamWriter(this.getOStream, "UTF-8")
    
    def write(s: String) : Unit =
      using (this.getWriter) { _.write(s) }

    def write(nodes: xml.NodeSeq) : Unit = {
      using (this.getWriter) { writer => {
        writer.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
        writer.write("\n<!-- This file is generated automatically. Do not edit! -->\n\n")
        
        val elem = (nodes flatMap { case elem:xml.Elem => Some(elem); case _ => None } head)
        if (elem.namespace == "http://www.w3.org/1999/xhtml")
          writer.write("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
        
        val xmlString = new xml.PrettyPrinter(200, 4).formatNodes(nodes)
        writer.write(xmlString)
      } }
    }

    def writeBytes(data: Seq[Byte]): Unit = {
      using(this.getOStream) { oStream =>
        oStream.write(data.toArray)
        oStream.flush()
      }
    }
    
    def getSource : scala.io.Source = scala.io.Source.fromFile(file)

    def read : String =
      using (this.getSource) { _.mkString }
      
    def readBytes: IndexedSeq[Byte] = {
      val fileSize = file.length
      if (fileSize > Int.MaxValue) throw new IllegalArgumentException("File " + file + "to large to read into array")
      val arraySize = fileSize.toInt
      val bytes = Array.ofDim[Byte](arraySize)
      var offset = 0
      using(this.getIStream) { iStream =>
        while (offset < bytes.size) {
          val nRead = iStream.read(bytes, offset, bytes.size - offset)
          offset += nRead;
        }
      }
      bytes.toSeq.asInstanceOf[IndexedSeq[Byte]]
    }

    def readLines : List[String] =
      using (this.getSource) { _.getLines.toList }

    def readLineSeq : IndexedSeq[String] =
      using (this.getSource) { _.getLines.toSeq.asInstanceOf[IndexedSeq[String]] }
    
    def readXML: scala.xml.Elem = xml.XML.loadFile(file)
    
    def splitPath: (Option[String], String, Option[String]) = {
      val dir = Option(file.getParent)
      val name = file.getName
      val (basename, ext) = name.lastIndexWhere {_ == '.'} match {
        case 0 => (name, None)
        case idx: Int if (idx >= 0) => { val (b, e) = name.splitAt(idx); (b, Some(e.drop(1))) }
        case idx: Int => (name, None)
      }
      (dir, basename, ext)
    }
    
    def relativeTo(that: File) = {
      def takeSame[T](a:Seq[T], b:Seq[T]) : Seq[T] = {
        def takeSameImpl[T](s:Seq[(T,T)], r:List[T] = Nil) : List [T] = if (s.isEmpty || (s.head._1 != s.head._2)) r; else takeSameImpl(s.tail, s.head._1::r)
        takeSameImpl(a zip b).reverse
      }
    
      val thisAbs = file.getAbsolutePath.split(File.separator)
      val thatAbs = that.getAbsolutePath.split(File.separator)
      val common = takeSame(thisAbs, thatAbs)
      if (common.isEmpty) new File(file.getAbsolutePath)
      else {
        val thisRest = thisAbs.drop(common.size)
        val thatRest = thatAbs.drop(common.size)
        val thisRestString =
          if (thisRest.isEmpty) ""
          else thisRest.reduceLeft(_ + File.separator + _)
        //tools.nsc.Interpreter.breakIf(true, "thisRest"->thisRest, "thatRest" -> thatRest)
        val newPath = thatRest.foldLeft(thisRestString)
          { (p,r) => ".." + File.separator + p }
        new File(if (!newPath.isEmpty) newPath else ".")
      }
    }
    
    def dropParent(that: File) = {
      val thisAbs = file.getAbsolutePath.split(File.separator)
      val thatAbs = that.getAbsolutePath.split(File.separator)
      if(!(thisAbs startsWith thatAbs))
        throw new IllegalArgumentException("Path \"" + that.getAbsolutePath + "\" is not parent of path \"" + file.getAbsolutePath + "\"")
      val thisRel = thisAbs.drop(thatAbs.size)
      if (thisRel.isEmpty) new File(".")
      else new File(thisRel.reduceLeft(_ + File.separator + _))
    }
    
    def isWithin(that: File) = {
      val thisAbs = file.getAbsolutePath.split(File.separator)
      val thatAbs = that.getAbsolutePath.split(File.separator)
      thisAbs startsWith thatAbs
    }

    def isSymlink = {
      val cfile = Option(file.getParentFile) match {
        case Some(parent) => new File(parent.getCanonicalFile, file.getName)
        case None => file
      }
      cfile.getCanonicalFile != cfile.getAbsoluteFile
    }
    
    def /(path: String) : File = new File(file.getPath + File.separator + path)
  }
  
  val tmpDir = new java.io.File(System.getProperty("java.io.tmpdir"))

  val homeDir = new java.io.File(System.getProperty("user.home"))

  val currDir = new java.io.File(".")
}
