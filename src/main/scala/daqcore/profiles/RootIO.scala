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


package daqcore.profiles

import java.net.InetAddress
import java.io.{File => JFile}
import java.util.concurrent.atomic.AtomicInteger

import akka.config.Supervision.{LifeCycle, UndefinedLifeCycle}

import daqcore.util._
import daqcore.actors._
import daqcore.servers._
import daqcore.prot.rootsys._


trait RootIO extends Profile with Closeable {
  import RootIO._
  import RootIO.requests._

  def getIdn(timeout: Long = defaultTimeout): (String, String, String, String) = srv.!>(GetIdn(), timeout)
  
  def openTFile(file: JFile, mode: filemode.Value = filemode.read, timeout: Long = defaultTimeout): TFile = {
    val id = nextId(timeout)
    srv ! OpenTFile(id, file.getPath, mode.toString)
    new TFile(file, this, id, mode, timeout)
  }
  
  def nextId(timeout: Long = defaultTimeout): Int = srv !> GetNextId

  def writeSeq[T <: Product : ClassManifest](file: java.io.File, treeName: String, treeTitle: String, data: Seq[T], mode: filemode.Value = filemode.recreate, timeout: Long = defaultTimeout) = {
    using (openTFile(file, mode)) { tfile =>
      val ttree = tfile.createTTree[T](treeName, treeTitle)
      for { value <- data } {
        ttree += value
      }
    }
  }
}


object RootIO {
  lazy val defaultIO = RootIO()

  def apply(sv: Supervising = defaultSupervisor, lc: LifeCycle = UndefinedLifeCycle): RootIO =
    RootIOServer(sv, lc)

  case object GetNextId extends ActorQuery[Int]

  object requests {
    case class GetIdn() extends RootSysQuery[(String, String, String, String)]
    case class OpenTFile(fileId: Int, name: String, mode: String) extends RootSysCmd
    case class CloseTFile(fileId: Int) extends RootSysCmd
    case class CreateTree(treeId: Int, file: Int, name: String, title: String) extends RootSysCmd
    case class OpenTree(treeId: Int, file: Int, name: String) extends RootSysCmd
    case class CreateBranch(treeId: Int, name: String, typespec: String) extends RootSysCmd
    case class OpenBranch(treeId: Int, name: String, typespec: String) extends RootSysCmd

    case class AddTreeEntry[C : ClassManifest](treeId: Int, content: C) extends RootSysCmd {
      override def writeRequest(out: BasicOutput)(implicit serializerCache: ContentSerCache): Unit = {
        out.writeString(requestName)
        out.writeInt(treeId)
        serializerCache.forType[C].write(out, content)
      }
    }

    case class GetTreeEntry[C : ClassManifest](treeId: Int, index: Int) extends RootSysQuery[C] {
      override def writeRequest(out: BasicOutput)(implicit serializerCache: ContentSerCache): Unit = {
        out.writeString(requestName)
        out.writeInt(treeId)
        out.writeInt(index)
      }
    }
    
    case class GetTreeSize(treeId: Int) extends RootSysQuery[Int]
  }
}



object filemode extends Enumeration {
    val read = Value(1,"read")
    val write = Value(2, "write")
    val recreate = Value(3, "recreate")
}



class TFile(val file: JFile, val io: RootIO, val id: Int, val mode: filemode.Value, val timeout: Long) {
  import TFile._
  import RootIO.requests._
  
  def close(): Unit = {
    io.srv ! CloseTFile(id)
    io.getIdn(timeout) // Make close operation synchronous
  }
  
  def createTTree[T <: Product : ClassManifest](name: String, title: String, mapBranches: PartialFunction[String, String] = stringIdentity): TTree[T] = {
    val treeId = io.nextId(timeout)
    io.srv ! CreateTree(treeId, id, name, title)
    for ((branchName, branchType) <- branchesOf[T](mapBranches)) io.srv ! CreateBranch(treeId, branchName, branchType)
    new TTree[T](this, name, treeId)
  }
  
  def openTTree[T <: Product : ClassManifest](name: String, mapBranches: PartialFunction[String, String] = stringIdentity): TTree[T] = {
    val treeId = io.nextId(timeout)
    io.srv ! OpenTree(treeId, id, name)
    for ((branchName, branchType) <- branchesOf[T](mapBranches)) io.srv ! OpenBranch(treeId, branchName, branchType)
    new TTree[T](this, name, treeId)
  }
  
  override def toString = "TFile(%s)".format(file.getPath)
}


object TFile {
  val stringIdentity: PartialFunction[String, String] = { case s => s }

  def branchesOf[T <: Product : ClassManifest](mapName: PartialFunction[String, String] = stringIdentity): Seq[(String, String)] = {
    val ser = ProductSerializer.forType[T]
    for (field <- ser.flatFields) yield {
      val name = field.name.replaceAll("[.]", "_")
      val mappedName = if (mapName isDefinedAt name) mapName(name) else name
      (mappedName, field.typeName)
    }
  }

  def apply(file: JFile, mode: filemode.Value = filemode.read)(implicit io:RootIO = RootIO.defaultIO): TFile =
    io.openTFile(file, mode)
}



class TTree[T <: Product : ClassManifest](val file: TFile, val name: String, val id: Int) extends
  collection.SeqView[T, IndexedSeq[T]]
{
  tree =>
  import RootIO.requests._

  val io = file.io
  val timeout = file.timeout

  def underlying = null

  def iterator = new Iterator[T] {
    val n = tree.size
    var i = -1
    def hasNext = { i < n-1 }
    def next = { i += 1; tree(i) }
  }

  def length = getSize()

  def getSize(): Int = io.srv.!>(GetTreeSize(id), timeout)
  
  def apply(index: Int): T = {
    io.srv.!>(GetTreeEntry[T](id, index), timeout)
  }

  def +=(element: T): TTree[T] = {
    io.srv ! AddTreeEntry(id, element)
    this
  }
  
  override def toString = "TTree(%s, %s)".format(file, name)
}
