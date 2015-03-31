// Copyright (C) 2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import scala.collection.breakOut
import scala.collection.immutable.Queue
import akka.actor._

import daqcore.actors._, daqcore.actors.TypedActorTraits._
import daqcore.util._
import daqcore.io._
import daqcore.io.memory._


trait SIS3316 extends Device {
  def memory: Future[SIS3316Memory]

  //def vmeFWVersion: Future[String]
}

object SIS3316 extends DeviceCompanion[SIS3316] {
  def impl = { case uri => new SIS3316Impl(uri.toString) }

  class SIS3316Impl(vmeURI: String) extends SIS3316
    with CloseableTAImpl with SyncableImpl
  {
    val mem = SIS3316Memory(vmeURI, "memory")

    def memory = successful(mem)


    def identity = successful("SIS3316")

    ///def vmeFWVersion = ...
  }
}
