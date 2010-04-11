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

import scala.actors._

import daqcore.util._
import daqcore.actors._

import java.net.InetSocketAddress


trait InetConnector extends ServerProxy with Closeable {
  profile[InetConnector]

  def connect(host: String, port: Int): Future[StreamIO] =
    connect(new InetSocketAddress(host, port), 0)

  def connect(host: String, port: Int, timeout: Long): Future[StreamIO] =
    connect(new InetSocketAddress(host, port), timeout)
  
  def connect(to: InetSocketAddress): Future[StreamIO] =
    connect(to, 0)

  def connect(to: InetSocketAddress, timeout: Long): Future[StreamIO] =
    self.!!& (InetConnector.Connect(to, timeout))
      { case a: Actor => StreamIO(a) }
}


object InetConnector {
  // reply: server supporting StreamIO
  case class Connect(to: InetSocketAddress, timeout: Long = 0)

  def apply(a: Actor) = new InetConnector { def self = a }
}
