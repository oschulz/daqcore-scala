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


trait InetConnector extends Profile with Closeable {
  def connectF(host: String, port: Int): Future[InetConnection] =
    connectF(new InetSocketAddress(host, port), -1)

  def connectF(host: String, port: Int, timeout: Long): Future[InetConnection] =
    connectF(new InetSocketAddress(host, port), timeout)
  
  def connectF(to: InetSocketAddress): Future[InetConnection] =
    connectF(to, -1)

  def connectF(to: InetSocketAddress, timeout: Long): Future[InetConnection] =
    srv.!!& (InetConnector.Connect(to, timeout))
      { case a: Server with InetConnection => a }
}


object InetConnector {
  // reply: InetConnection
  case class Connect(to: InetSocketAddress, timeout: Long = -1)
}


trait InetConnection extends StreamIO


object InetConnection {
  def apply(host: String, port: Int)(implicit connector: InetConnector): StreamIO =
    connector.connectF(host, port)()

  def apply(host: String, port: Int, timeout: Long)(implicit connector: InetConnector): StreamIO =
    connector.connectF(host, port, timeout)()
  
  def apply(to: InetSocketAddress)(implicit connector: InetConnector): StreamIO =
    connector.connectF(to)()

  def apply(to: InetSocketAddress, timeout: Long)(implicit connector: InetConnector): StreamIO =
    connector.connectF(to, timeout)()
}
