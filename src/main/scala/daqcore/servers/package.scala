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


package daqcore

package object servers {
  import daqcore.profiles._

  implicit object DefaultMinaInetAcceptorBuilder extends InetAcceptorBuilder {
    import daqcore.servers._
    
    def apply (port: Int) (body: ByteIO => Unit) = {
      val ma = new MinaAcceptor(port,body)
      ma.start()
      ma
    }
  }

  
  implicit lazy val DefaultMinaInetConnector = {
    import daqcore.servers._
    val mc = new MinaConnector
    mc.start
    mc
  }

  def defaultInetAcceptorBuilder = DefaultMinaInetAcceptorBuilder
  def defaultInetConnector = DefaultMinaInetConnector

  implicit val defaultVXI11Connector : VXI11Connector = {
    val connector = new RTVXI11Connector
    connector.start
    connector
  }
}
