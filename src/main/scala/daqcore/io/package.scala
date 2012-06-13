// Copyright (C) 2010-2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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

import java.net.{URI => JavaURI}
import daqcore.util.{ByteString, ByteStringBuilder}


package object io {
  val IO = akka.actor.contrib.daqcore.IO

  type URI = java.net.URI
  val URI = GenericURI
  
  type Encoder[A] = (ByteStringBuilder, A) => Unit
  type Decoder[A] = IO.Iteratee[A]
  
  type FrameEncoder = Decoder[ByteString]
  type FrameDecoder = Decoder[ByteString]
  type FrameCodec = Codec[ByteString, ByteString]

  type InetAddr = java.net.InetAddress

  object InetAddr {
    def apply(host: String): InetAddr = java.net.InetAddress.getByName(host)
  }

  implicit def inetAddr(host: String): InetAddr = InetAddr(host)


  type InetSockAddr = java.net.InetSocketAddress

  object InetSockAddr {
    def apply(host: String, port: Int): InetSockAddr = new InetSockAddr(host, port)
    def apply(port: Int): InetSockAddr = new InetSockAddr(port)
  }

  implicit def inetSockAddr(hostPort: (String, Int)): InetSockAddr = InetSockAddr(hostPort._1, hostPort._2)
  implicit def inetSockAddr(port: Int): InetSockAddr = InetSockAddr(port)
}
