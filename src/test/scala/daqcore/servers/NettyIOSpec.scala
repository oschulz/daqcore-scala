// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>,

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


package daqcore.servers

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers
import daqcore.util._, daqcore.actors._, daqcore.profiles._, daqcore.servers._

import akka.actor.Actor.actorOf


class NettyIOSpec extends WordSpec with MustMatchers with Logging {
  "NettyIO" should {
    // Disabled until Netty shuts down correctly
    "behave correctly" in {
      // implicit def defaultInetAcceptorBuilder = NettyInetAcceptorBuilder
      // implicit def defaultInetConnector = NettyInetConnector
    
      val ping = ByteCharSeq("ping\n")
      val pong = ByteCharSeq("pong\n")
      val quit = ByteCharSeq("quit\n")
      val err = ByteCharSeq("error\n")

      val acc = NettyServer (8002) { (conn: ByteStreamIO) =>
        actorOf(new Server {
          override def init = { conn.triggerRecv() }
          def serve = { case ByteStreamInput.Received(bytes) =>
            conn.triggerRecv()
            Thread.sleep(200)
            val msg = ByteCharSeq(bytes: _*).toString.trim
            debug(msg)
            if (msg == quit.toString.trim) { conn.close(); exit() }
            else if (msg == ping.toString.trim) conn send pong
            else conn send err
          }
        } ).start
      }
      
      val conn = NettyClientConnection(("localhost", 8002))
      
      for (i <- 1 to 10) {
        debug("ping %s".format(i))
        conn send ping
        val bytes = conn.recv()
        val msg = ByteCharSeq(bytes: _*).toString.trim
        debug(msg)
        assert( msg == pong.toString.trim )
      }
      //conn send quit
      conn.close
      acc.close
    }
  }
}
