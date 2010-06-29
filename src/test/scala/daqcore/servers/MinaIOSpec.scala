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


class MinaIOSpec extends WordSpec with MustMatchers with Logging {
  "MinaIO" should {
    // Disabled until Mina shuts down correctly
    "behave correctly" in {
      // implicit def defaultInetAcceptorBuilder = MinaInetAcceptorBuilder
      // implicit def defaultInetConnector = MinaInetConnector
    
      val ping = ByteCharSeq("ping\n")
      val pong = ByteCharSeq("pong\n")
      val quit = ByteCharSeq("quit\n")


      val acc = InetAcceptor (8002) { (conn: StreamIO) =>
        start(new Server {
          override def init = { conn setReceiver(srv, true) }
          def serve = { case StreamIO.Received(bytes) =>
            Thread.sleep(200)
            val msg = bytes.toString.trim
            debug(msg)
            if (msg == quit.toString.trim) { conn.close(); exit() }
            else conn.write(pong)
          }
        } )
      }
      
      Thread.sleep(2000)

      val conn = InetConnection("localhost", 8002)
      
      for (i <- 1 to 10) {
        debug("ping %s".format(i))
        conn write ping
        val bytes = conn.read()
        debug(bytes.toString.trim)
      }
      conn write quit
    }
  }
}
