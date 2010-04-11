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
    "behave correctly" ignore {
      // implicit def defaultInetAcceptorBuilder = MinaInetAcceptorBuilder
      // implicit def defaultInetConnector = MinaInetConnector
    
      val ping = ByteCharSeq("ping\n")
      val pong = ByteCharSeq("pong\n")
      val quit = ByteCharSeq("quit\n")


      val acc = InetAcceptor (8002) { (conn: StreamIO) =>
        import scala.actors.Futures.alarm
        import scala.actors.Actor.loop
        loop {
          for (
            bytes <- conn.read();
            _ <- Futures.alarm(200)
          ) {
            val msg = bytes.toString.trim
            debug(msg)
            if (msg == quit.toString.trim) conn.close()
            else conn.write(pong)
          }
        }
      }

      actwait {
        val conn = InetConnection("localhost", 8002)
        
        def pings(i:Int): Unit = {
          if (i > 0) {
            debug("pings(%s)".format(i))
            conn write ping
            conn.read().respond { bytes =>
              debug(bytes.toString.trim)
              pings(i - 1)
            }
          }
          else conn write quit
        }
        
        pings(10)
      }
    }
  }
}
