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


package daqcore.actors

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers


class ActorOpsSpec extends WordSpec with MustMatchers {
  import scala.actors._, scala.actors.Actor._
  import daqcore.util._
  
  "ActorOps.!!?" should {
  
    val a = new Server {
      def serve = {
        case i:Int => {
          val replyCh = sender
          actor {
            Thread.sleep(50)
            replyCh ! (2*i)
          }
        }
      }
    }
    a.start
  
    "function correctly" in {
      def af(i:Int) = a.!!^[Int](i)

      def exec[A](x: => Unit): Option[Boolean] = { x; Some(true) }

      var sum = 0
      
      actwait {
        println("actwait in " + self)
        // yield does not work here (because react never returns?)
        val ar = for (
          i <- af(5);
          j <- af(1);
          s <- Some(i+j)
        ) { sum = s}
      }
      assert (sum === 12)

      // Alternative:
      actwait { af(5) respond { i => af(2) respond { j => sum = i+j } } }
      assert (sum === 14)

      // Parallel Execution (doesn't seem to work correctly):

      actwait {
        val fi = af(5)
        val fj = af(3)
        fi respond { i => fj respond { j => sum = i+j } }
      }
      assert (sum === 16)

      actwait {
        val fi = af(5)
        val fj = af(4)

        val ar = for (
          i <- fi;
          j <- fj;
          s <- Some(i+j)
        ) sum = s
      }
      assert (sum === 18)
    }
  }
}
