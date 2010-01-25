// Copyright (C) 2009-2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


import daqcore.{shutdown, noshutdown}

class ShutdownTest {
  try {
    noshutdown {
      while (! shutdown.requested) {
        println("enter main loop")
        Thread.sleep(4000)
        println("exit main loop")
      }
      
      println("cleanup")
      
      throw new Exception  // Exception safety test
    }
  }
  catch {
    case e => println("Exception caught")
    // VM should exit on SIGHUP despite the endless loop below,
    // since it is outside the noshutdown block.
    while (true) Thread.sleep(1000)
  }
}
