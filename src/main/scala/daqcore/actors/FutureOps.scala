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


package daqcore.actors

import java.util.concurrent.TimeoutException

import scala.concurrent.Future
import akka.util.Timeout


class FutureOps[A](val future: Future[A]) extends AnyVal {
  def get(implicit timeout: Timeout): A = {
    scala.concurrent.Await.result(future, timeout.duration)
  }
  
  def getOpt(implicit timeout: Timeout): Option[A] = {
    try { Some(get) } 
    catch { case e: java.util.concurrent.TimeoutException => None }
  }

  def v = future.value.get.get
}
