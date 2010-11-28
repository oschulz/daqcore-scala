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


package daqcore.util


trait Logging extends akka.util.Logging {
  def trace(a: => Any): Unit = log.trace(a.toString)
  def debug(a: => Any): Unit = log.debug(a.toString)
  def info(a: => Any):  Unit = log.info(a.toString)
  def warn(a: => Any):  Unit = log.warn(a.toString)
  def error(a: => Any): Unit = log.error(a.toString)
  
  def isTraceEnabled = log.trace_?
  def isDebugEnabled = log.debug_?
  def isInfoEnabled  = log.info_?
  def isWarnEnabled  = log.warning_?
  def isErrorEnabled = log.error_?
}
