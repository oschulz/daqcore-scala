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

import org.slf4j.{Logger => SLFLogger}
import akka.event.slf4j.{Logger => AkkaSLFLogger}


trait Logging {
  obj =>

  import Logging._

  @transient lazy val log = Logger(obj.getClass.getName)

  final def trace(a: => Any) { log.trace(a.toString) }
  final def debug(a: => Any) { log.debug(a.toString) }
  final def info(a: => Any)  { log.info(a.toString) }
  final def warn(a: => Any)  { log.warn(a.toString) }
  final def error(a: => Any) { log.error(a.toString) }
}


object Logging {
  class Logger(loggerName: String) {
    val logger: SLFLogger = AkkaSLFLogger(loggerName)

    final def name = logger.getName

    final def traceEnabled = logger.isTraceEnabled
    final def debugEnabled = logger.isDebugEnabled
    final def infoEnabled  = logger.isInfoEnabled
    final def warnEnabled  = logger.isWarnEnabled
    final def errorEnabled = logger.isErrorEnabled

    final def trace(msg: => String) { if (traceEnabled) logger trace msg }
    final def debug(msg: => String) { if (debugEnabled) logger debug msg }
    final def info(msg: => String)  { if (infoEnabled)  logger info  msg }
    final def warn(msg: => String)  { if (warnEnabled)  logger warn  msg }
    final def error(msg: => String) { if (errorEnabled) logger error msg }
  }

  object Logger {
    def apply(loggerName: String) : Logger = new Logger(loggerName)
    def rootLogger : Logger = apply(SLFLogger.ROOT_LOGGER_NAME)
  }
}
