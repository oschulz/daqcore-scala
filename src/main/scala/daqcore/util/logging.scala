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


object LogLevel extends Enumeration {
  import org.apache.log4j.{Level => L4JLevel}
  
  val All = Value(0, "All")
  val Trace = Value(5000, "Trace")
  val Debug = Value(10000, "Debug")
  val Info = Value(20000, "Info")
  val Warn = Value(30000, "Warn")
  val Error = Value(40000, "Error")
  val Fatal = Value(50000, "Fatal")
  val Off = Value(Int.MaxValue, "Off")
}


object log {
  val logger: Logger = Log4JLogger
  
  def level = logger.level
  def level_=(newLevel: LogLevel.Value) = {logger.level = newLevel}

  def traceEnabled = logger.traceEnabled
  def debugEnabled = logger.debugEnabled
  def infoEnabled = logger.infoEnabled
  def warnEnabled = logger.warnEnabled
  def errorEnabled = logger.errorEnabled
  def fatalEnabled = logger.fatalEnabled

  def trace(msg: => AnyRef, obj: AnyRef = null) = logger.trace(msg, obj)
  def debug(msg: => AnyRef, obj: AnyRef = null) = logger.debug(msg, obj)
  def info(msg: => AnyRef, obj: AnyRef = null) = logger.info(msg, obj)
  def warn(msg: => AnyRef, obj: AnyRef = null) = logger.warn(msg, obj)
  def error(msg: => AnyRef, obj: AnyRef = null) = logger.error(msg, obj)
  def fatal(msg: => AnyRef, obj: AnyRef = null) = logger.fatal(msg, obj)
}


trait Logging {
  protected def trace(msg: => AnyRef) = log.trace(msg, this)
  protected def debug(msg: => AnyRef) = log.debug(msg, this)
  protected def info(msg: => AnyRef) = log.info(msg)
  protected def warn(msg: => AnyRef) = log.warn(msg)
  protected def error(msg: => AnyRef) = log.error(msg)
  protected def fatal(msg: => AnyRef) = log.fatal(msg)
}


abstract class Logger {
  def level: LogLevel.Value
  def level_=(newLevel: LogLevel.Value) : Unit

  def traceEnabled: Boolean
  def debugEnabled: Boolean
  def infoEnabled: Boolean
  def warnEnabled: Boolean
  def errorEnabled: Boolean
  def fatalEnabled: Boolean

  def trace(msg: => AnyRef, obj: AnyRef = null)
  def debug(msg: => AnyRef, obj: AnyRef)
  def info(msg: => AnyRef, obj: AnyRef)
  def warn(msg: => AnyRef, obj: AnyRef)
  def error(msg: => AnyRef, obj: AnyRef)
  def fatal(msg: => AnyRef, obj: AnyRef)
}


object ConsoleLogger extends Logger {
  var logLevel = LogLevel.Info

  def level = logLevel
  def level_=(newLevel: LogLevel.Value) = {logLevel = newLevel}

  def traceEnabled = level <= LogLevel.Trace
  def debugEnabled = level <= LogLevel.Debug
  def infoEnabled = level <= LogLevel.Info
  def warnEnabled = level <= LogLevel.Warn
  def errorEnabled = level <= LogLevel.Error
  def fatalEnabled = level <= LogLevel.Fatal

  def trace(msg: => AnyRef, obj: AnyRef = null) = if (traceEnabled) {
    val message =
      if (obj == null) msg
      else obj.getClass.getName + ": " + msg
    synchronized{ Console.err.println("TRACE: " + message) }
  }

  def debug(msg: => AnyRef, obj: AnyRef = null) = if (debugEnabled) {
    val message =
      if (obj == null) msg
      else obj.getClass.getName + ": " + msg
    synchronized{ Console.err.println("DEBUG: " + message) }
  }
    
  def info(msg: => AnyRef, obj: AnyRef = null) = if (infoEnabled)
    synchronized{ Console.err.println("INFO: " + msg) }

  def warn(msg: => AnyRef, obj: AnyRef = null) = if (warnEnabled)
    synchronized{ Console.err.println("WARNING: " + msg) }

  def error(msg: => AnyRef, obj: AnyRef = null) = if (errorEnabled)
    synchronized{ Console.err.println("ERROR: " + msg) }

  def fatal(msg: => AnyRef, obj: AnyRef = null) = if (fatalEnabled)
    synchronized{ Console.err.println("FATAL: " + msg) }
}


object Log4JLogger extends Logger {
  import org.apache.log4j.{Logger=>L4JLogger, Level=>L4JLevel, LogManager}
  
  protected lazy val log4jConfigured = {
    if (LogManager.getLoggerRepository.getCurrentLoggers.hasMoreElements) {
      true
    } else {
      org.apache.log4j.BasicConfigurator.configure()
      true
    }
  }

  protected implicit def toL4JLevel(level: LogLevel.Value): L4JLevel = level match {
    case LogLevel.All => L4JLevel.ALL
    case LogLevel.Trace => L4JLevel.TRACE
    case LogLevel.Debug => L4JLevel.DEBUG
    case LogLevel.Info => L4JLevel.INFO
    case LogLevel.Warn => L4JLevel.WARN
    case LogLevel.Error => L4JLevel.ERROR
    case LogLevel.Fatal => L4JLevel.FATAL
    case LogLevel.Off => L4JLevel.OFF
    case level => throw new IllegalArgumentException("Unknown logging level " + level)
  }

  protected implicit def fromL4JLevel(level: L4JLevel):LogLevel.Value  = level match {
    case L4JLevel.ALL => LogLevel.All
    case L4JLevel.TRACE => LogLevel.Trace
    case L4JLevel.DEBUG => LogLevel.Debug
    case L4JLevel.INFO => LogLevel.Info
    case L4JLevel.WARN => LogLevel.Warn
    case L4JLevel.ERROR => LogLevel.Error
    case L4JLevel.FATAL => LogLevel.Fatal
    case L4JLevel.OFF => LogLevel.Off
    case level => throw new IllegalArgumentException("Unknown logging level " + level)
  }
  
  protected def log4jLogger(obj: AnyRef): L4JLogger =
    if (obj == null) L4JLogger.getRootLogger
    else L4JLogger.getLogger(obj.getClass.getName)

  protected def log4jLogger: L4JLogger = log4jLogger(null)

  def level = log4jLogger.getLevel
  def level_=(newLevel: LogLevel.Value) = log4jLogger.setLevel(newLevel)
  
  def traceEnabled = log4jLogger.isEnabledFor(L4JLevel.TRACE)
  def debugEnabled = log4jLogger.isEnabledFor(L4JLevel.DEBUG)
  def infoEnabled = log4jLogger.isEnabledFor(L4JLevel.INFO)
  def warnEnabled = log4jLogger.isEnabledFor(L4JLevel.WARN)
  def errorEnabled = log4jLogger.isEnabledFor(L4JLevel.ERROR)
  def fatalEnabled = log4jLogger.isEnabledFor(L4JLevel.FATAL)

  def trace(msg: => AnyRef, obj: AnyRef = null) = { require(log4jConfigured); if (traceEnabled) log4jLogger(obj).trace(msg.toString) }
  def debug(msg: => AnyRef, obj: AnyRef = null) = { require(log4jConfigured); if (debugEnabled) log4jLogger(obj).debug(msg.toString) }
  def info(msg: => AnyRef, obj: AnyRef = null) =  { require(log4jConfigured); if (infoEnabled) log4jLogger.info(msg.toString) }
  def warn(msg: => AnyRef, obj: AnyRef = null) =  { require(log4jConfigured); if (warnEnabled) log4jLogger.warn(msg.toString) }
  def error(msg: => AnyRef, obj: AnyRef = null) = { require(log4jConfigured); if (errorEnabled) log4jLogger.error(msg.toString) }
  def fatal(msg: => AnyRef, obj: AnyRef = null) = { require(log4jConfigured); if (fatalEnabled) log4jLogger.error(msg.toString) }
}
