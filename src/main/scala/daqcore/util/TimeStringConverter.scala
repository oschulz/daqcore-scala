// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>,

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

import java.util.{Date,TimeZone}, java.text.SimpleDateFormat


abstract class TimeStringConverter {
  protected def newDateFormat(fields: String) = {
    val format = new SimpleDateFormat(fields)
    format.setTimeZone(TimeZone.getTimeZone("UTC"))
    format
  }
  
  protected def unixMillis(unixTime: Double) = (unixTime * 1e3).toLong
  
  def apply(unixTime: Double): String
}


object ISOTime extends TimeStringConverter {
  private val dateFormatSeconds = newDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
  private val dateFormatMillis = newDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

  def apply(unixTime: Double): String = apply(unixTime, false)

  def apply(unixTime: Double, millis: Boolean = false): String = {
    val format = if (millis) dateFormatMillis else dateFormatSeconds
    format.format(new Date(unixMillis(unixTime)))
  }
}


object FileNameTimeStamp extends TimeStringConverter {
  val dateFormat = newDateFormat("yyyyMMdd_HHmmss")

  def apply(unixTime: Double): String = dateFormat.format(new Date(unixMillis(unixTime)))
}
