// Copyright (C) 2010 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.scpi


object NR1 {
  def unapply(a: Any) : Option[Int] = a match {
    case s:String => try { Some(s.toInt) } catch { case e: NumberFormatException => None }
    case _ => None
  }
}


object NRf {
  def unapply(a: Any) : Option[Double] = a match {
    case s:String => try { Some(s.toDouble) } catch { case e: NumberFormatException => None }
    case _ => None
  }
}


object SRD {
  protected val sqString = """'([^']*)'""".r
  protected val dqString = """"([^']*)"""".r

  def unapply(a: Any) : Option[String] = a match {
    case s:String => s match {
      case sqString(contents) => Some(contents)
      case dqString(contents) => Some(contents)
      case _ => None
    }

    case _ => None
  }
}


object IntSeq {
  def unapply(seq: Seq[Any]) : Option[Seq[Int]] = try {
    Some(seq map {_.asInstanceOf[String].toInt})
  } catch {
    case e: ClassCastException => None
    case e: NumberFormatException => None
  }
}


object BlockData {
  def apply(data: IndexedSeq[Byte]) : IndexedSeq[Byte] = {
    val tag = "#".getBytes
    val sizeStr = data.size.toString.getBytes
    val sizeSizeStr = sizeStr.size.toString.getBytes
    
    tag ++ sizeSizeStr ++ sizeStr ++ data
  }
}
