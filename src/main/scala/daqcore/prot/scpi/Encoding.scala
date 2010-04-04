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


package daqcore.prot.scpi

object Encoding extends Enumeration {
  type Encoding = Value

  /** Comma-separated <Numeric> data */
  val ASCii = Value(0, "ASCii")

  /** IEEE floating point, 32 bits */
  val IFP32 = Value(1, "IFP32")

  /** IEEE floating point, 64 bits */
  val IFP64 = Value(2, "IFP64")

  /** IEEE 8-bit signed integer (default) */
  val INT8 = Value(3, "INT8")

  /** IEEE 16-bit signed integer */
  val INT16 = Value(4, "INT16")

  /** IEEE 32-bit signed integer */
  val INT32 = Value(5, "INT32")

  /** IEEE 64-bit signed integer */
  val INT64 = Value(6, "INT64")

  /** IEEE swapped floating point, 32 bits */
  val SFP32 = Value(7, "SFP32")

  /** IEEE swapped floating point, 64 bits */
  val SFP64 = Value(8, "SFP64")

  /** Swapped IEEE 16-bit signed integer */
  val SINT16 = Value(9, "SINT16")

  /** Swapped IEEE 32-bit signed integer */
  val SINT32 = Value(10, "SINT32")

  /** Swapped IEEE 64-bit signed integer */
  val SINT64 = Value(11, "SINT64")

  /** Swapped IEEE 16-bit unsigned integer */
  val SUINT16 = Value(12, "SUINT16")

  /** Swapped IEEE 32-bit unsigned integer */
  val SUINT32 = Value(13, "SUINT32")

  /** Swapped IEEE 64-bit unsigned integer */
  val SUINT64 = Value(14, "SUINT64")

  /** IEEE 8-bit unsigned integer */
  val UINT8 = Value(15, "UINT8")

  /** IEEE 16-bit unsigned integer */
  val UINT16 = Value(16, "UINT16")

  /** IEEE 32-bit unsigned integer */
  val UINT32 = Value(17, "UINT32")

  /** IEEE 64-bit unsigned integer */
  val UINT64 = Value(18, "UINT64")
}
