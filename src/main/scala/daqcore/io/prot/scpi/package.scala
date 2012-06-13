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


package daqcore.io.prot

import daqcore.util._


package object scpi {

  implicit def Mnem2ICHeaderPart(mnem: SpecMnemonic) = mnem(1)
  implicit def int2NR1(i:Int) = NR1(i)
  implicit def double2NRf(x:Double) = NRf(x)
  implicit def mnem2CPD(m:SpecMnemonic) = CPD(m)
  
  //!! Sometimes cause SCPI parser test to fail (why?):
  //implicit def bytes2BlockData(bytes: ByteString) = BlockData(bytes)

  /** Clear Status (Command) */
  val CLS = CCQHeader("CLS")

  /** Standard Event Status Enable (Query) */
  val ESE = CCQHeader("ESE")

  /** Standard Event Status Register (Query) */
  val ESR = CCQHeader("ESR")

  /** Identification (Query) */
  val IDN = CCQHeader("IDN")

  /** Operation Complete (Command/Query) */
  val OPC = CCQHeader("OPC")

  /** Reset (Command) */
  val RST = CCQHeader("RST")

  /** Service Request Enable (Command/Query) */
  val SRE = CCQHeader("SRE")

  /** Read Status Byte (Query) */
  val STB = CCQHeader("STB")

  /** Self-Test (Query) */
  val TST = CCQHeader("TST")

  /** Wait-to-Continue (Command) */
  val WAI = CCQHeader("WAI")
  
  /** Default message terminator for stream-based connections */
  val StreamMsgTerm = ByteCharSeq.crlf
}
