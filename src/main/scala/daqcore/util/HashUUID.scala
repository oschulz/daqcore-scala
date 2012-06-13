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


package daqcore.util

import java.util.UUID
import java.security.MessageDigest


object ByteStringUUID {
  def apply(bytes: ByteString) : UUID = {
    require(bytes.size == 16)
    val it = bytes.iterator
    val msb = BigEndian.getLong(it)
    val lsb = BigEndian.getLong(it)
    
    new UUID(msb, lsb)
  }
    
  def unapply(uuid: UUID) : Option[ByteString] = {
    implicit def byteOrder = BigEndian.nioByteOrder
    val builder = ByteString.newBuilder
    builder.sizeHint(16)
    BigEndian.putLong(builder, uuid.getMostSignificantBits)
    BigEndian.putLong(builder, uuid.getLeastSignificantBits)
    Some(builder.result)
  }
  
  // Test:
  // val u = UUID.randomUUID
  // val ByteStringUUID(bytes) = u
  // val u2 = ByteStringUUID(bytes)
  // u == u2
}


class HashUUID (val version: Int) {
  val hashAlgo = version match {
    case 3 => "MD5"
    case 5 => "SHA-1"
    case i => throw new IllegalArgumentException("Invalid HashUUID version " + i + ", must be 3 or 5")
  }
  
  def apply(namespace: UUID, name: Seq[Byte]) : UUID = {
    val ByteStringUUID(nsBytes) = namespace
    val digest = MessageDigest.getInstance(hashAlgo)
    digest.reset()
    digest.update(nsBytes.toArray)
    digest.update(name.toArray)
    var hash = digest.digest()
    hash(6) = ((hash(6) & 0x0F) | (version << 4)).toByte;
    hash(8) = ((hash(8) & 0x3F) | 0x80).toByte;
    
    ByteStringUUID(ByteString(hash.take(16)))
  }

  def apply(namespace: UUID, name: String) : UUID =
    apply(namespace, name.getBytes)
}


object Version3UUID extends HashUUID(3) {}
object Version5UUID extends HashUUID(5) {}

/* Testing:

import util.Random
import scala.tools.nsc.io.Process.Pipe._

val urlNS = UUID.fromString("6ba7b811-9dad-11d1-80b4-00c04fd430c8")

def v5URLUUID(url: String) = {
  val cmd = "uuid -v 5 'ns:URL' '" + url + "'"
  // println(cmd)
  UUID.fromString((Nil | cmd) head)
}

val v5Test = (1 to 100).view map { i => {
    val s = Random.nextASCIIString(Random.nextInt(20)).replace("'","").replace("-","_")
    (v5URLUUID(s), Version5UUID(urlNS, s))
  } }

v5Test.forall (e => e._1 == e._2)

def v3URLUUID(url: String) = {
  val cmd = "uuid -v 3 'ns:URL' '" + url + "'"
  // println(cmd)
  UUID.fromString((Nil | cmd) head)
}

val v3Test = (1 to 100).view map { i => {
    val s = Random.nextASCIIString(Random.nextInt(20)).replace("'","").replace("-","_")
    (v3URLUUID(s), Version3UUID(urlNS, s))
  } }

v3Test.forall (e => e._1 == e._2)
*/
