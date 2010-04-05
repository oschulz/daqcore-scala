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


package daqcore.prot.keithley

import daqcore.util._


abstract class Message


case class Request(val commands: Command*) extends Message {
  def charSeq = {
    commands.map(_.charSeq).reduceLeft { _ ++ _ } ++
    ByteCharSeq('X')
  }

  override def toString = charSeq.toString
}


case class Output(data: ByteCharSeq) extends Message


class Command(val code: Char, val params:ByteCharSeq*) extends ByteCharSeqFragment {
  require( (code >= 'A') && (code <= 'Z') )

  def charSeq = ByteCharSeq(code) ++
    params.reduceLeft {_ ++ ByteCharSeq(',') ++ _ }

  override def toString = charSeq.toString
}


object Command {
  def apply(code: Char, params:ByteCharSeq*) = new Command(code, params: _*)
  def unapply(cmd: Command) = Some((cmd.code, cmd.params))

  def A(params:ByteCharSeq*) = Command('A', params: _*)
  def B(params:ByteCharSeq*) = Command('B', params: _*)
  def C(params:ByteCharSeq*) = Command('C', params: _*)
  def D(params:ByteCharSeq*) = Command('D', params: _*)
  def E(params:ByteCharSeq*) = Command('E', params: _*)
  def F(params:ByteCharSeq*) = Command('F', params: _*)
  def G(params:ByteCharSeq*) = Command('G', params: _*)
  def H(params:ByteCharSeq*) = Command('H', params: _*)
  def I(params:ByteCharSeq*) = Command('I', params: _*)
  def J(params:ByteCharSeq*) = Command('J', params: _*)
  def K(params:ByteCharSeq*) = Command('K', params: _*)
  def L(params:ByteCharSeq*) = Command('L', params: _*)
  def M(params:ByteCharSeq*) = Command('M', params: _*)
  def N(params:ByteCharSeq*) = Command('N', params: _*)
  def O(params:ByteCharSeq*) = Command('O', params: _*)
  def P(params:ByteCharSeq*) = Command('P', params: _*)
  def Q(params:ByteCharSeq*) = Command('Q', params: _*)
  def R(params:ByteCharSeq*) = Command('R', params: _*)
  def S(params:ByteCharSeq*) = Command('S', params: _*)
  def T(params:ByteCharSeq*) = Command('T', params: _*)
  def U(params:ByteCharSeq*) = Command('U', params: _*)
  def V(params:ByteCharSeq*) = Command('V', params: _*)
  def W(params:ByteCharSeq*) = Command('W', params: _*)
  def X(params:ByteCharSeq*) = Command('X', params: _*)
  def Y(params:ByteCharSeq*) = Command('Y', params: _*)
  def Z(params:ByteCharSeq*) = Command('Z', params: _*)
}
