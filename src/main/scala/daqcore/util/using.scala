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

import scala.language.reflectiveCalls


/** Language extensions to use resources and close them afterwards, exception
 * safe.
 */
 
object using {
  /** Apply a block of code to a resource and close if afterwards
   *
   * Example usage:
   * <pre>
   *     val lines = using ( scala.io.Source.fromPath("/etc/hosts") ) {
   *         src => src.getLines().toList
   *     }
   *     lines foreach println
   * </pre>
   *  
   * @param   resource  an object with a <code>close()</code> method
   * @param   body      the code to run on <code>resource</code>
   * @return  the result of executing <code>body<c/ode>
   */
 
  def apply [T <: {def close()}, U] (resource: T) (body : T => U) : U = {
    try { body(resource) }
    finally { if (resource != null) resource.close() }
  }


  /** Apply a block of code to two resources and close them afterwards
   *
   * Note: If <code>getRes1</code> throws an exception, <code>getRes2</code>
   * will never be executed. It it recommended to open the resources in question
   * within <code>getRes1</code> and <code>getRes2</code> to make the code fully
   * exception safe.
   *  
   * @param   getRes1  something yielding an object with a <code>close()</code> method
   * @param   getRes2  something yielding an object with a <code>close()</code> method
   * @param   body  the code to run on <code>resource</code>
   * @return  the result of executing <code>body<c/ode>
   */

  def apply [T1 <: {def close()}, T2 <: {def close()}, U]
    (getRes1: => T1, getRes2: => T2) (body : (T1,T2) => U) : U =
  {
    val res1 = getRes1
    try {
      val res2 = getRes2
      try { body(res1, res2) }
      finally { if (res2 != null) res1.close() }
    }
    finally { if (res1 != null) res1.close() }
  }
}
