// Copyright (C) 2011 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.actors

import akka.dispatch.Future

import java.lang.reflect.Method

import daqcore.util._


abstract trait MServer extends CascadableServer {
  protected final val srvMMaps = MServer.methodMaps(getClass)
  import MServer._
  import srvMMaps._

  // @call def methods = pubMethods
  
  protected def srvSCmd(func: Symbol, args: Any*): Unit = {
    val m = scmdMethod(SCallSignature.fromCall(func, args: _*))
    m.invoke(this, args.asInstanceOf[Seq[AnyRef]]: _*)
  }

  protected def srvSQry(func: Symbol, args: Any*): Unit = {
    val m = sqryMethod(SCallSignature.fromCall(func, args: _*))
    val result = m.invoke(this, args.asInstanceOf[Seq[AnyRef]]: _*)
    val target = replyTarget
    result match {
      case f: Future[_] => f onComplete { f => target ! f.get }
      case x => target ! x
    }
  }

  protected def srvACmd(op: ActorCmd): Unit = {
    val m = acmdMap(op.getClass)
    m.invoke(this, op.productIterator.toArray.asInstanceOf[Array[AnyRef]]: _*)
  }

  protected def srvAQry(op: ActorQuery[_]): Unit = {
    val m = aqryMap(op.getClass)
    val result = m.invoke(this, op.productIterator.toArray.asInstanceOf[Array[AnyRef]]: _*)
    val target = replyTarget
    result match {
      case f: Future[_] => f onComplete { f => target ! f.get }
      case x => target ! x
    }
  }

  protected def srvMsg(msg: AnyRef): Unit = {
    val m = msgMethod(msg.getClass)
    val result = m.invoke(this, msg)
    if (m.getReturnType != classOf[Unit]) {
      val target = replyTarget
      result match {
        case f: Future[_] => f onComplete { f => target ! f.get }
        case x => target ! x
      }
    }
  }

  override def serve = super.serve orElse {
    case SCmd(func, args @ _*) if scmdMethod isDefinedAt SCallSignature.fromCall(func, args: _*) => srvSCmd(func, args: _*)
    case SQry(func, args @ _*) if sqryMethod isDefinedAt SCallSignature.fromCall(func, args: _*) => srvSQry(func, args: _*)
    case op: ActorCmd if acmdMap.isDefinedAt(op.getClass) => srvACmd(op)
    case op: ActorQuery[_] if aqryMap.isDefinedAt(op.getClass) => srvAQry(op)
    case msg: AnyRef if msgMethod  isDefinedAt msg.getClass => srvMsg(msg)
  }
}


object MServer {
  import java.lang.annotation.{Annotation => JAnnotation}
  
  case class SCallSignature(func: Symbol, argTypes: Class[_]*) {
    def fullfills(that: SCallSignature) = {
      that.argTypes.corresponds(this.argTypes) {
        case (thatType, thisType) => thatType.wrapperClass.isAssignableFrom(thisType.wrapperClass)
      }
    }
  }
  
  case object SCallSignature {
    def fromCall(func: Symbol, args: Any*) = SCallSignature(func, args.toList map {_.asInstanceOf[AnyRef].getClass}: _*)
    def apply(m: Method): SCallSignature = SCallSignature(Symbol(m.getName), m.getParameterTypes: _*)
  }


  case class MethodMaps(cl: Class[AnyRef]) {
    val pubMethods = cl.getMethods.toList filter { m => m.findAnnotation[call] != None }

    val (sreqMethods, areqMethods) = pubMethods partition { m => classOf[SReq[_]].isAssignableFrom(m.findAnnotation[call].get.mc) }
    
    val scmdMap: Map[Symbol, Seq[Method]] = ( for {
      m <- sreqMethods
      if (m.getReturnType == classOf[Unit])
    } yield { Symbol(m.getName) -> m } ) groupBy { e => e._1} map { case (k,v) => (k, v.map{_._2}) }

    val sqryMap: Map[Symbol, Seq[Method]] = ( for {
      m <- sreqMethods
      if (m.getReturnType != classOf[Unit])
    } yield { Symbol(m.getName) -> m } ) groupBy {e => e._1} map { case (k,v) => (k, v.map{_._2}) }
    
    def findSMethod(map: Map[Symbol, Seq[Method]], sig: SCallSignature): Option[Method] = {
      map.get(sig.func) match {
        case Some(seq) => seq find { m => sig.fullfills(SCallSignature(m)) }
        case None => None
      }
    }

    val scmdMethod = OptResultCache { sig: SCallSignature => findSMethod(scmdMap, sig) }
    val sqryMethod = OptResultCache { sig: SCallSignature => findSMethod(sqryMap, sig) }


    protected case class ClassMethodPair(cl: Class[_], m: Method)

    val acmdMap: Map[Class[_], Method] = ( ( for {
      m <- areqMethods
      mc <- Seq(m.findAnnotation[call].get.mc)
      if (classOf[ActorCmd].isAssignableFrom(mc))
    } yield { ClassMethodPair(mc, m) } ) map { p => p.cl -> p.m } ).toMap

    val aqryMap: Map[Class[_], Method] = ( ( for {
      m <- areqMethods
      mc <- Seq(m.findAnnotation[call].get.mc)
      if (classOf[ActorQuery[_]].isAssignableFrom(mc))
    } yield { ClassMethodPair(mc, m) } ) map { p => p.cl -> p.m } ).toMap


    val msgReqSeq = {
      val msgMethods = cl.getMethods.toList filter { m => m.findAnnotation[msg] != None }

      val msgReqMap: Map[Class[_], Method] = ( ( for { m <- msgMethods } yield {
        val paramTypes = m.getParameterTypes
        require (paramTypes.length == 1)
        val mc = paramTypes(0)
        ClassMethodPair(mc, m)
      } ) map { p => p.cl -> p.m } ).toMap
      
      msgReqMap.toSeq sortWith { (a,b) => b._1.isAssignableFrom(a._1) }
    }

    val msgMethod = OptResultCache { msgClass: Class[_] => msgReqSeq find {e => e._1 isAssignableFrom msgClass} map {_._2} }
  }
  
  val methodMaps = ResultCache { k: Class[_] => MethodMaps(k.asInstanceOf[Class[AnyRef]]) }
}
