// Copyright (C) 2012 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.io


import scala.language.postfixOps

import java.net.{URI => JavaURI}

import akka.actor.ActorPath


object GenericURI {
  def apply(string: String): URI = new JavaURI(string)

  def unapply(x: Any): Option[String] = x match {
    case uri: JavaURI => Some(uri.toString)
    case _ => None
  }
}


object AuthorityURL {
  def apply(
    scheme: Option[String] = None, userInfo: Option[String] = None,
    host: Option[String] = None, port: Option[Int] = None,
    path: Option[String] = None, query: Option[String] = None,
    fragment: Option[String] = None
  ) = new JavaURI(
    scheme orNull, userInfo orNull, host orNull,
    port getOrElse -1, path orNull, query orNull, fragment orNull
  )
  
  def unapply(x: Any): Option[(
    Option[String], Option[String], Option[String], Option[Int],
    Option[String], Option[String], Option[String]
  )] = x match {
    case uri: JavaURI => Some( (
      Option(uri.getScheme),
      Option(uri.getUserInfo),
      Option(uri.getHost),
      { val port = uri.getPort; if (port >= 0) Some(port) else None },
      Option(uri.getRawPath) flatMap { p => if (p.isEmpty) None else Some(p)},
      Option(uri.getQuery),
      Option(uri.getFragment)
    ) )
    case uri: String => unapply(new JavaURI(uri))
    case _ => None
  }
}


object HostURL {
  def apply(scheme: String, host: String, port: Option[Int] = None) =
    AuthorityURL(Some(scheme), None, Some(host), port)
  
  def unapply(x: Any): Option[(String, String, Option[Int])] = x match {
    case AuthorityURL(Some(scheme), None, Some(host), port, None, None, None) =>
      Some( (scheme, host, port) )
    case uri: String => unapply(new JavaURI(uri))
    case _ => None
  }
}


object AkkaActorPath {
  def unapply(x: Any): Option[ActorPath] = x match {
    case uri: JavaURI => unapply(uri.toString)
    case uri: String =>
      try Some(ActorPath.fromString(uri))
      catch { case e: java.net.MalformedURLException => None }
    case _ => None
  }
}
