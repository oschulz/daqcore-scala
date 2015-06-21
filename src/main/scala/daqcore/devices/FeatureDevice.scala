// Copyright (C) 2013-2015 Oliver Schulz <oliver.schulz@tu-dortmund.de>

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


package daqcore.devices

import java.lang.reflect.{InvocationTargetException, Method, InvocationHandler, Proxy}

import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

import akka.actor.ActorRef

import daqcore.util._
import daqcore.actors._


trait FeatureDevice[+Feats <: DeviceFeatures] extends Device {
  // State count increase does *not* necessarily imply that any state values have changed.
  def stateCount: Future[StateCount]

  //def getNextFeatureState(feature: PropPath, forceUpdate: Boolean = false): Future[PartialState]
  //def getLastFeatureState(feature: PropPath, timeWindow: NanoTimeWindow): Future[PartialState]
  //def getFeatureStateWithin(feature: PropPath, timeWindow: NanoTimeWindow): Future[PartialState]
  //def getFeatureStateAtCount(feature: PropPath, state: StateCount): Future[PartialState]
  //def setFeatureState(feature: PropPath, state: PartialState): Future[Unit]
}


object FeatureDevice {
  implicit class FeatureDeviceOps[+Feats <: DeviceFeatures : ClassTag](val device: FeatureDevice[Feats]) {
    def features: Feats = DeviceFeatures.proxy[Feats, FeatureDevice[Feats]](device)
  }


  class StateCounter {
    protected def getCurrentTime: NanoTime = System.currentTimeMillis() * 1000000

    protected var currentTime: NanoTime = getCurrentTime
    protected var currentValue: StateCount = scala.util.Random.nextLong
 
    def value: StateCount = currentValue

    def inc(): StateCount = inc(1)

    def inc(n: Long): StateCount = {
      currentTime = getCurrentTime
      currentValue = currentValue + 1
      currentValue
    }

    def time: NanoTime = currentTime
  }


  abstract class TAImpl[+Feats <: DeviceFeatures] extends CloseableTAImpl with SyncableImpl with LocalECTypedActorImpl {
    def features: Feats

    def stateCounter: StateCounter = new StateCounter

    def stateCount = successful(stateCounter.value)


    abstract class AbstractFeaturesImpl extends DeviceFeatures {
      def currentValueContext = TASContext(stateCounter.time, selfRef, stateCounter.value)

      trait ValueFeatureImpl extends ValueFeature

      trait ReadableValueImpl[+X] extends ReadableValueDecl[X] with ValueFeatureImpl {
        def readLast() = successful(ContextValue(lastValueContext, lastValue.get))

        def lastValue: Option[X]
        def lastValueContext: TASContext
      }

      trait WritableValueImpl[-X] extends WritableValueDecl[X] with ValueFeatureImpl

      trait ROValueImpl[+X] extends ROValueDecl[X] with ReadableValueImpl[X]
      trait WOValueImpl[-X] extends WOValueDecl[X] with WritableValueImpl[X]
      trait RWValueImpl[X] extends RWValueDecl[X] with ReadableValueImpl[X] with WritableValueImpl[X]

      type ReadableValue[+X] = ReadableValueImpl[X]
      type WritableValue[-X] = WritableValueImpl[X]

      type ROValue[+X] = ROValueImpl[X]
      type WOValue[-X] = WOValueImpl[X]
      type RWValue[X] = RWValueImpl[X]


      case class ConstValue[+X](value: X) extends ROValueImpl[X] {
        val lastValue = Some(value)
        val lastValueContext = currentValueContext
      }


      case class VarValue[X](initValue: Option[X]) extends RWValueImpl[X] {
        var lastValue = initValue
        var lastValueContext = currentValueContext

        def write(value: X) = {
          lastValue = Some(value)
          lastValueContext = currentValueContext
          successful(lastValueContext.state)
        }
      }
    }

  }


  abstract class Companion[Dev <: FeatureDevice[_] : ClassTag] extends DeviceCompanion[Dev]
}



trait DeviceFeatures {
  import DeviceFeatures._

  trait Feature {
    //!!! def name: PropKey
    //!!! def path: PropPath
    //!!! def title: String  //!!! Implement via annotations
  }

  trait FeatureGroup extends Feature

  trait RepeatedFeature[T <: Feature] extends FeatureGroup with PartialFunction[Int, T]


  trait SingleFeature extends Feature


  trait ValueFeature extends SingleFeature {
    //!!! def Unit: ???  //!!! Implement via annotations. Option[Unit]?
  }

  trait ReadableValueDecl[+X] extends ValueFeature {
    // def update(): Future[Unit]
    // def readNext(): Future[ReadResult[X]]
    def readLast(): Future[ReadResult[X]]

    def readNewerThan(time: NanoTime): Future[ReadResult[X]] = null  //!!!

    // Possibly later: def readWithin(timeWindow: NanoTimeWindow): Future[ReadResult[X]]
    // Possibly later: def readAfterState(state: StateCount): Future[ReadResult[X]]

    //def subscribe(subscriber: ActorRef): Future[Unit]
    //def unsubscribe(subscriber: ActorRef): Unit
  }


  trait WritableValueDecl[-X] extends ValueFeature {
    def write(value: X): Future[StateCount]

    // Add later. To fail if current state != lastState
    // def writeFromState(lastState: StateCount, value: X): Future[StateCount] = null  //!!!

    // TODO: API for set exact and set nearest
  }


  trait ROValueDecl[+X] extends ReadableValueDecl[X]
  trait WOValueDecl[-X] extends WritableValueDecl[X]
  trait RWValueDecl[X] extends ReadableValueDecl[X] with WritableValueDecl[X]

  type ReadableValue[+X] <: ReadableValueDecl[X]
  type WritableValue[-X] <: WritableValueDecl[X]

  type ROValue[+X] <: ROValueDecl[X]
  type WOValue[-X] <: WOValueDecl[X]
  type RWValue[X] <: RWValueDecl[X]

  trait ActionDecl extends SingleFeature
  type Action <: ActionDecl
}


object DeviceFeatures {
  type ReadResult[+T] = ContextValue[TASContext, T]

  def proxy[Feats <: DeviceFeatures : ClassTag, Dev <: FeatureDevice[Feats]](device: Dev): Feats = {
    val rtclass = classTag[Feats].runtimeClass
    val proxy = Proxy.newProxyInstance(rtclass.getClassLoader, Array(rtclass), new FeaturesInvokationHandler(device))
    proxy.asInstanceOf[Feats]
  }

  class FeaturesInvokationHandler[Feats <: DeviceFeatures : ClassTag](val device: FeatureDevice[Feats]) extends InvocationHandler {
    def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
      def noArgs = (args == null) || (args.length == 0)

      val result: Any = method.getName match {
        case "toString" => assert(false)
        case "equals"   => assert(false)
        case "hashCode" => assert(false)

        case _ =>
          val name = method.getName
          val arguments = args match { case null => Array.empty[AnyRef]; case args => args }
          val returnType = method.getReturnType
          if (classOf[DeviceFeatures#Feature].isAssignableFrom(returnType)) {
            println(s"Method ${method} called")
          } else throw new IllegalArgumentException(s"Method ${method} with return type ${returnType} not supported by proxy object")
      }
      result.asInstanceOf[AnyRef]
    }
  }
}



object FeaturesTest {
  trait MyDevice extends FeatureDevice[MyDevice.Features]


  object MyDevice extends FeatureDevice.Companion[MyDevice] {
    def impl = { case uri => new DeviceImpl(uri.toString) }

    trait Features extends DeviceFeatures {
      def foo: ROValue[Double]

      trait SomeFeatureGroup extends FeatureGroup {
        def baz: RWValue[Double]
      }

      def bar: SomeFeatureGroup
    }


    class DeviceImpl(uri: String) extends FeatureDevice.TAImpl[Features] with MyDevice {
      def identity(): scala.concurrent.Future[String] = successful(s"MyDevice($uri)")

      class FeaturesImpl extends AbstractFeaturesImpl with MyDevice.Features {
        val foo = ConstValue[Double](4.2)

        val bar = new SomeFeatureGroup {
          val baz = VarValue[Double](Some(4.2))
        }
      }

      val features = new FeaturesImpl

      //features.bar.baz.lastValue = 1.1
    }

  }


  def run(): Unit = {
    val dev: MyDevice = null
    dev.features.foo.readLast()
    dev.features.bar.baz.write(1.1)
  }
}
