 /*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package nl.ru.cs.ecalogic
package model

/*

  COMMENT/IDEA:

     we could use reflection to implement ComponentStates, e.g.:
     trait ComponentState
     class MyComponentState extends ComponentState {
       var x: IntType
       var t: TimeStamp
     }

     howwwwwwwwever. it seems to be a recent addition to the language, so perhaps
     it is risky to use this 'in production'?

 */

// isn't it useful to keep as a base class in a place we can access it?

// use ComponentModel#ComponentState for a path independent reference
// or c.ComponentState where c is an instance of a ComponentModel for a path dependent reference
// I don't see the problem of using an inner class

/**
 * @author Marc Schoolderman
 * @author Jascha Neutelings
 */
trait ComponentModel {

  type State <: ComponentState

  trait ComponentState extends PartiallyOrdered[State] {

    val elements: Map[String, ECAValue]

    lazy val (timestamps, integers) = elements.partition {
      case (name, _) => isTimestamp(name)
    }

    def tryCompareTo[B >: State <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
      case that: ComponentState =>
        var sign = 0
        for (key <- integers.keys) {
          val cmp = integers(key) compare that.integers(key)
          if (sign*cmp < 0) return None // note: a*b<0 iff a,b have different signs
          else sign |= cmp
        }
        for (key <- timestamps.keys) {
          val cmp = -(timestamps(key) compare that.timestamps(key))
          if (sign*cmp < 0) return None
          else sign |= cmp
        }
        Some(sign)
      case _ => None
    }

    protected def update(newElements: Map[String, ECAValue]): State

    private[ComponentModel] def _update(newElements: Map[String, ECAValue]) = update(newElements)

    override def equals(that: Any) = that match {
      case that: ComponentState => tryCompareTo(that) == Some(0)
      case _                    => false
    }

    override def hashCode = elements.hashCode

    override def toString = elements.map {
      case (name, value) => s"$name = $value"
    }.mkString("(", ", ", ")")

  }

  val name: String

  val initialState: State

  protected def isTimestamp(name: String): Boolean

  def lub(a: State, b: State): State =
    initialState._update(
      a.integers.  keys.map(key => key -> (a.integers  (key) max b.integers  (key))).toMap ++
      a.timestamps.keys.map(key => key -> (a.timestamps(key) min b.timestamps(key))).toMap)

  def r(s: State, old: State): State = initialState._update(s.integers ++ old.timestamps)

  def td(s: State, t: ECAValue): ECAValue = ECAValue.Zero

// humorous: convergent evolution in program design:

/*
MARC:
  //according to the paper, this should be the signature:
  //def rc(fun: String, gamma: Set[ComponentState], delta: GlobalState) =

  //however, i currently fail to see why this is necessary
JASCHA:
  // Does not actually operate on the set of component states, but only the component state that belongs to this model.
  // Might be sufficient?
*/

  def rc(fun: String)(gamma: State, delta: GlobalState, args: Seq[ECAValue] = Seq.empty): (State, GlobalState) = (gamma, delta)

}





abstract class LinearComponentModel(val name: String) extends ComponentModel {

  case class State(content: Map[String, ECAValue] = Map.empty, power: ECAValue = 0, tau: ECAValue = 0) extends ComponentState {
    val elements: Map[String, ECAValue] =
      content + ("power" -> power) + ("tau" -> tau)

    def this(elements: Map[String, ECAValue]) =
      this(elements -- Seq("power", "tau"), elements.getOrElse("power", 0), elements.getOrElse("tau", 0))

    protected def update(newElements: Map[String, ECAValue]): State = State(elements ++ newElements)

    def update(level: ECAValue, delay: ECAValue) = State(content, level, tau+delay)

  }

  protected def isTimestamp(name: String) = name == "tau"

  override def td(s: State, t: ECAValue) = s.power * ((t - s.tau) max 0)

}

object DemoComponent extends LinearComponentModel("Demo") {

  val initialState = State()

  override def rc(fun: String)(gamma: State, delta: GlobalState, args: Seq[ECAValue]): (State, GlobalState) = {
    fun match {
      case "on"  => (gamma.update(1, 0), delta)
      case "off" => (gamma.update(0, 0), delta)
      case "idle"=> (gamma.update(gamma.power, 1), delta)
      case _     => (gamma, delta)
    }
  }

}
