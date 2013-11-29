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

import util.Polynomial
import config.Options.{Model => config}

/**
 * @author Marc Schoolderman
 * @author Jascha Neutelings
 */
trait ComponentModel { model =>

  // Utility types
  type TDFunction  = (EACState, Polynomial) => Polynomial
  type LUBFunction = (EACState, EACState) => EACState
  type PHIFunction = CState => ECAValue
  type DFunction   = CState => CState
  type RVFunction  = (CState, Seq[ECAValue]) => ECAValue

  type CState <: ComponentState
  trait ComponentState extends PartiallyOrdered[CState] {

    val elements: Map[String, ECAValue]

    def tryCompareTo[B >: CState <% PartiallyOrdered[B]](that: B): Option[Int] = that match {
      case that: ComponentState =>
        var sign = 0
        for (key <- elements.keys) {
          val cmp = elements(key) compare that.elements(key)
          if (sign* cmp < 0) return None // note: a*b<0 iff a,b have different signs
          else sign |= cmp
        }
        Some(sign)
      case _ => None
    }

    protected def update(newElements: Map[String, ECAValue]): CState

    private[ComponentModel] def update(newElements: Iterable[(String, ECAValue)]): CState = update(newElements.toMap)

    override def equals(that: Any) = that match {
      case that: ComponentState => tryCompareTo(that) == Some(0)
      case _                    => false
    }

    override def hashCode = elements.hashCode

    override def toString = elements.map {
      case (name, value) => s"$name = $value"
    }.mkString("(", ", ", ")")

  }

  case class EACState(state: CState, timestamp: Polynomial, energy: Polynomial) {
    def s = state
    def t = timestamp
    def e = energy

    def update(f: String, t1: Polynomial): (EACState, Polynomial) = {
      val e1 = e + E(f)
      val t2 = t1 + T(f)
      val s1 = delta(f)(s)
      phiCheck(s, s1)

      if(s1 != s || config.alwaysUpdate) {
        val upd = EACState(s1, t1, e1 + td(this,t1))
        if(config.alwaysForwardTime)
          // not only update, but set it to the most recent
          (EACState(s1, t2, upd.e + td(upd, t2)), t2)
        else
          (upd, t2)
      } else
        // do not update the timestamp if the state did not change
        (EACState(s, t, e1), t2)
    }

    def forward(t1: Polynomial) =
      EACState(s, t max t1, e + td(this,t1))

    def reset = EACState(s, 0, 0)

    def update(timestamp: Polynomial, energy: Polynomial) = EACState(s, timestamp, energy)

    // why not define the lub here in the first place?
    def lub(that: ComponentModel#EACState) =
      // there has to be a better way?
      model.lub(this, that.asInstanceOf[EACState])

    /* checks if monotonicity of phi holds for s1 => s2 */
    def phiCheck(s1: CState, s2: CState) {
      val stOrder  = s1 tryCompareTo s2
      val phiOrder = phi(s1) compareTo phi(s2)
      // check if the signs of the comparisons differ
      if(stOrder.exists(_ * phiOrder < 0))
        throw new ECAException(s"$name::phi not monotone with respect to $s1 and $s2")
    }
  }

  val name: String

  val initialState: CState

  def E(f: String) = ECAValue.Zero

  def T(f: String) = ECAValue.Zero

  def initialEACState(timestamp: ECAValue = 0, energy: ECAValue = 0) =
    EACState(initialState, timestamp, energy)

  def lub(a: EACState, b: EACState): EACState = {
    val EACState(sa, ta, ea) = a
    val EACState(sb, tb, eb) = b

    EACState(
      sa.update(sa.elements.keys.map(key => key -> (sa.elements(key) max sb.elements(key)))),
      ta min tb,
      ea max eb
    )
  }

  // comparing t >= g.t is dubious since Polynomials are not totally ordered. 
  // this next code is the only spot that requires a comparison on them.
  // we should figure out if we *really* need this

  def td(g: EACState, t: Polynomial): Polynomial = if (t >= g.t) (t - g.t) * phi(g.s) else ECAValue.Zero

  def delta(f: String)(s: CState) = s

  def phi(s: CState) = ECAValue.Zero

  def rv(f: String)(s: CState, a: Seq[ECAValue]) = ECAValue.Zero

}

/** Marker for CPU components. */
trait CPUComponent { this: ComponentModel => }
