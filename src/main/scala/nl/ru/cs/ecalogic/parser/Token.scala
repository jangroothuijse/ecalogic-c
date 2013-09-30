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

package nl.ru.cs.ecalogic.parser

import scala.reflect.ClassTag

trait Pattern {
  def matches(token: Token): Boolean

  def |(pattern: Pattern): Pattern = Pattern.union(this, pattern)
}

object Pattern {
  private case class MultiPattern(patterns: Set[Pattern]) extends Pattern {
    def matches(token: Token) = patterns.exists(_.matches(token))
  }

  private case object NilPattern extends Pattern {
    def matches(token: Token) = false
  }

  def empty: Pattern = NilPattern

  def union(patterns: Set[Pattern]): Pattern =  {
    val patSet = patterns.flatMap {
      case NilPattern       => Set[Pattern]()
      case MultiPattern(ps) => ps
      case p                => Set(p)
    }
    if      (patSet.isEmpty)   NilPattern
    else if (patSet.size == 1) patSet.head
    else                       MultiPattern(patSet)
  }

  def union(patterns: Pattern*): Pattern = union(patterns.toSet)
}

abstract class TypePattern[T <: Token : ClassTag] extends Pattern {
  def matches(token: Token) = implicitly[ClassTag[T]].runtimeClass.isInstance(token)
}



trait Token extends Pattern {
  def matches(token: Token) = this == token
}

abstract class FixedToken(fixedValue: String) extends Token {
  override def toString = s"'$fixedValue'"
}

abstract class Keyword(val keyword: String) extends FixedToken(keyword)

abstract class VariableToken[T](name: String) extends Token {
  def value: T

  override def toString = s"'$value' ($name)"
}