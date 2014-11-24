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

class ModelLexer(_input: String) extends Lexer(_input) {
  import ModelLexer.Tokens._

  override protected def parseToken = {
    val parseToken: PartialFunction[Char, (Token, Int)] = {
      case ':' if !lookahead('=') && !lookahead(':') => (Colon       , 1)
      case '.' if lookahead('.')                     => (PeriodPeriod, 2)
    }
    parseToken orElse super.parseToken
  }

  override protected def keywords =  super.keywords ++ Set(Class, Uses, Initial, Time, Energy)

}

object ModelLexer {

  object Tokens {
    // Make base tokens available
    val EndOfFile  = Lexer.Tokens.EndOfFile
    val Unknown    = Lexer.Tokens.Unknown
    val Import     = Lexer.Tokens.Import
    val Component  = Lexer.Tokens.Component
    val Function   = Lexer.Tokens.Function
    val Identifier = Lexer.Tokens.Identifier
    val Numeral    = Lexer.Tokens.Numeral
    val LParen     = Lexer.Tokens.LParen
    val RParen     = Lexer.Tokens.RParen
    val Comma      = Lexer.Tokens.Comma
    val End        = Lexer.Tokens.End
    val Assign     = Lexer.Tokens.Assign

    case object Class        extends Keyword("class")

    case object Uses         extends Keyword("uses")
    case object Time         extends Keyword("time")
    case object Energy       extends Keyword("energy")

    case object Initial      extends Keyword("initial")

    case object Colon        extends FixedToken(":")
    case object PeriodPeriod extends FixedToken("..")

  }

}
