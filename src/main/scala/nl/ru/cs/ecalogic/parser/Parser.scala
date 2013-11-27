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
package parser

import ast._
import util.{Position, DefaultErrorHandler, ErrorHandler}
import Lexer.Tokens

import scala.annotation.tailrec
import scala.io.Source

import java.io.File

/** Parser for ECA programs.
  *
  * @param input        input string to parse
  * @param errorHandler error handler for reporting error messages
  *
  * @author Jascha Neutelings
  */
class Parser(input: String, protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) extends BaseParser {
  import Parser._

  override protected val ignored = Tokens.Comment | Tokens.Whitespace
  protected val lexer = new Lexer(input)
  private var lastLineNumber = 0

  protected def tryParse[A <: ASTNode](expected: Pattern, default: A)(follows: Pattern)(parser: PartialFunction[Token, A]): A = {
    val pos = position
    val res = tryParse {_ => unexpected(expected); default}(follows)(parser)
    res.withPosition(pos)
  }

  protected def tryParse[A >: ErrorNode <: ASTNode](expected: Pattern)(follows: Pattern)(parser: PartialFunction[Token, A]): A =
    tryParse[A](expected, ErrorNode())(follows)(parser)


  override protected def advance() {
    lastLineNumber = position.line
    super.advance()
  }

  // DO NOT USE FOR LOOKAHEAD!
  protected object EndOfLine extends Pattern {

    def unapply(token: Token) = matches(token)

    def matches(token: Token) = position.line > lastLineNumber

    override def toString = "<end-of-line>"

  }

  private def expectSeparator(follows: Pattern) {
    tryParse {_ => unexpected(Tokens.Semicolon | EndOfLine)} (follows) {
      case Tokens.Semicolon => advance()
      case EndOfLine()      =>
    }
  }

  def literal(follows: Pattern): Literal = tryParse(Tokens.Numeral, Literal(0)) (follows) {
    case Tokens.Numeral(n) => advance(); Literal(n)
  }

  /** Parses an identifier.
    *
    * Returns "&lt;error&gt;" in case of failure.
    *
    * @param follows follow set pattern
    * @return        identifier
    */
  def identifier(follows: Pattern): String = expect(Tokens.Identifier, "<error>")(follows)

  /** Parses a program.
    *
    * @return        program node
    */
  def program(follows: Pattern = Pattern.empty): Program = { // TODO: Make me reusable
    val pos = position
    val definitions = sequenceOf(funDef, Tokens.Function % "<function definition>")(Pattern.empty)
    Program(definitions).withPosition(pos)
  }

  /** Parses a function definition.
    *
    * @param follows follow set pattern
    * @return        function definition node
    */
  def funDef(follows: Pattern): FunDef = {
    val pos = position

    expect(Tokens.Function)(follows)

    val name = identifier(follows | Tokens.LParen)
    val params = parameters(follows | Tokens.Assign | First.statement)

    val body = funBody(name)(follows)

    FunDef(name, params, body).withPosition(pos)
  }

  def parameters(follows: Pattern): Seq[Param] = {
    val params = Seq.newBuilder[Param]
    if (current(Tokens.LParen)) {
      advance()
      if (!current(Tokens.RParen)) {
        var halt = false
        do {
          val paramPos = position
          val paramName = identifier(follows | Tokens.Comma | Tokens.RParen)

          params += Param(paramName).withPosition(paramPos)

          if (current(Tokens.Comma)) advance()
          else halt = true
        } while (!halt)
      }
      expect(Tokens.RParen)(follows)
    }
    params.result()
  }

  def funBody(funName: String)(follows: Pattern): Statement = tryParse[Statement](First.funBody)(follows) {
    case Tokens.Semicolon =>
      // Don't remove it
      Skip()
    case Tokens.Assign =>
      advance()
      val expr = expression(follows)

      Assignment(funName, expr)
    case t if First.statement.matches(t) =>
      val stmt = composition(follows | Tokens.End)

      expect(Tokens.End)(follows | Tokens.Function)
      expect(Tokens.Function)(follows)

      stmt
  }

  /** Parses a list of one or more statements.
    *
    * @param follows follow set pattern
    * @return        composition node
    */
  def composition(follows: Pattern): Statement = {
    sequenceOf[Statement](statement, First.statement)(follows) match {
      case Seq()              => Skip().withPosition(position)//errorHandler.error(new ECAException("Statement list can not be empty.", position)); ErrorNode()(position)
      case Seq(first)         => first
      case seq @ (first +: _) => Composition(seq).withPosition(first.position)
    }
  }

  def sequenceOf[T <: ASTNode](f: (Pattern) => T, first: Pattern)(follows: Pattern): Seq[T] = {
    val nodes = Seq.newBuilder[T]

    while (current(first)) {
      nodes += f(follows | Tokens.Semicolon)
      if (current(first) || (current(Tokens.Semicolon) && lookahead(first)))
        expectSeparator(follows | first)
    }

    optional(Tokens.Semicolon)
    nodes.result()
  }

  /** Parses a statement.
    *
    * Returns an [[nl.ru.cs.ecalogic.ast.ErrorNode]] in case of failure.
    *
    * @param follows follow set pattern
    * @return        statement node
    */
  def statement(follows: Pattern) = tryParse[Statement](First.statement)(follows) {
    case Tokens.If =>
      advance()
      val predicate = expression(follows | Tokens.Then)

      expect(Tokens.Then)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
      val consequent = composition(follows | Tokens.Else)

      expect(Tokens.Else)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
      val alternative = composition(follows | Tokens.End)

      expect(Tokens.End)(follows | Tokens.If)
      expect(Tokens.If)(follows)

      If(predicate, consequent, alternative)
    case Tokens.While =>
      advance()
      val predicate = expression(follows | Tokens.Bound)

      val rankingFunction = if (current(Tokens.Bound)) {
        advance()
        Some(expression(follows | Tokens.Do))
      } else
        None

      expect(Tokens.Do)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
      val consequent = composition(follows | Tokens.End)

      expect(Tokens.End)(follows | Tokens.While)
      expect(Tokens.While)(follows)

      While(predicate, rankingFunction, consequent)
    case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon | Tokens.Period) =>
      funCall(follows)
    case Tokens.Identifier(n) if lookahead(Tokens.Assign) =>
      advance(2)
      val expr = expression(follows)

      Assignment(n, expr)
    case Tokens.Skip =>
      advance()

      Skip()
  }

  /** Parses a function call.
    *
    * @param follows follow set pattern
    * @return        function call node
    */
  def funCall(follows: Pattern): FunCall = {
    val pos = position

    val compOrNamePart = identifier(follows | Tokens.ColonColon | Tokens.LParen)

    val name = current match {
      case Tokens.ColonColon =>
        advance()
        val namePart = identifier(follows | Tokens.LParen)

        FunName(namePart, Some(compOrNamePart))
      case Tokens.Period =>
        val className = StringBuilder.newBuilder ++= compOrNamePart
        do {
          advance()
          className += '.'
          className ++= identifier(follows | Tokens.Period | Tokens.ColonColon)
        } while (current(Tokens.Period))

        expect(Tokens.ColonColon)(follows | Tokens.Identifier)

        val namePart = identifier(follows | Tokens.LParen)

        FunName(namePart, Some(className.result()))
      case _ =>
        FunName(compOrNamePart)
    }

    expect(Tokens.LParen)(follows | Tokens.Identifier | Tokens.Numeral | Tokens.LParen | Tokens.RParen)
    val arguments = Seq.newBuilder[Expression]
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        arguments += expression(follows | Tokens.Comma | Tokens.RParen)

        if (current(Tokens.Comma)) advance()
        else halt = true
      } while(!halt)
    }
    expect(Tokens.RParen)(follows)

    FunCall(name, arguments.result()).withPosition(pos)
  }

  /** Parses a expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def expression(follows: Pattern): Expression = orExpr(follows)

  /** Parses a primary expression.
    *
    * Returns an [[nl.ru.cs.ecalogic.ast.ErrorNode]] in case of failure.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def primary(follows: Pattern) = tryParse[Expression](First.expression) (follows) {
    case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon | Tokens.Period) =>
      funCall(follows)
    case Tokens.Identifier(n) =>
      advance()

      VarRef(n)
    case Tokens.Numeral(v) =>
      advance()

      Literal(v)
    case Tokens.LParen =>
      advance()

      val expr = expression(follows | Tokens.RParen)
      expect(Tokens.RParen)(follows)

      expr
  }

  protected def expDivRightExpr(follows: Pattern) = literal(follows)

  def expExpr(follows: Pattern): Expression = {
    @tailrec
    def _expExpr(acc: Expression): Expression = current match {
      case Tokens.Exponent => advance(); _expExpr(Exponent(acc, expDivRightExpr(follows)).withPosition(acc.position))
      case _               => acc
    }
    _expExpr(primary(follows))
  }

  /** Parses an optional multiply-expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def multExpr(follows: Pattern): Expression = {
    @tailrec
    def _multExpr(acc: Expression): Expression = current match {
      case Tokens.Multiply => advance(); _multExpr(Multiply(acc, expExpr(follows)).withPosition(acc.position))
      case Tokens.Divide   => advance(); _multExpr(Divide(acc, expDivRightExpr(follows)).withPosition(acc.position))
      case _               => acc
    }
    _multExpr(expExpr(follows))
  }

  /** Parses an optional add- or subtract-expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def addExpr(follows: Pattern): Expression = {
    @tailrec
    def _addExpr(acc: Expression): Expression = current match {
      case Tokens.Plus  => advance(); _addExpr(Add     (acc, multExpr(follows)).withPosition(acc.position))
      case Tokens.Minus => advance(); _addExpr(Subtract(acc, multExpr(follows)).withPosition(acc.position))
      case _            => acc
    }
    _addExpr(multExpr(follows))
  }

  /** Parses an optional relative expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def relExpr(follows: Pattern): Expression = {
    val left = addExpr(follows)
    current match {
      case Tokens.LT => advance(); LT(left, addExpr(follows)).withPosition(left.position)
      case Tokens.LE => advance(); LE(left, addExpr(follows)).withPosition(left.position)
      case Tokens.GT => advance(); GT(left, addExpr(follows)).withPosition(left.position)
      case Tokens.GE => advance(); GE(left, addExpr(follows)).withPosition(left.position)
      case Tokens.EQ => advance(); EQ(left, addExpr(follows)).withPosition(left.position)
      case Tokens.NE => advance(); NE(left, addExpr(follows)).withPosition(left.position)
      case _         => left
    }
  }

  /** Parses an optional and-expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def andExpr(follows: Pattern): Expression = {
   @tailrec
    def _andExpr(acc: Expression): Expression = current match {
      case Tokens.And => advance(); _andExpr(And(acc, relExpr(follows)).withPosition(acc.position))
      case _          => acc
    }
    _andExpr(relExpr(follows))
  }

  /** Parses an optional or-expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def orExpr(follows: Pattern): Expression = {
    @tailrec
    def _orExpr(acc: Expression): Expression = current match {
      case Tokens.Or => advance(); _orExpr(Or(acc, andExpr(follows)).withPosition(acc.position))
      case _         => acc
    }
    _orExpr(andExpr(follows))
  }

}

object Parser {

  object First {

    val statement =
      Tokens.Skip       % "<skip statement>"  |
      Tokens.If         % "<if statement>"    |
      Tokens.While      % "<while statement>" |
      Tokens.Identifier % "<assignment>"      |
      Tokens.Identifier % "<function call>"

    val expression =
      Tokens.Numeral                                   |
      Tokens.Identifier % "<function call>"            |
      Tokens.Identifier % "<variable reference>"       |
      Tokens.LParen     % "<parenthesized expression>"

    val funBody = statement | Tokens.Assign | Tokens.Semicolon

  }

  def main(args: Array[String]) {
    import scala.util.control.Exception._

    val file = new File(args.headOption.getOrElse(sys.error("Missing argument.")))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    println(program.getOrElse(sys.exit(1)))
  }

}
