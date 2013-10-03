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

import scala.annotation.tailrec
import scala.util.control.Exception._

import nl.ru.cs.ecalogic.util.{Position, DefaultErrorHandler, ErrorHandler}
import nl.ru.cs.ecalogic.ast._
import nl.ru.cs.ecalogic.ECAException
import scala.io.Source
import java.io.File
import nl.ru.cs.ecalogic.parser.Lexer.Tokens

/** Parser for ECA programs.
  *
  * @param input        input string to parse
  * @param errorHandler error handler for resporting error messages
  *
  * @author Jascha Neutelings
  */
final class Parser(input: String, protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) extends BaseParser {
  override protected val ignored = Tokens.Comment | Tokens.Whitespace
  protected val lexer = new Lexer(input)

  private def parse[A >: ErrorNode <: ASTNode](expected: Pattern)(follows: Pattern)(parser: PartialFunction[Token, Position => A]): A = {
    val pos = position
    val res = parse[(Position => A), (Position => ErrorNode)]{_ => unexpected(expected); ErrorNode()}(follows)(parser)
    res(pos)
  }

  /** Parses an identifier.
    *
    * Returns "&lt;error&gt;" in case of failure.
    *
    * @param follows follow set pattern
    * @return        identifier
    */
  def identifier(follows: Pattern): String =
    parse {_ => unexpected(Tokens.Identifier); "<error>"} (follows) {
      case Tokens.Identifier(n) => advance(); n
    }


  /** Parses a program.
    *
    * @param follows follow set pattern
    * @return        program node
    */
  def program(follows: Pattern = Pattern.empty): Program = {
    val pos = position
    val definitions = Seq.newBuilder[FunDef]
    while (!current(Tokens.EndOfFile)) {
      definitions += funDef(follows)
    }
    Program(definitions.result())(pos)
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

    val params = Seq.newBuilder[Param]
    expect(Tokens.LParen)(follows | Tokens.Identifier | Tokens.RParen)
    if (!current(Tokens.RParen)) {
      var halt = false
      do {
        val paramPos = position
        val paramName = identifier(follows | Tokens.Comma | Tokens.RParen)

        params += Param(paramName)(paramPos)

        if (current(Tokens.Comma)) advance()
        else halt = true
      } while (!halt)
    }
    expect(Tokens.RParen)(follows | Tokens.Returns)
    expect(Tokens.Returns)(follows | Tokens.Identifier)

    val resultPos = position
    val result = VarRef(identifier(follows | Tokens.Semicolon | Tokens.If | Tokens.While | Tokens.Skip | Tokens.Identifier))(resultPos)
    optional(Tokens.Semicolon)

    val body = composition(follows | Tokens.End)
    optional(Tokens.Semicolon)

    expect(Tokens.End)(follows | Tokens.Function)
    expect(Tokens.Function)(follows)

    FunDef(name, params.result(), result, body)(pos)
  }

  /** Parses a list of one or more statements.
    *
    * @param follows follow set pattern
    * @return        composition node
    */
  def composition(follows: Pattern): Statement = {
    val first = statement(follows | Tokens.Semicolon)

    if (current(Tokens.Semicolon) && !lookahead(Tokens.End | Tokens.Else)) {
      val statements = Seq.newBuilder += first

      do {
        advance()
        statements += statement(follows | Tokens.Semicolon)
      } while (current(Tokens.Semicolon) && !lookahead(Tokens.End | Tokens.Else))

      Composition(statements.result())(first.position)
    } else
      first
  }

  /** Parses a statement.
    *
    * Returns an [[nl.ru.cs.ecalogic.ast.ErrorNode]] in case of failure.
    *
    * @param follows follow set pattern
    * @return        statement node
    */
  def statement(follows: Pattern) =
    parse[Statement]( Tokens.Skip       % "<skip statement>"
                    | Tokens.If         % "<if statement>"
                    | Tokens.While      % "<while statement>"
                    | Tokens.Identifier % "<assignment>"
                    | Tokens.Identifier % "<function call>"
                    ) (follows) {
      case Tokens.If =>
        advance()
        val predicate = expression(follows | Tokens.Then)

        expect(Tokens.Then)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val consequent = composition(follows | Tokens.Then | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.Else)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val alternative = composition(follows | Tokens.End | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.End)(follows | Tokens.If)
        expect(Tokens.If)(follows)

        If(predicate, consequent, alternative)
      case Tokens.While =>
        advance()
        val predicate = expression(follows | Tokens.Upto)

        expect(Tokens.Upto)(follows | Tokens.Identifier | Tokens.Numeral | Tokens.LParen)
        val rankingFunction = expression(follows | Tokens.Do)

        expect(Tokens.Do)(follows | Tokens.Identifier | Tokens.Skip | Tokens.If | Tokens.While)
        val consequent = composition(follows | Tokens.End | Tokens.Semicolon)
        optional(Tokens.Semicolon)

        expect(Tokens.End)(follows | Tokens.While)
        expect(Tokens.While)(follows)

        While(predicate, rankingFunction, consequent)
      case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon) =>
        _ => funCall(follows)
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

    val name = if (current(Tokens.ColonColon)) {
      advance()
      val namePart = identifier(follows | Tokens.LParen)

      FunName(namePart, Some(compOrNamePart))
    } else
      FunName(compOrNamePart)

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

    FunCall(name, arguments.result())(pos)
  }

  /** Parses a expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def expression(follows: Pattern): Expression = orExpr(follows)()

  /** Parses a primary expression.
    *
    * Returns an [[nl.ru.cs.ecalogic.ast.ErrorNode]] in case of failure.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def primary(follows: Pattern) =
    parse[Expression]( Tokens.Numeral
                     | Tokens.Identifier % "<function call>"
                     | Tokens.Identifier % "<variable reference>"
                     | Tokens.LParen     % "<parenthesized expression>"
                     ) (follows) {
      case Tokens.Identifier(n) if lookahead(Tokens.LParen | Tokens.ColonColon) =>
        _ => funCall(follows)
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

        _ => expr
    }

  /** Parses an optional multiply-expression.
    *
    * @param follows follow set pattern
    * @param acc     accumulator for tail recursion
    * @return        expression node
    */
  @tailrec
  def multExpr(follows: Pattern)(acc: Expression = primary(follows)): Expression = current match {
    case Tokens.Multiply => advance(); multExpr(follows)(Multiply(acc, primary(follows))(acc.position))
    case _               => acc
  }

  /** Parses an optional add- or subtract-expression.
    *
    * @param follows follow set pattern
    * @param acc     accumulator for tail recursion
    * @return        expression node
    */
  @tailrec
  def addExpr(follows: Pattern)(acc: Expression = multExpr(follows)()): Expression = current match {
    case Tokens.Plus  => advance(); addExpr(follows)(Add     (acc, multExpr(follows)())(acc.position))
    case Tokens.Minus => advance(); addExpr(follows)(Subtract(acc, multExpr(follows)())(acc.position))
    case _            => acc
  }

  /** Parses an optional relative expression.
    *
    * @param follows follow set pattern
    * @return        expression node
    */
  def relExpr(follows: Pattern): Expression = {
    val left = addExpr(follows)()
    current match {
      case Tokens.LT => advance(); LT(left, addExpr(follows)())(left.position)
      case Tokens.LE => advance(); LE(left, addExpr(follows)())(left.position)
      case Tokens.GT => advance(); GT(left, addExpr(follows)())(left.position)
      case Tokens.GE => advance(); GE(left, addExpr(follows)())(left.position)
      case Tokens.EQ => advance(); EQ(left, addExpr(follows)())(left.position)
      case Tokens.NE => advance(); NE(left, addExpr(follows)())(left.position)
      case _         => left
    }
  }

  /** Parses an optional and-expression.
    *
    * @param follows follow set pattern
    * @param acc     accumulator for tail recursion
    * @return        expression node
    */
  @tailrec
  def andExpr(follows: Pattern)(acc: Expression = relExpr(follows)): Expression = current match {
    case Tokens.And => advance(); andExpr(follows)(And(acc, relExpr(follows))(acc.position))
    case _          => acc
  }

  /** Parses an optional or-expression.
    *
    * @param follows follow set pattern
    * @param acc     accumulator for tail recursion
    * @return        expression node
    */
  @tailrec
  def orExpr(follows: Pattern)(acc: Expression = andExpr(follows)()): Expression = current match {
    case Tokens.Or => advance(); orExpr(follows)(Or(acc, andExpr(follows)())(acc.position))
    case _         => acc
  }

}

object Parser {

  def main(args: Array[String]) {
    val file = new File(args.headOption.getOrElse("zooi/test.eca"))
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler(source = Some(source), file = Some(file))
    val parser = new Parser(source, errorHandler)
    val program = catching(classOf[ECAException]).opt(parser.program()).filterNot(_ => errorHandler.errorOccurred)
    println(program.getOrElse(sys.exit(1)))
  }

}