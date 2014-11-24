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
package ast

import model.ECAValue
import util.{Positional, Position}

/** Base sealed trait for all AST nodes.
  *
  * @author Jascha Neutelings
  */
sealed trait ASTNode extends Positional {
  private var _position = Position.default

  def position = _position

  def withPosition(p: Position): this.type = {
    _position = p
    this
  }

}

/** Dummy class representing an error during parsing. */
@unchecked
case class ErrorNode() extends PrimaryExpression with Statement


/** Class representing an entire ECA program. */
case class Program(imports: Map[String, Import], functions: Map[String, FunDef], defs: Map[String, Definition]) extends ASTNode

case class Import(namePath: Seq[String], alias: String) extends ASTNode {
  val qualifiedName = namePath.mkString(".")
}

sealed trait BasicFunction extends ASTNode {
  val name: String
  val parameters: Seq[String]
  val body: Statement
  val arity = parameters.length
  val isComponent: Boolean
}

/** Class representing a function definition. */
case class FunDef(name: String, parameters: Seq[String], body: Statement) extends BasicFunction {
  val isComponent = false
}

//Sealed trait for Definitions (structs, unions, enumerations, typedefs)
sealed trait Definition extends ASTNode {
	//Superclass
}

//Structure definition node
case class StructDef(name: String, fields: Map[String, ASTType]) extends Definition {
	
}

//Union definition node
case class UnionDef(name: String, fields: Map[String, ASTType]) extends Definition {
	
}

/** Base sealed trait for statement nodes. */
sealed trait Statement extends ASTNode {
  /** Returns the underlying unannotated statement.**/
  val underlying: Statement = this
  /** Collects all annotations for the underlying statement.**/
  val annotations: Map[Expression, Expression] = Map.empty
}

case class Annotated(elements: Map[Expression, Expression], statement: Statement) extends Statement {
  override val underlying: Statement = statement
  override val annotations: Map[Expression, Expression] = elements ++ underlying.annotations
}

/** Class representing a function definition. */
case class If(predicate: Expression, consequent: Statement, alternative: Statement) extends Statement

// TODO: documentatie afmaken
case class While(predicate: Expression, rankingLowerbound: Option[Expression], rankingUpperbound: Option[Expression], consequent: Statement) extends Statement

case class Assignment(variable: String, value: Expression) extends Statement

case class Composition(statements: Seq[Statement]) extends Statement

case class Skip() extends Statement

//Array declaration
case class ArrayDeclaration(name: Expression, subtype: ASTType, length: Expression) extends Statement

//Array element assign
case class ArrayAssign(name: Expression, index: Expression, value: Expression) extends Statement

//Struct field assign
case class StructAssign(struct: Expression, field: String, value: Expression) extends Statement

//Union field assign
case class UnionAssign(union: Expression, field: String, value: Expression) extends Statement

//Break statement
case class Break() extends Statement

//Continue statement
case class Continue() extends Statement

//Base sealed trait for types
sealed trait ASTType extends ASTNode {
	val name: String = "undef"
}

//Primitive supertypes
case class ASTIntegerT () extends ASTType { override val name = "integer" }
case class ASTRealT () extends ASTType { override val name = "real" }
case class ASTVoidT () extends ASTType { override val name = "void" }

//Array types
case class ASTArrayT(sub: ASTType) extends ASTType { override val name = "array[" + sub.name + "]" }

//Pointer types
case class ASTPointerT(sub: ASTType) extends ASTType { override val name = sub.name + "*" }

//Struct type
case class ASTStructT(struct: String) extends ASTType { override val name = "struct " + struct }

//Union type
case class ASTUnionT(union: String) extends ASTType { override val name = "union " + union }

sealed trait Expression extends ASTNode {
  val arity: Int
  val operands: Seq[Expression]
  
  override def equals(other: Any) = {
	if (getClass() == other.getClass()) {
		var i = 0;
		var res = true;
		for (exp <- operands) {
			if (!(exp == other.asInstanceOf[Expression].operands.apply(i))) {
				res = false;
			}
			i = i + 1;
		}
		res
	} else {
		false
	}
  }

  def rewrite(ops: Seq[Expression]): Expression
  
  def transform(f: PartialFunction[Expression, Expression]): Expression =
    f.applyOrElse(rewrite(operands.map(_.transform(f))), identity[Expression])

  def foreach(f: PartialFunction[Expression, Unit]) {
    f.applyOrElse(this, (_:Expression)=>())
    operands.foreach(_.foreach(f))
  }
  
  val repr: String = "undef"
}


sealed trait PrimaryExpression extends Expression {
  val arity = 0
  val operands: Seq[Expression] = Seq()

  def rewrite(ops: Seq[Expression]) = this
}

case class Literal(value: ECAValue) extends PrimaryExpression { 
	override val repr = {
		if (value.underlying.toInt.toDouble == value.underlying)
			value.underlying.toInt.toString()
		else
			value.underlying.toString()
	}
	
	override def equals(other: Any) = {
		if (other.isInstanceOf[Literal]) {
			value == other.asInstanceOf[Literal].value
		} else {
			false
		}
	}
}

case class StringConstant(value: String) extends PrimaryExpression {
	override val repr = "\"" + value + "\""
	
	override def equals(other: Any) = other.isInstanceOf[StringConstant] && other.asInstanceOf[StringConstant].value == value
}

case class VarRef(name: String) extends PrimaryExpression { 
	override val repr = name
	
	override def equals(other: Any) = other.isInstanceOf[VarRef] && other.asInstanceOf[VarRef].name == name
}

case class SizeOfType(typ: ASTType) extends PrimaryExpression { 
	override val repr = "sizeof(" + typ.name + ")" 
	
	//Cursory
	override def equals(other: Any) = other.isInstanceOf[SizeOfType] && other.asInstanceOf[SizeOfType].repr == repr
}

case class SizeOfExpr(expr: Expression) extends PrimaryExpression { 
	override val repr = "sizeof(" + expr.repr + ")" 
	override def rewrite(ops: Seq[Expression]) = copy(expr = ops.apply(0))
	override val operands = Seq(expr)
}

case class SizeOfString(str: String) extends PrimaryExpression { 
	override val repr = "sizeof(\"" + str + "\")" 
	override def equals(other: Any) = other.isInstanceOf[SizeOfString] && other.asInstanceOf[SizeOfString].str == str
}

case class ToNum(expr: Expression) extends PrimaryExpression { 
	override val repr = "numeric(" + expr.repr + ")" 
	override def rewrite(ops: Seq[Expression]) = copy(expr = ops.apply(0))
	override val operands = Seq(expr)
}

//Array element access
case class ArrayAccess(name: Expression, index: Expression) extends PrimaryExpression {
	val accoperands = Seq(name, index)
	override val repr = name.repr + "[" + index.repr + "]"
	override def rewrite(ops: Seq[Expression]) = copy(name = ops.apply(0), index = ops.apply(1))
	override val operands = Seq(name, index)
}

//Struct field access
case class StructAccess(struct: Expression, field: String) extends PrimaryExpression {
	val accoperands = Seq(struct)
	override val repr = struct.repr + "." + field
	override def rewrite(ops: Seq[Expression]) = copy(struct = ops.apply(0))
	override val operands = Seq(struct)
	override def equals(other: Any) = other.isInstanceOf[StructAccess] && other.asInstanceOf[StructAccess].struct == struct && other.asInstanceOf[StructAccess].field == field
}

//Union field access
case class UnionAccess(union: Expression, field: String) extends PrimaryExpression {
	val accoperands = Seq(union)
	override val repr = union.repr + "." + field
	override def rewrite(ops: Seq[Expression]) = copy(union = ops.apply(0))
	override val operands = Seq(union)
	override def equals(other: Any) = other.isInstanceOf[UnionAccess] && other.asInstanceOf[UnionAccess].union == union && other.asInstanceOf[UnionAccess].field == field
}

//Start of an array expression
case class StartOfArray(name: String) extends PrimaryExpression {
	override val repr = "startof(\"" + name + "\")"
	override def equals(other: Any) = other.isInstanceOf[StartOfArray] && other.asInstanceOf[StartOfArray].name == name
}

sealed trait NAryExpression extends Expression {
  val operatorName: String
}

sealed trait BinaryExpression extends NAryExpression {
  val arity = 2
  val left: Expression
  val right: Expression
  val operands = Seq(left, right)
  def operator: (ECAValue, ECAValue) => ECAValue
}

sealed trait UnaryExpression extends NAryExpression {
  val arity = 1
  def operand: Expression
  val operands = Seq(operand)
  def operator: ECAValue => ECAValue
}

//Length of an array; only usable in annotations
case class ArrayLength(name: Expression) extends UnaryExpression {
	def operand = name
	override val operands = Seq(name)
	val operatorName = "length"
	
	override val repr = "length(" + name.repr + ")"
	def operator = (v => v)
	
	override def rewrite(ops: Seq[Expression]) = copy(name = ops(0))
}

//Dereference expression
case class Dereference(exp: Expression) extends UnaryExpression {
	def operand = exp
	val operatorName = "*"
	
	override val repr = "deref(" + exp.repr + ")"
	def operator = (v => v)
	
	override def rewrite(ops: Seq[Expression]) = copy(exp = ops(0))
}

//Reference expression
case class Reference(exp: Expression) extends UnaryExpression {
	def operand = exp
	val operatorName = "&"
	
	override val repr = "ref(" + exp.repr + ")"
	def operator = (v => v)
	
	override def rewrite(ops: Seq[Expression]) = copy(exp = ops(0))
}

sealed trait LogicalExpression extends NAryExpression

case class Or(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  val operatorName = "or"
  def operator = _ || _

  override def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class And(left: Expression, right: Expression) extends BinaryExpression with LogicalExpression {
  val operatorName = "and"
  def operator = _ && _

  override def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Not(sub: Expression) extends UnaryExpression with LogicalExpression {
	def operand = sub
	def operator = !_
	val operatorName = "not"
	
	override def rewrite(ops: Seq[Expression]) = copy(sub = ops(0)).withPosition(position)
}


sealed trait ArithmeticExpression extends NAryExpression

case class Add(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  val operatorName = "+"
  def operator = _ + _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Subtract(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  val operatorName = "-"
  def operator = _ - _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Multiply(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  val operatorName = "*"
  def operator = _ * _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Divide(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  val operatorName = "/"
  def operator = _ / _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class Exponent(left: Expression, right: Expression) extends BinaryExpression with ArithmeticExpression {
  val operatorName = "^"
  def operator = _ ** _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}


sealed trait RelationalExpression extends BinaryExpression

case class EQ(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = "="
  def operator = _ == _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class NE(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = "<>"
  def operator = _ != _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class LT(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = "<"
  def operator = _ < _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class LE(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = "<="
  def operator = _ <= _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class GT(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = ">"
  def operator = _ > _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class GE(left: Expression, right: Expression) extends RelationalExpression {
  val operatorName = ">="
  def operator = _ >= _

  def rewrite(ops: Seq[Expression]) = copy(left = ops(0), right = ops(1)).withPosition(position)
}

case class FunName(name: String, prefix: Option[String] = None) {
  val qualified = prefix.map(_ + "::").getOrElse("") + name
  val isPrefixed = prefix.isDefined

  override val toString = qualified
  
  override def equals(other: Any) = other.isInstanceOf[FunName] && other.asInstanceOf[FunName].name == name && other.asInstanceOf[FunName].prefix == prefix
}

case class FunCall(name: FunName, arguments: Seq[Expression]) extends NAryExpression with Statement {
  val operatorName = name.qualified
  val arity = arguments.size
  val operands = arguments

  def rewrite(ops: Seq[Expression]) = copy(arguments = ops).withPosition(position)
  
  override def equals(other: Any) = {
	if (other.isInstanceOf[FunCall]) {
		var res = name == other.asInstanceOf[FunCall].name;
		var i = 0;
		for (exp <- operands) {
			if (!(exp == other.asInstanceOf[FunCall].operands(i))) {
				res = false;
			}
			i = i + 1;
		}
		res
	} else {
		false
	}
  }	
}





sealed trait ModelASTNode extends ASTNode

case class Initializer(name: String, value: Literal) extends ModelASTNode

case class CompVarDecl(name: String, lower: ECAValue, upper: ECAValue, initializer: Option[Initializer]) extends ModelASTNode {
  val initialValue = initializer.map(_.value.value)
}

case class Component(name: String,
                     imports: Map[String, Import],
                     variables: Map[String, CompVarDecl],
                     componentFunctions: Map[String, CompFunDef],
                     functions: Map[String, FunDef]) extends ModelASTNode

case class CompFunDef(name: String, parameters: Seq[String], energy: ECAValue, time: ECAValue, body: Statement) extends ModelASTNode with BasicFunction {
  val isComponent = true
}
