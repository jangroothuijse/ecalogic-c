package nl.ru.cs.ecalogic
package translate

import org.eclipse.jdt.core.dom.Expression
import org.eclipse.jdt.core.dom.ArrayAccess
import org.eclipse.jdt.core.dom.FieldAccess
import org.eclipse.jdt.core.dom.BooleanLiteral
import org.eclipse.jdt.core.dom.CharacterLiteral
import org.eclipse.jdt.core.dom.StringLiteral
import org.eclipse.jdt.core.dom.NumberLiteral
import org.eclipse.jdt.core.dom.NullLiteral
import org.eclipse.jdt.core.dom.SimpleName
import org.eclipse.jdt.core.dom.MethodInvocation
import org.eclipse.jdt.core.dom.InfixExpression
import org.eclipse.jdt.core.dom.PostfixExpression
import org.eclipse.jdt.core.dom.PrefixExpression
import org.eclipse.jdt.core.dom.ThisExpression
import org.eclipse.jdt.core.dom.InfixExpression.Operator
import nl.ru.cs.ecalogic.ast.ASTArrayT

/**
 * Visits expressions, can cross over to Statements again using expression statement
 * an assignment in an expression would be such and expression statement.
 * 
 * Since ASTNode does not have such expression, we will have to model it as an extra statement preceding
 * the statement our current expression in on. So now every expression can be an expression statement and
 * emit an expression and thus every expression can can have multiple subexpressions that are expression
 * statements, meaning each expression has a list of these emitted statements.
 * 
 * Even worse, assignments are expressions in java jargon. They are not in eca.
 */
class ExpressionVisitor extends TranslateVisitor[(List[ast.Statement], ast.Expression)] {
  
  // i = (j += 2) < 3 ? j : j++;
  // =>
  // j'1 = j += 2
  // j'2 = j'1 + 1
  // i == j'1 < 3 ? j'1 : j'2;
  
  /**
   * Translating from a language where expression do have side effects to a language
   * without such side effects gives rise to renaming variables.
   * 
   * This rebind maps some original name, to a new name in the context representing
   * a variable of which the state has been changed. 
   */
  private val rebind: Map[String, String] = Map();
  val emittedAssignments: List[ast.Statement] = Nil
  var e: Option[ast.Expression] = None;
  
  def result(): Option[(List[ast.Statement], ast.Expression)] = { 
    e match { 
      case None => None; 
      case Some(expression) => Some (emittedAssignments, expression);
    }
  }
  /*
   * Accessing data structures 
   */
  
  override def visit(node: ArrayAccess) : Boolean = {
    new ExpressionVisitor().acceptResult(node.getArray) match {
      case None =>
      case Some(array) => new ExpressionVisitor().acceptResult(node.getIndex) match {
        case None =>
        case Some(index) => e = Some(ast.ArrayAccess(array._2, index._2))
      }
    }
    false 
  }
  override def visit(node: FieldAccess) : Boolean = { 
    new ExpressionVisitor().acceptResult(node.getExpression) match {
      case None =>
      case Some(struct) => e = Some(ast.StructAccess(struct._2, node.getName.getIdentifier))
    }
    false 
  }
  override def visit(node: ThisExpression) : Boolean = { 
    e = Some(ast.VarRef(ExpressionVisitor.thisName))
    false 
  }
  
 
  /*
   * Literals:
   */
  
  override def visit(node: BooleanLiteral) : Boolean = { 
    if (node.booleanValue()) e = Some(ast.Literal(new model.ECAValue(1)))
    else e = Some(ast.Literal(new model.ECAValue(0))) 
    false 
  }
  override def visit(node: CharacterLiteral) : Boolean = { 
    e = Some(ast.Literal(new model.ECAValue(node.charValue.toInt)))
    false 
  }
  override def visit(node: StringLiteral) : Boolean = { 
    e = Some(ast.StringConstant(node.getEscapedValue))
    false 
  }
  override def visit(node: NumberLiteral) : Boolean = {
    e = Some(ast.Literal(Integer.parseInt(node.getToken)))
    false 
  }
  override def visit(node: NullLiteral) : Boolean = {
    throw new ECAException("No null representative in ECA, the ECA Language does not support a default bottom type");
  }
  
  /*
   * From context:
   */
  
  override def visit(node: SimpleName) : Boolean = {
    e = Some(ast.VarRef(node.getIdentifier))
    false 
  }
  override def visit(node: MethodInvocation) : Boolean = { 
    val args : List[ast.Expression] = List();
    for (o : Object <- node.arguments.toArray()) {
      val e = o.asInstanceOf[Expression];
      new ExpressionVisitor().acceptResult(e) match {
        case None =>
        case Some(ae) => args.+:(ae._2);
      }
    }
    
    if (node.getExpression == null)
      e = Some(ast.FunCall(ast.FunName(node.getName().getFullyQualifiedName()), args));
    else new ExpressionVisitor().acceptResult(node.getExpression) match {
      case None => e = Some(ast.FunCall(ast.FunName(node.getName().getFullyQualifiedName()), args));
      case Some(context) => // for method invocation on objects, the object will be arg0:
        e = Some(ast.FunCall(ast.FunName(node.getName().getFullyQualifiedName()), context._2 :: args));
    }
    false
  }
  
  /*
   * Operators:
   */
  
  override def visit(node: InfixExpression) : Boolean = {
    new ExpressionVisitor().acceptResult(node.getLeftOperand) match {
      case None => 
      case Some(lhs) => new ExpressionVisitor().acceptResult(node.getRightOperand) match {
        case None => 
        case Some(rhs) => 
          if (node.getOperator == Operator.OR | node.getOperator == Operator.CONDITIONAL_OR) {
            e = Some(ast.Or(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.AND | node.getOperator == Operator.CONDITIONAL_AND) {
            e = Some(ast.And(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.PLUS) {
            e = Some(ast.Add(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.MINUS) {
            e = Some(ast.Subtract(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.TIMES) {
            e = Some(ast.Multiply(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.DIVIDE) {
            e = Some(ast.Divide(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.EQUALS) {
            e = Some(ast.EQ(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.NOT_EQUALS) {
            e = Some(ast.NE(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.LESS) {
            e = Some(ast.LT(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.LESS_EQUALS) {
            e = Some(ast.LE(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.GREATER) {
            e = Some(ast.GT(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.GREATER_EQUALS) {
            e = Some(ast.GE(lhs._2, rhs._2));
          } else if (node.getOperator == Operator.LEFT_SHIFT) {
            e = Some(ast.Multiply(lhs._2,
                 ast.Exponent(
                      ast.Literal(new model.ECAValue(2)),
                      rhs._2
                 )
            ))
          } else if (node.getOperator == Operator.RIGHT_SHIFT_SIGNED
              | node.getOperator == Operator.RIGHT_SHIFT_UNSIGNED) {
            e = Some(ast.Divide(lhs._2,
                 ast.Exponent(
                      ast.Literal(new model.ECAValue(2)),
                      rhs._2
                 )
            ))
          } else if (node.getOperator == Operator.REMAINDER) {
            e = Some(ast.Subtract(lhs._2, 
                ast.Multiply(rhs._2,
                     ast.Divide(lhs._2, rhs._2) 
                  )            
            ))
          } else throw new ECAException("Operator unsupported: " + node.getOperator.toString);
      }
    } 
      
    false 
  }
  /**
   *  Postfix operators: return old value
   */
  override def visit(node: PostfixExpression) : Boolean = { 
    new ExpressionVisitor().acceptResult(node.getOperand) match {
      case None => false
      case Some(a) =>
        if (node.getOperator == PostfixExpression.Operator.INCREMENT) {
          e = Some(ast.Subtract(a._2, ast.Literal(new model.ECAValue(1))))
          /*a._2 match {
            case ast.VarRef(name) =>
              emittedAssignments .:+ (ast.Assignment(name,
                   ast.Add(ast.VarRef(name), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.ArrayAccess(name, index) =>
              emittedAssignments .:+ (ast.ArrayAssign(name, index,
                   ast.Add(ast.ArrayAccess(name, index), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.StructAccess(struct, field) =>
              emittedAssignments .:+ (ast.StructAssign(struct, field,
                   ast.Add(ast.StructAccess(struct, field), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.UnionAccess(union, field) =>
              emittedAssignments .:+ (ast.UnionAssign(union, field,
                   ast.Add(ast.UnionAccess(union, field), ast.Literal(new model.ECAValue(1)))
              ))
          }*/
        } else if (node.getOperator == PostfixExpression.Operator.DECREMENT) {
          e = Some(ast.Add(a._2, ast.Literal(new model.ECAValue(1))))
          /*a._2 match {
            case ast.VarRef(name) =>
              emittedAssignments .:+ (ast.Assignment(name,
                   ast.Subtract(ast.VarRef(name), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.ArrayAccess(name, index) =>
              emittedAssignments .:+ (ast.ArrayAssign(name, index,
                   ast.Subtract(ast.ArrayAccess(name, index), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.StructAccess(struct, field) =>
              emittedAssignments .:+ (ast.StructAssign(struct, field,
                   ast.Subtract(ast.StructAccess(struct, field), ast.Literal(new model.ECAValue(1)))
              ))
            case ast.UnionAccess(union, field) =>
              emittedAssignments .:+ (ast.UnionAssign(union, field,
                   ast.Subtract(ast.UnionAccess(union, field), ast.Literal(new model.ECAValue(1)))
              ))
          }*/
        } else throw new ECAException("Operator unsupported: " + node.getOperator.toString);
    }
    false 
  }
  /** 
   *  Prefix operators: return new value
   */
  override def visit(node: PrefixExpression) : Boolean = {
    val one : ast.Literal = ast.Literal(new model.ECAValue(1));
    new ExpressionVisitor().acceptResult(node.getOperand) match {      
      case None => false
      case Some(a) => {
        if (node.getOperator == PrefixExpression.Operator.DECREMENT) {
          val ex : ast.Expression = a._2; 
          /*ex match {      Not needed for now and made the compiler crash...:( 
            case ast.VarRef(name) => {
              emittedAssignments .:+ (ast.Assignment(name, ast.Subtract(ast.VarRef(name), one)));
            }
            case ast.ArrayAccess(name, index) => {
              emittedAssignments .:+ (ast.ArrayAssign(name, index,
                   ast.Subtract(ast.ArrayAccess(name, index), one)
              ));
            }
            case ast.StructAccess(struct, field) => {
              emittedAssignments .:+ (ast.StructAssign(struct, field,
                   ast.Subtract(ast.StructAccess(struct, field), one)
              ));
            }
            case ast.UnionAccess(union, field) => {
              emittedAssignments .:+ (ast.UnionAssign(union, field,
                   ast.Subtract(ast.UnionAccess(union, field), one)
              ));
            }
          };*/
          //e = Some(a._2);
          e = Some(ex);
          false;
        } else if (node.getOperator == PrefixExpression.Operator.INCREMENT) {
          /*a._2 match {
            case ast.VarRef(name) => {
              emittedAssignments .:+ (ast.Assignment(name,
                   ast.Add(ast.VarRef(name), ast.Literal(new model.ECAValue(1)))
              ));
            }
            case ast.ArrayAccess(name, index) => {
              emittedAssignments .:+ (ast.ArrayAssign(name, index,
                   ast.Add(ast.ArrayAccess(name, index), ast.Literal(new model.ECAValue(1)))
              ));
            }
            case ast.StructAccess(struct, field) => {
              emittedAssignments .:+ (ast.StructAssign(struct, field,
                   ast.Add(ast.StructAccess(struct, field), ast.Literal(new model.ECAValue(1)))
              ));
            }
            case ast.UnionAccess(union, field) => {
              emittedAssignments .:+ (ast.UnionAssign(union, field,
                   ast.Add(ast.UnionAccess(union, field), ast.Literal(new model.ECAValue(1)))
              ));
            }
          };*/
          e = Some(a._2);
          false;
        } else if (node.getOperator == PrefixExpression.Operator.COMPLEMENT) {
          // due to two complements notations: -i == ~(i + 1)
          // so -(i-1) = ~i
          e = Some(ast.Subtract(ast.Literal(new model.ECAValue(0)), 
                ast.Subtract(a._2, ast.Literal(new model.ECAValue(1)))));
          false;
        } else if (node.getOperator == PrefixExpression.Operator.MINUS) {
          e = Some(ast.Subtract(ast.Literal(new model.ECAValue(0)), a._2));
          false;
        } else if (node.getOperator == PrefixExpression.Operator.NOT) { 
          e = Some(ast.Not(a._2));
          false;
        } else if (node.getOperator == PrefixExpression.Operator.PLUS) { 
          e = Some(a._2);
          false;
        } else throw new ECAException("Operator unsupported: " + node.getOperator.toString);
      }
    }
  }
  
  
}

object ExpressionVisitor {  
  /**
   * The name of the parameter that holds in a reference to 'this', in java its called this which
   * is why no other things can be called this and why this is a safe choice for an ecalogic
   * program representing a java program.
   */
  val thisName : String = "this"
}