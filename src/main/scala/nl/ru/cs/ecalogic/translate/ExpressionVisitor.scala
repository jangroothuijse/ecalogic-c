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
    false 
  }
  override def visit(node: FieldAccess) : Boolean = { 
    false 
  }
  override def visit(node: ThisExpression) : Boolean = { 
    false 
  }
  
  /*
   * Literals:
   */
  
  override def visit(node: BooleanLiteral) : Boolean = { 
    false 
  }
  override def visit(node: CharacterLiteral) : Boolean = { 
    false 
  }
  override def visit(node: StringLiteral) : Boolean = { 
    false 
  }
  override def visit(node: NumberLiteral) : Boolean = { 
    false 
  }
  override def visit(node: NullLiteral) : Boolean = { 
    false 
  }
  
  /*
   * From context:
   */
  
  override def visit(node: SimpleName) : Boolean = { 
    false 
  }
  override def visit(node: MethodInvocation) : Boolean = { 
    false 
  }
  
  /*
   * Operators:
   */
  
  override def visit(node: InfixExpression) : Boolean = { 
    false 
  }
  override def visit(node: PostfixExpression) : Boolean = { 
    false 
  }
  override def visit(node: PrefixExpression) : Boolean = { 
    false 
  }
  
  
}