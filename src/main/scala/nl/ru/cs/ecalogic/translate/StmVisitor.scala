package nl.ru.cs.ecalogic
package translate

// http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2FAST.html
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.IfStatement
import org.eclipse.jdt.core.dom.WhileStatement
import org.eclipse.jdt.core.dom.DoStatement
import org.eclipse.jdt.core.dom.ForStatement
import org.eclipse.jdt.core.dom.Statement
import org.eclipse.jdt.core.dom.Assignment
import org.eclipse.jdt.core.dom.Assignment.Operator
import nl.ru.cs.ecalogic.translate.TranslateVisitor
import nl.ru.cs.ecalogic.translate.NotImplementedVisitor;
import ast.If
import ast.Skip
import ast.While
import ast.Expression

import org.eclipse.jdt.core.dom.Block

class Stm extends TranslateVisitor[ast.Statement] {  
  val statements: Seq[ast.Statement] = List();
  
  /**
   * @see http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2FAssignment.html
   */
  override def visit(a: Assignment) : Boolean = {
    new NotImplementedVisitor().acceptResult(a.getLeftHandSide()) match {
      case None => 
      case Some(lhs) => new ExpressionVisitor().acceptResult(a.getRightHandSide()) match {
        case None => 
        case Some(rhs) => 
          // @see http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2FAssignment.Operator.html
          if (a.getOperator == Assignment.Operator.ASSIGN) {
            statements.+:(new ast.Assignment("", rhs._2))
          } else {
            // handle all other Operator. constants, by applying what every they do to rhs
          }
      }
    }
    false
  }
  
  /**
   * Handles block statements, will become a composition if its more than one.
   */
  override def visit(block : Block) : Boolean = {
    for (node <- block.statements().toArray()) {
      val result = new Stm().acceptResult(node.asInstanceOf[Statement])
      result match {
        case None => 
        case Some(v) => statements.+:(v) 
      }
    }
    false
  }
  
  override def visit(w: WhileStatement) : Boolean = {
    throw new ECAException("'while' not permitted, use a for loop and annotate its bound in an initializer.");
  }
  
  override def visit(d: DoStatement) : Boolean = {
    throw new ECAException("'do' not permitted, use a for loop and annotate its bound in an initializer.");
  }
  
  /**
   * Handles if statements
   */
  override def visit(ifthenelse : IfStatement) : Boolean = {
    new IfStm(ifthenelse).result() match { 
      case None => 
      case Some(node) => statements.+:(node)
    }
    false
  }
  
  // visit todo's:
  //   expression statement
  //   variable declaration
  //   return statement?
  
  def result(): Option[ast.Statement] = {
    statements.length match {
      case 0 => Option.empty
      case 1 => Some(statements.head);
      case _ => Some(new ast.Composition(statements)) 
    }
    
  }
}

class IfStm(node: IfStatement) extends TranslateVisitor[If] {

  def result(): Option[If] = {
    val predicateVisitor = new ExpressionVisitor
    node.getExpression.accept(predicateVisitor)
    predicateVisitor.result() match {
      case None => Option.empty
      case Some(p) => {        
        new Stm().acceptResult(node.getThenStatement)  match {
          case None => Option.empty
          case Some(t) => if (node.getElseStatement == null) {
            Some(new If(p._2, t, new Skip))
          } else {
            new Stm().acceptResult(node.getElseStatement) match {
              case None    => Some(new If(p._2, t, new Skip))
              case Some(e) => Some(new If(p._2, t, e))
            }
          }
        }
      }
    }
  }
}

class ForStm(node: ForStatement) extends TranslateVisitor[While] {
  def result(): Option[While] = {
    val initVisitor = new ExpressionVisitor
    val expressionVisitor = new ExpressionVisitor
    val updateVisotor = new ExpressionVisitor
    new Stm().acceptResult(node.getBody) match {
      case None => Option.empty
      case Some(body) => Option.empty
    }
  }
}