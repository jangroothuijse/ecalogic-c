package nl.ru.cs.ecalogic
package translate.statement

import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.IfStatement
import org.eclipse.jdt.core.dom.WhileStatement
import org.eclipse.jdt.core.dom.Statement
import nl.ru.cs.ecalogic.translate.TranslateVisitor
import nl.ru.cs.ecalogic.translate.NotImplementedVisitor;
import ast.If
import ast.Skip
import ast.Expression

import org.eclipse.jdt.core.dom.Block


class Stm extends TranslateVisitor[ast.Statement] {  
  val statements: Seq[ast.Statement] = List();
  
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
  
  /**
   * Handles if statements
   */
  override def visit(ifthenelse : IfStatement) : Boolean = {
    new IfStm(ifthenelse).result() match { 
      case None => 
      case Some(node) => statements.+:()
    }
    false
  }
  
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
    val predicateVisitor = new NotImplementedVisitor
    node.getExpression.accept(predicateVisitor)
    predicateVisitor.result() match {
      case None => Option.empty
      case Some(p) => {
        new Stm().acceptResult(node.getThenStatement)  match {
          case None => Option.empty
          case Some(t) => if (node.getElseStatement == null) {
            Some(new If(p, t, new Skip))
          } else {
            new Stm().acceptResult(node.getElseStatement) match {
              case None    => Some(new If(p, t, new Skip))
              case Some(e) => Some(new If(p, t, e))
            }
          }
        }
      }
    }
  }
}