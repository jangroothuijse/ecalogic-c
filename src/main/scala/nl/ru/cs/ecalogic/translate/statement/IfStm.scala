package nl.ru.cs.ecalogic
package translate.statement

import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.IfStatement
import org.eclipse.jdt.core.dom.WhileStatement
import org.eclipse.jdt.core.dom.Statement
import nl.ru.cs.ecalogic.translate.Visitor
import nl.ru.cs.ecalogic.translate.NotImplementedVisitor;
import ast.If
import ast.Skip
import ast.Expression

class Stm(node: Statement) extends ASTVisitor with Visitor[ast.Composition] {
  def result(): Option[ast.Composition] = {
    Option.empty
  }
}

class IfStm(node: IfStatement) extends ASTVisitor with Visitor[If] {

  def result(): Option[If] = {
    val predicateVisitor = new NotImplementedVisitor
    node.getExpression.accept(predicateVisitor)
    predicateVisitor.result() match {
      case None => Option.empty
      case Some(p) => {
        val then = new Stm(node.getThenStatement)
        then.result() match {
          case None => Option.empty
          case Some(t) => if (node.getElseStatement == null) {
            Some(new If(p, t, new Skip))
          } else {
            val elseVisitor = new Stm(node.getElseStatement);
            elseVisitor.result() match {
              case None    => Some(new If(p, t, new Skip))
              case Some(e) => Some(new If(p, t, e))
            }
          }
        }
      }
    }
  }
}