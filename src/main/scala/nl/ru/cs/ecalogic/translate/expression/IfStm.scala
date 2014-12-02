package nl.ru.cs.ecalogic
package translate.expression

import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.WhileStatement;
import translate.Visitor
import ast.If
import ast.Skip
import ast.Expression
import ast.Statement

class IfStm extends ASTVisitor with Visitor[If] {
  var predicate : Option[Expression] = None
  var consequent: Option[Statement] = None
  var alt: Option[Statement] = None
  
  // visit functions
  override def visit(node: WhileStatement) : Boolean = {
    return false;
  }
  
  
  def result() : Option[If] = 
    predicate match {
      case None => Option.empty
      case Some(p) => consequent match {        
        case None => Option.empty
        case Some(c) => alt match {
          case None => Option.apply(new If(p, c, new Skip))
          case Some(a) => Option.apply(new If(p, c, a))
        } 
      }
    } 
  
}