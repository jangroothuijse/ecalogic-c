package nl.ru.cs.ecalogic
package translate

import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.MemberValuePair

/**
 * Trait to define utility function we want to gather the results of visiting
 */
trait Visitor[A] {
  /**
   * To be called after the object has a chance to visit the nodes.
   */
  def result() : Option[A]
  
}

/**
 * Default mock/stub implementation.
 */
class NotImplementedVisitor[A]() extends ASTVisitor with Visitor[A] {
  def result() : Option[A] = Option.empty;
}

abstract class TranslateVisitor[A] extends ASTVisitor with Visitor[A] {
  def acceptResult(node : ASTNode) : Option[A] = {
    node.accept(this);
    this.result();
  }
}