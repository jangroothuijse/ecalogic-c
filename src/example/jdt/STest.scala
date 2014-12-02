package example.jdt;

import java.util.HashSet;
import java.util.Set;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.WhileStatement;
import org.eclipse.jdt.core.dom.ForStatement;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;
import org.eclipse.jdt.core.dom.VariableDeclarationExpression;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.MemberValuePair
import scala.io.Source

import nl.ru.cs.ecalogic.ast._


object STest {
  def main(args: Array[String]) {
    val parser: ASTParser = ASTParser.newParser(AST.JLS3);
    parser.setSource(Source.fromFile("src/main/scala/nl/ru/cs/ecalogic/model/examples/RadioProgram.java").mkString.toArray)
    //parser.setSource("/*abc*/".toCharArray());
    parser.setKind(ASTParser.K_COMPILATION_UNIT);
    //ASTNode node = parser.createAST(null);
 
 
    val cu: CompilationUnit = parser.createAST(null).asInstanceOf[CompilationUnit];
    
    // give visitor a name, such that it can be recursive, because it needs to be.
    val p = new Program(Map[String, Import](), Map[String, FunDef](), Map[String, Definition]());
    cu.accept(new ASTVisitor() {
 
      var names: Set[String] = new HashSet();
      
      p.imports.++:("Hoi", Import([""]));
      
      override def visit(node: WhileStatement) : Boolean = {
        System.out.println("Usage of while detected; we should throw an error")
        
        // throw new ECAException()
        return false;
      }
      
      override def visit(node: ForStatement) : Boolean = {
        
        
        System.out.println("Usage of for detected, investigating")
        node.accept(new ASTVisitor() {
          override def visit(node: VariableDeclarationExpression): Boolean = {
            System.out.println("VraibleDeclerationExpression")
            node.accept(new ASTVisitor() {
              override def visit(node: VariableDeclarationFragment): Boolean = {
                System.out.println("Declaration of " + node.getName)
                return true;
              }
              
              override def visit(node: NormalAnnotation): Boolean = {
                System.out.println ("Declaration of " + node.getTypeName)
                //if (node.getTypeName == "ECAForBound") {
                for (pair <- node.values().toArray) {
                  System.out.println(
                    "\t" + pair.asInstanceOf[MemberValuePair].getName
                    + ": " + pair.asInstanceOf[MemberValuePair].getValue)
                }
                //
                return true;
              }
            })
            
            return false;
          }
        })
        // throw new ECAException()
        return false;
      }
 
      override def visit(node: VariableDeclarationFragment): Boolean = {
        val name: SimpleName = node.getName();
        this.names.add(name.getIdentifier());
        System.out.println("Declaration of variable named: '"+name+"' at line "+cu.getLineNumber(name.getStartPosition()));
        return true; // do not continue to avoid usage info
      }
 
      override def visit(node: SimpleName): Boolean = {
        if (this.names.contains(node.getIdentifier())) {
        System.out.println("Usage of '" + node + "' at line " + cu.getLineNumber(node.getStartPosition()));
        }
        return true;
      }
 
    });
  }
}