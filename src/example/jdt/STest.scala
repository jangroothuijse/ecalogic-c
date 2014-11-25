package example.jdt;

import java.util.HashSet;
import java.util.Set;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.ASTVisitor;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.core.dom.VariableDeclarationFragment;

object STest {
  def main(args: Array[String]) {
    val parser: ASTParser = ASTParser.newParser(AST.JLS3);
    parser.setSource("public class A { int i = 9;  \n int j; \n ArrayList<Integer> al = new ArrayList<Integer>();j=1000; }".toCharArray());
    //parser.setSource("/*abc*/".toCharArray());
    parser.setKind(ASTParser.K_COMPILATION_UNIT);
    //ASTNode node = parser.createAST(null);
 
 
    val cu: CompilationUnit = parser.createAST(null).asInstanceOf[CompilationUnit];
 
    cu.accept(new ASTVisitor() {
 
      var names: Set[String] = new HashSet();
 
      override def visit(node: VariableDeclarationFragment): Boolean = {
        val name: SimpleName = node.getName();
        this.names.add(name.getIdentifier());
        System.out.println("Declaration of variable named: '"+name+"' at line "+cu.getLineNumber(name.getStartPosition()));
        return false; // do not continue to avoid usage info
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