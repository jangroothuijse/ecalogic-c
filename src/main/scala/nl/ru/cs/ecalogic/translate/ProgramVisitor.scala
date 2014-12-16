package nl.ru.cs.ecalogic
package translate

// http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2FAST.html
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.ImportDeclaration
import org.eclipse.jdt.core.dom.TypeDeclaration
import org.eclipse.jdt.core.dom.DoStatement
import org.eclipse.jdt.core.dom.ForStatement
import org.eclipse.jdt.core.dom.IExtendedModifier
import org.eclipse.jdt.core.dom.Assignment
import org.eclipse.jdt.core.dom.VariableDeclarationFragment
import nl.ru.cs.ecalogic.translate.TranslateVisitor
import nl.ru.cs.ecalogic.translate.NotImplementedVisitor
import ast.Program
import ast.Import
import ast.FunDef
import ast.Definition
import ast.StructDef
import ast.ASTType
import org.eclipse.jdt.core.dom.FieldDeclaration

class ProgramVisitor extends TranslateVisitor[Program] {
  // imports
  val imports: Map[String, Import] = Map();
  // methods of objects and classes (so both static and non static)
  val functions: Map[String, FunDef] = Map();
  // structs to store object variables
  val defs: Map[String, Definition] = Map();
  
  override def visit(i: ImportDeclaration) : Boolean = {
    val name: String = i.getName.getFullyQualifiedName;
    val ecaImport = new Import(name.split("."), name)
    imports.+((name, ecaImport));
    false
  }
  
  // [Classes] -> ([Functions], [Structs])
  override def visit(t: TypeDeclaration) : Boolean = {
    val name: String = t.getName.getIdentifier;
    // per type, make 1 struct (if it has any fields)
    val fields: Map[String, ASTType] = Map()
    
    for (field <- t.getFields) {
      // TODO: check if its static
      field.accept(new ASTVisitor() {
        override def visit(vdf: VariableDeclarationFragment) : Boolean = {
          vdf.getName.getIdentifier
          // we dont have ASTType ... :(
          // otherwise we now have a name and a type, so we can add it to the fields
          false
        }
      })
    }
    defs.+((name, new StructDef(name, fields)))
    
    // parse functions, prepend their name with our name
    false
  }
  
  def result() : Option[Program] = Some(new Program(imports, functions, defs));
}