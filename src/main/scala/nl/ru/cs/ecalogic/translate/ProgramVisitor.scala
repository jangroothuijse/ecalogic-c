package nl.ru.cs.ecalogic
package translate

// http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fjdt%2Fcore%2Fdom%2FAST.html
import org.eclipse.jdt.core.dom.ASTVisitor
import org.eclipse.jdt.core.dom.ImportDeclaration
import org.eclipse.jdt.core.dom.TypeDeclaration
import org.eclipse.jdt.core.dom.DoStatement
import org.eclipse.jdt.core.dom.ForStatement
import org.eclipse.jdt.core.dom.Modifier
import org.eclipse.jdt.core.dom.MethodDeclaration
import org.eclipse.jdt.core.dom.IExtendedModifier
import org.eclipse.jdt.core.dom.SingleVariableDeclaration
import org.eclipse.jdt.core.dom.VariableDeclarationFragment
import ast.Program
import ast.Import
import ast.FunDef
import ast.Definition
import ast.StructDef
import ast.ASTType
import org.eclipse.jdt.core.dom.FieldDeclaration

class ProgramVisitor extends TranslateVisitor[Program] {
  // imports
  var imports: Map[String, Import] = Map();
  // methods of objects and classes (so both static and non static)
  var functions: Map[String, FunDef] = Map();
  // structs to store object variables
  var defs: Map[String, Definition] = Map();
  
  override def visit(i: ImportDeclaration) : Boolean = {
    val name: String = i.getName.getFullyQualifiedName;
    val ecaImport = new Import(name.split("."), name)
    imports = imports.+((name, ecaImport));
    false
  }
  
  // [Classes] -> ([Functions], [Structs])
  override def visit(t: TypeDeclaration) : Boolean = {
    val name: String = t.getName.getIdentifier;
    // per type, make 1 struct (if it has any fields)
    var fields: Map[String, ASTType] = Map()
    
    for (field <- t.getFields) {
      // TODO: check if its static
      field.accept(new ASTVisitor() {
        override def visit(vdf: VariableDeclarationFragment) : Boolean = {          
          new TypeVisitor(field.getType).result() match {
            case None =>
            case Some(astType) =>
              fields = fields.+((vdf.getName.getIdentifier, astType))
          }
          false
        }
      })
    }
    defs = defs.+((name, new StructDef(name, fields)))
    
    for (fun <- t.getMethods) {
      new Stm().acceptResult(fun.getBody()) match {
          case None =>
          case Some(body) =>
            if ((fun.getModifiers & (Modifier.STATIC | Modifier.FINAL)) == 0
                && (t.getModifiers & Modifier.FINAL) == 0)
               throw new ECAException("Methods must be at final, static or both or the class must be final.")
            else {
              functions = functions.+(
                (fun.getName.getFullyQualifiedName,  
                new FunDef(
                  fun.getName.getFullyQualifiedName, 
                  // in case of static functions, we do not need an argument for the this object
                  (if ((fun.getModifiers & Modifier.STATIC) == 0) ProgramVisitor.methodArgs else Array()) ++ 
                  (for (f <- fun.parameters().toArray()) 
                    yield f.asInstanceOf[SingleVariableDeclaration].getName.getIdentifier),
                  body
                ))
              )
            }
      }
    }
    false
  }
  
  def result() : Option[Program] = Some(new Program(imports, functions, defs)); 
}

object ProgramVisitor {
  val methodArgs : Array[String] = Array(ExpressionVisitor.thisName)
}