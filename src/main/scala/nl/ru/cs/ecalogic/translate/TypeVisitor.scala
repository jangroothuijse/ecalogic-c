package nl.ru.cs.ecalogic
package translate
import org.eclipse.jdt.core.dom.TypeDeclaration
import nl.ru.cs.ecalogic.ast
import nl.ru.cs.ecalogic.translate.Stm
import org.eclipse.jdt.core.dom.Type
import org.eclipse.jdt.core.dom.PrimitiveType
import ast.ASTIntegerT
import ast.ASTRealT
import ast.ASTVoidT
import ast.ASTUnionT

/**
 * Transform Java types to Ecalogic types
 * 
 * eca                      java
 * 
 * :: ASTType =
 * |  ASTIntegerT           // int long char byte short
 * |  ASTRealT              // float double
 * |  ASTVoidT              // Void
 * |  ASTArrayT ASTType     // build-in arrays of java, so the [] as in int[] float[] byte[] SomeType[]
 * |  ASTPointerT ASTType   // Does not exist in java
 * |  ASTStructT String     // Object
 * |  ASTUnionT String      // Does not exist in java
 * 
 * We should ignore generics 
 * 
 */
class TypeVisitor(node: Type) extends NotImplementedVisitor[ast.ASTType]{
 
  override def result(): Option[ast.ASTType] = {
    val whatType = node
    if (whatType.isInstanceOf[PrimitiveType]) {
      val typePrimitive = node.asInstanceOf[PrimitiveType]
      typePrimitive.getPrimitiveTypeCode() match {
        case int => Some(new ASTIntegerT())
        case long => Some(new ASTIntegerT())
        case char => Some(new ASTIntegerT())
        case byte => Some(new ASTIntegerT())
        case short => Some(new ASTIntegerT())
        case float => Some(new ASTRealT())
        case double => Some(new ASTRealT())
        case void => Some(new ASTVoidT())
        case boolean => Some(new ASTUnionT("")) // boolean instead of string 
      }
      None
      } else None
    
//    else if (whatType.isArrayType() == true) {
//        val typeArray = new ArrayType
//        node.getComponentType().accept(typeArray)
//        typeArray.result() match {
//          
//        }          
          
   
  }  
}