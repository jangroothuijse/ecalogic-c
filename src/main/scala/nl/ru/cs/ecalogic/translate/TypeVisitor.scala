package nl.ru.cs.ecalogic
package translate

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
class TypeVisitor extends NotImplementedVisitor[ast.ASTType]{
  
}