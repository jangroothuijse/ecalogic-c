package nl.ru.cs.ecalogic
package translate

/**
 * Visits expressions, can cross over to Statements again using expression statement
 * an assignment in an expression would be such and expression statement.
 * 
 * Since ASTNode does not have such expression, we will have to model it as an extra statement preceding
 * the statement our current expression in on. So now every expression can be an expression statement and
 * emit an expression and thus every expression can can have multiple subexpressions that are expression
 * statements, meaning each expression has a list of these emitted statements.
 */
class ExpressionVisitor extends NotImplementedVisitor[(List[ast.Statement], ast.Expression)] {
  
}