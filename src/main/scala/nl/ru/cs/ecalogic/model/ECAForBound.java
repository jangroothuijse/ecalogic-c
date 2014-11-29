/**
 * 
 */
package main.scala.nl.ru.cs.ecalogic.model;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotates the upper and lower bound of a for construct, should be placed right before the 
 * local variable declaration inside the for parenthesis.
 * 
 * for (ECAForBound(upper = '0', lower = '0' int i = 0; i < 0; i++)
 * - note omitted at sign before ECAForBound because of javadoc
 */
@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.LOCAL_VARIABLE)
public @interface ECAForBound {
	public String upper();
	public String lower();
}
