/**
 * 
 */
package main.scala.nl.ru.cs.ecalogic.model.examples;
import main.scala.nl.ru.cs.ecalogic.model.ECAForBound;
import main.scala.nl.ru.cs.ecalogic.model.examples.JavaRadio;

/**
 * Example program, adapted from radioexample, but left out any part of the energy models that 
 * where not found.
 */
public class RadioProgram {
	/*
	 * RadioProgram object variables are put as fields into the RadioProgram struct
	 * 
	 * RadioProgram methods are functions:
	 * the static methods translate: RadioProgram.methodName => RadioProgram_methodName
	 * member methods are translate: RadioProgram.methodName => RadioProgram_methodName(this: RadioProgram)
	 * where RadioProgram is a struct in eca
	 * 
	 * Now when used in expressions, static methods are easy to translate
	 * member methods require the type of the variable, which is stored in
	 * MethodInvocation/DECLARING_CLASS
	 */
		
	
	public int foo = 1; // translates to a field foo: Int of struct RadioProgram
	static int bar = 1; // illegal in eca, no global variables allowed
	
	class Foo { 
		void bla() {
			int foo = 1;
			this.getClass();
			int bar = foo++ == 2 ? 1 : 2;
			bar = (foo += 2) * (foo *= 4) + bar;
		}
	}
	// 
	// RadioProgram_Foo_bla(Foo this)
	// bla1 = new Foo()
	// bla1.bla() => Foo_bla(bla1)
	// 
	
	static void algorithm1(int x, int xdiv10) {		
		JavaRadio.on();
		for (@ECAForBound(lower = "x", upper = "x") int i = 0; i < x; i++) {
			
		}
	}
	
	static void start(int x) {
		algorithm1(x, x * 10);
	}
	
	public static void main(String[] args) {
		start(10);
	}
}
