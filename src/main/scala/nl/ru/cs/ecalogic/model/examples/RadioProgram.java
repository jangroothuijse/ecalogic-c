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
	public int foo = 1;
	static int bar = 1;
	
	class Foo {
		void bla() {
			this.getClass();
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
