package main.scala.nl.ru.cs.ecalogic.model.examples;


/**
 * Example Radio hardware component, in Java
 */
public final class JavaRadio {
	public static int active = 0;
	
	@Consumes(energy = 200, time = 20)
	public static void on() {
		active = 1;
	}
	
	@Consumes(energy = 10, time = 10)
	public static void off() {
		active = 0;
	}
	
	public static int phi() {
		return 20 + active * 200;
	}
}