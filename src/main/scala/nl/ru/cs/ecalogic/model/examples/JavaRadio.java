package nl.ru.cs.ecalogic.model.examples;

/**
 * Example Radio hardware component, in Java
 */
public class JavaRadio {
	public int active;

	public JavaRadio() {
		active = 0;
	}
	
	@Consumes(energy = 200, time = 20)
	public void on() {
		active = 1;
	}
	
	@Consumes(energy = 10, time = 10)
	public void off() {
		active = 0;
	}
	
	public int phi() {
		return 20 + active * 200;
	}
}