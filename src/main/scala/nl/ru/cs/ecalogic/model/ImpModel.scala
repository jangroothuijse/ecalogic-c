/*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package nl.ru.cs.ecalogic
package model

import util._
import ast._

import scala.collection.mutable.Builder

import java.lang.Class
import java.lang.reflect.Constructor
import java.lang.reflect.Method
import java.lang.reflect.Field
import java.lang.annotation._
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import hcminterface._

/**
 * ComponentModel class representing HCMs in direct implementation format
 * @author Stein Keijzers
 */
class ImpModel(classname: String, protected val errorHandler: ErrorHandler = new DefaultErrorHandler()) extends ComponentModel {
	import ECAException._
	
	class CState private[ImpModel](val elements: Map[String, ECAValue]) extends ComponentState {
		protected def update(newElements: Map[String, ECAValue]) = new CState(newElements)
	}
	
	//Checks if the given annotation class has energy() and time() methods
	def checkAnnotations (annotclass : Class[_]) : Boolean = {
		var energy = false;
		var time = false;
		for (method <- annotclass.getDeclaredMethods()) {
			if (method.getName().equals("energy")) energy = true;
			if (method.getName().equals("time")) time = true;
		}
		energy && time
	}
	
	//Create a classloader for the components folder
	val classloader = new URLClassLoader(Array(new URL("file://" + System.getProperty("user.dir") + "/components/javacomponents/")));
	
	//Get the class for the component
	val componentclass = classloader.loadClass(classname);
	
	//Instantiate the component class
	val comp = componentclass.newInstance();
	
	//Retrieve the fields
	val fields = componentclass.getDeclaredFields();
		
	//Retrieve the methods annotated with energy/time
	val methods = {
		val allmethods = componentclass.getMethods();
		val methodsbuilder = Seq.newBuilder[(Method, Annotation)];
		for (method <- allmethods) {
			if (method.getDeclaringClass().getSimpleName().equals(classname)) {
				val annots = method.getDeclaredAnnotations();
				var found = false;
				for (annot <- annots) {
					if (checkAnnotations(annot.getClass()) && !found) {
						methodsbuilder += ((method, annot));
						found = true;
					}
				}
			}
		}
		methodsbuilder.result()
	}
	
	//Get the phi method
	val phimethod = {
		val allmethods = componentclass.getMethods();
		var res: Option[Method] = None;
		for (method <- allmethods) {
			if (method.getName().equals("phi")) res = Some(method);
		}
		res match {
			case None => errorHandler.fatalError(new ECAException(s"Component '${componentclass.getSimpleName()}' does not contain a phi function!"));
			case Some(m) => m
		}
	}
	
	//Initial state is the fields from the class
	val initialState: CState = makeState();
	
	//Name of the component is the classname
	val name: String = classname;
	
	//Constructs a CState from the current values of the component
	def makeState(): CState = {
		val elements = Map.newBuilder[String, ECAValue];
		for (field <- fields) {
			val getresult = field.get(comp);
			elements += (field.getName() -> anyToECAV(getresult));
		}
		new CState(elements.result())
	}
	
	//Adds aliases to the given seqbuilder
	def processAliases(map : Builder[(String, (String, String)), Seq[(String, (String, String))]]) = {
		for ((method, annot) <- methods) {
			var methodoption : Option[Method] = None;
			for (annotmethod <- annot.getClass().getDeclaredMethods()) {
				if (annotmethod.getName().equals("alias")) methodoption = Some(annotmethod);
			}
			methodoption match {
				case None => Unit
				case Some(m) =>
					val realname = m.invoke(annot).asInstanceOf[String];
					map += ((realname, (name, method.getName)));
					Unit
			}
		}
	}
	
	//Retrieves given operations for a method
	def getOps(f: String) : Seq[(String, HCMOperation)] = {
		val oldstate = makeState();
		var methodoption: Option[Method] = None;
		for ((method, annot) <- methods) {
			if (method.getName().equals(f)) {
				methodoption = Some(method);
			}
		}
		methodoption match {
			case None => errorHandler.fatalError(new ECAException(s"Component function '${f}' for component '${name}' not found!"));
			case Some(method) => {
				val retval = method.invoke(comp);
				applyState(oldstate);
				val retname = retval.getClass().getSimpleName();
				if (retname.equals("StateChanges")) {
					val res = Seq.newBuilder[(String, HCMOperation)];
					val changes = retval.asInstanceOf[StateChanges];
					var i = 0;
					for (i <- 0 until changes.size()) {
						res += ((changes.getVar(i), changes.getOp(i)));
					}
					res.result
				} else {
					println("Warning: function " + f + " in component " + name + " does not change the state!");
					Seq.empty[(String, HCMOperation)]
				}
			}
		}
	}
	
	//Applies given operations to this component state
	//The format is in a long sequence of <statevar, ops>
	def applyOps(ops: Seq[(String, HCMOperation)]) = {
		for (tup <- ops) {
			var fieldoption : Option[Field] = None;
			for (f <- fields) {
				if (f.getName().equals(tup._1)) {
					fieldoption = Some(f);
				}
			}
			fieldoption match {
				case None => errorHandler.fatalError(new ECAException(s"Component '${name}' does not have state variable '${tup._1}'!"));
				case Some(f) => ECAVToField(evalOp(tup._2), f);
			}
		}
	}
	
	//Evaluates an HCMOperation
	def evalOp(op: HCMOperation) : Double = {
		op match {
			case con : ConstantValue => con.doubleValue
			case ref : StateReference => {
				var fieldoption : Option[Field] = None;
				for (f <- fields) {
					if (f.getName().equals(ref.referenceName)) {
						fieldoption = Some(f);
					}
				}
				fieldoption match {
					case None => errorHandler.fatalError(new ECAException(s"Component '${name}' does not have state variable '${ref.referenceName}'!"));
					case Some(f) => {
						val getres = f.get(comp);
						val getecav = anyToECAV(getres);
						val getd = getecav.underlying;
						getd
					}
				}
			}
			case add : AddOperation => evalOp(add.leftOperand) + evalOp(add.rightOperand)
			case sub : SubOperation => evalOp(sub.leftOperand) - evalOp(sub.leftOperand)
		}
	}
	
	//Definition of E(f): energy consumption for function f
	override def E(f: String) = {
		var annotoption: Option[Annotation] = None;
		for ((method, annot) <- methods) {
			if (method.getName().equals(f)) {
				annotoption = Some(annot);
			}
		}
		var result: Option[ECAValue] = None;
		annotoption match {
			case None => errorHandler.fatalError(new ECAException(s"Component function '${f}' for component '${name}' not found!"));
			case Some(annot) => {
				var found = false;
				for (method <- annot.getClass().getDeclaredMethods()) {
					if (!found && method.getName().equals("energy")) {
						result = Some(callToECAV(method, annot));
						found = true;
					}
				}
				if (!found) errorHandler.fatalError(new ECAException(s"Annotation for function '${f}' for component '${name}' does not contain an energy field!"));
			}
		}
		result match {
			case None => errorHandler.fatalError(new ECAException(s"Retrieving energy for function '${f}' for component '${name}' failed!"));
			case Some(ecav) => ecav
		}
	}
	
	//Definition of T(f): time consumption for function f
	override def T(f: String) = {
		var annotoption: Option[Annotation] = None;
		for ((method, annot) <- methods) {
			if (method.getName().equals(f)) {
				annotoption = Some(annot);
			}
		}
		var result: Option[ECAValue] = None;
		annotoption match {
			case None => errorHandler.fatalError(new ECAException(s"Component function '${f}' for component '${name}' not found!"));
			case Some(annot) => {
				var found = false;
				for (method <- annot.getClass().getDeclaredMethods()) {
					if (!found && method.getName().equals("time")) {
						result = Some(callToECAV(method, annot));
						found = true;
					}
				}
				if (!found) errorHandler.fatalError(new ECAException(s"Annotation for function '${f}' for component '${name}' does not contain a time field!"));
			}
		}
		result match {
			case None => errorHandler.fatalError(new ECAException(s"Retrieving time for function '${f}' for component '${name}' failed!"));
			case Some(ecav) => ecav
		}
	}
	
	//Apply a given state to the internal component instance
	def applyState(s: CState) = {
		for (field <- fields) {
			val newvalueoption = s.elements.get(field.getName());
			newvalueoption match {
				case None => println(s"Warning: state variable '${field.getName()}' not found in the state, skipping");
				case Some(ecav) => ECAVToField(ecav, field);
			}
		}
	}
	
	//Definition of delta(f)(s): invoke the function on the component with the proper state
	override def delta(f: String)(s: CState) = {
		applyState(s);
		var methodoption: Option[Method] = None;
		for ((method, _) <- methods) {
			if (method.getName().equals(f)) {
				methodoption = Some(method);
			}
		}
		methodoption match {
			case None => errorHandler.fatalError(new ECAException(s"Component function '${f}' for component '${name}' not found!"));
			case Some(method) => method.invoke(comp);
		}
		makeState()
	}
	
	//Definition of phi(s): invoke the phi method on the given state
	override def phi(s: CState) = {
		applyState(s);
		callToECAV(phimethod, comp)
	}
	
	//Converts something to an ECAValue, if possible
	def anyToECAV(a: Any) : ECAValue = {
		val name = a.getClass().getSimpleName();
		name match {
			case "int" | "Integer" => a.asInstanceOf[Int].toDouble;
			case "float" | "Float" => a.asInstanceOf[Float].toDouble;
			case "double" | "Double" => a.asInstanceOf[Double];
			case _ => println(s"Error: Could not convert Any to ECAV: class '${name}' not recognized."); new ECAValue(0.0);
		}
	}
	
	//Converts an object to an array, provided it really is one
	def anyToArray(a: Any) : Array[Any] = {
		val ofArray = a.getClass().getComponentType();
		if (ofArray.isPrimitive()) {
			var ar : Array[Any] = Array.empty[Any];
			val length = java.lang.reflect.Array.getLength(a);
			var i = 0;
			for (i <- 0 until length) {
				ar = ar :+ java.lang.reflect.Array.get(a, i);
			}
			ar;
		}
		else {
			a.asInstanceOf[Array[Any]];
		}
	}
	
	//Converts a method call into an ECAValue
	def callToECAV(m: Method, o: Any) : ECAValue = {
		val returnvalue = m.invoke(o);
		anyToECAV(returnvalue);
	}
	
	//Converts an ECAValue into a desired type
	def ECAVToAny(v: ECAValue, c: Class[_]) : Any = {
		val name = c.getSimpleName();
		name match {
			case "int" | "Integer" => v.underlying.toInt.intValue;
			case "float" | "Float" => v.underlying.toFloat.floatValue;
			case "double" | "Double" => v.underlying.doubleValue;
			case _ => println(s"Error: Could not set field for class: '${name}', not recognized.");
		}
	}
	
	//Converts an ECAValue into a type suited for the specified field, and sets it on the model
	def ECAVToField(v: ECAValue, f: Field) = {
		val fieldclass = f.getType();
		f.set(comp, ECAVToAny(v, fieldclass));
	}
}

object ImpModel {
	def main(args: Array[String]) {
		val model = new ImpModel("JavaRadio");
		val newstate = model.delta("on")(model.initialState);
		for (varvalpair <- newstate.elements) {
			println(s"Variable '${varvalpair._1}' with value '${varvalpair._2}'");
		}
		println(s"Current passive energy consumption: '${model.phi(newstate)}'");
	}
}