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
package analysis

import ast._
import parser.Parser
import util.{ErrorHandler, DefaultErrorHandler, Polynomial}
import model._
import config.Options.{Analysis => Config}

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.Builder

import java.io.File

import hcminterface._


/**
 * @author Marc Schoolderman
 */
class EnergyAnalysis(program: Program, components: Map[String, ComponentModel], eh: ErrorHandler = new DefaultErrorHandler()) {

  import GlobalState.States
  import Transform._
  import EnergyAnalysis._


  type Environment = Map[Expression, Expression]
	
	//Class representing a function signature: energy & time changes caused by a function stored as polynomials
	class Signature(val energyConsumption: Map[String, (Polynomial, Polynomial)], val timeConsumption: Map[String, (Polynomial, Polynomial)], val stateOps: Map[String, (Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]], Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]])], val globalTimeStamp: (Polynomial, Polynomial)) {	
		override def toString = {
			var res : String = "Lower bound:\n";
			
			for (tup <- energyConsumption) {
				res = res + tup._1 + ": " + tup._2._1.toString + " energy, " + timeConsumption.get(tup._1).get._1.toString + " time\n";
			}
			
			res = res + "Global timestamp: " + globalTimeStamp._1.toString;
			
			res = res + "\nUpper bound:\n";
			
			for (tup <- energyConsumption) {
				res = res + tup._1 + ": " + tup._2._2.toString + " energy, " + timeConsumption.get(tup._1).get._2.toString + " time\n";
			}
			
			res = res + "Global timestamp: " + globalTimeStamp._1.toString;
			
			res
		}
	}
  
	//Other environment-like things that are kept track of for the C analysis
	class CEnv() {
		//Lengths of arrays as they are defined
		var arrayLengths = Map.empty[Expression, Expression];
		
		//Function signatures
		var functionSignatures = Map.empty[String, Signature];
		
		//Variable collection tracts used to keep track of diverging paths in individual statement HCM operational branches
		//Could really do with a custom type instead of this monster
		var collectionTracts = Map.empty[String, Map[String, Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]]]];
		
		//Current tract; "rootl" and "rootu" are the default for lower and upper bounds, and will be eventually used as the final tract
		var currentTract = "rootu";
		
		//Add an operation to the current tract
		def addOperation(comp: String, statevar: String, count: Polynomial, op: HCMOperation) = {
			//Get the current tract
			val resoption = collectionTracts.get(currentTract);
			resoption match {
				case None => println("Error: Couldn't find HCM state operation tract " + currentTract + "!");
				case Some(tract) => {
					//Get the component
					val compoption = tract.get(comp);
					compoption match {
						case None => println("Error: Couldn't add to tract for missing component " + comp + "!");
						case Some(builder) => {
							//Add it
							builder += ((statevar, count, op));
						}
					}
				}
			}
		}
		
		//Create a tract; the globalstate is only used to get component names
		def createTract(name: String, comps : GlobalState) {
			val builder = Map.newBuilder[String, Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]]];
			//Go through each component
			for (tup <- comps.gamma) {
				val compName = tup._1;
				
				//Create a new seqbuilder for this one and add it
				builder += (compName -> Seq.newBuilder[(String, Polynomial, HCMOperation)]);
			}
			
			//Add the actual tract
			collectionTracts += (name -> builder.result);
		}
		
		//Set the current tract
		def setTract(name: String) {
			currentTract = name;
		}
		
		//WHERE WE AT:
		/*
		- ADD BACKUP/RESTORE
		- ADD RETRIEVE/ADDTRACT
		- ACTUALLY ADD THIS TO THE ANALYSIS STEPS
		*/
		
		//Adds all elements of the specified tract to root
		def solidifyTract(name: String, upper: Boolean) {
			//Get the appropiate tracts
			val rootoption = if (upper) {collectionTracts.get("rootu")} else {collectionTracts.get("rootl")};
			rootoption match {
				case None => println("Error: Couldn't get root tract!");
				case Some(root) => {
					val suboption = collectionTracts.get(name);
					suboption match {
						case None => println("Error: Couldn't solidify tract " + name + "!");
						case Some(sub) => {
							//For each element in sub
							for (subtup <- sub) {
								val compName = subtup._1;
								val ops = subtup._2;
								
								//Get the appropiate builder from root
								val rootboption = root.get(compName);
								rootboption match {
									case None => println("Error: Component mismatch between tracts!");
									case Some(builder) => {
										for (trip <- ops.result) builder += trip;
									}
								}
							}
						}
					}
				}
			}
		}
		
		//Clear all tracts except root, sets the current tract to rootu
		def clearTracts() {
			collectionTracts = collectionTracts.filter(tup => tup._1.equals("rootu") || tup._1.equals("rootl"));
			setTract("rootu");
		}
		
		//Applies the root tracts to the given function and clears them
		def applyRoot(fun: String) {
			//Get everything! Now this is nesting
			val rootutractoption = collectionTracts.get("rootu");
			rootutractoption match {
				case None => println("Error: Couldn't get root upper-bound tract!");
				case Some(rootutract) => {
					val rootltractoption = collectionTracts.get("rootl");
					rootltractoption match {
						case None => println("Error: Couldn't get root lower-bound tract!");
						case Some(rootltract) => {
							val signatureoption = functionSignatures.get(fun);
							signatureoption match {
								case None => println("Error: No signature for function " + fun + " found!");
								case Some(signature) => {
									//We have all the tracts, now loop though the components
									for (signaturetup <- signature.stateOps) {
										val compname = signaturetup._1;
										val signaturelowerbuilder = signaturetup._2._1;
										val signatureupperbuilder = signaturetup._2._2;
										
										//Get the appropiate sequences
										val lowertractoption = rootltract.get(compname);
										lowertractoption match {
											case None => println("Error: Component mismatch in tract merging!");
											case Some(lowertract) => {
												//Loop and add these
												for (trip <- lowertract.result) signaturelowerbuilder += trip;
												
												//Clear it
												lowertract.clear();
											}
										}
										val uppertractoption = rootutract.get(compname);
										uppertractoption match {
											case None => println("Error: Component mismatch in tract merging!");
											case Some(uppertract) => {
												//Loop and add these
												for (trip <- uppertract.result) signatureupperbuilder += trip;
												
												//Clear it
												uppertract.clear();
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		
		def addArrayLength(name: Expression, length: Expression) = {
			arrayLengths = arrayLengths + (name -> length);
		}
		
		def addFunctionSignature(name: String, sig: Signature) = {
			functionSignatures = functionSignatures + (name -> sig);
		}
		
		def hasSignature(name: String) : Boolean = functionSignatures.contains(name);
		
		
		def printSignatures() = {
			println("\n**********************************");
			println("Function signatures (unfinished!):\n");
			for (tup <- functionSignatures) {
				println(tup._1 + ":");
				println(tup._2.toString + "\n");
			}
			println("**********************************\n");
		}
		
		def applyArrayLengths(exp: Expression) : Expression = {
			exp match {
				case bin: BinaryExpression => bin.rewrite(Seq(applyArrayLengths(bin.left), applyArrayLengths(bin.right)))
				case arrayL: ArrayLength => arrayLengths.get(arrayL.operand) match { case Some(exp) => exp case None => VarRef("Undefined Array Length") }
				case un: UnaryExpression => un.rewrite(Seq(applyArrayLengths(un.operand)))
				case e => e
			}
		}
		
		def clear() = {
			arrayLengths = Map.empty[Expression, Expression];
		}
		
		def copy() : CEnv = {
			val nenv = new CEnv();
			for (tup <- arrayLengths) nenv.addArrayLength(tup._1, tup._2);
			for (tup <- functionSignatures) nenv.addFunctionSignature(tup._1, tup._2);
			nenv
		}
	}
	var cenv = new CEnv;
	
	//Extracts the difference between two tuples of two GlobalStates each as a signature, and creates an empty operation thing
	def extractSignature(in: (GlobalState, GlobalState), out: (GlobalState, GlobalState)) : Signature = {
		//Keep track of the results
		var energyResult = Map.empty[String, (Polynomial, Polynomial)];
		var timeResult = Map.empty[String, (Polynomial, Polynomial)];
		var stateMap = Map.empty[String, (Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]], Builder[(String, Polynomial, HCMOperation), Seq[(String, Polynomial, HCMOperation)]])];
		
		//Go through each component
		for (tup <- in._1.gamma) {
			val compName = tup._1;
			
			//Get the lower energy change
			val lowerEnergy = {
				val before = in._1.gamma.get(compName).get.energy;
				val after = out._1.gamma.get(compName).get.energy;
				after - before
			}
			
			//Get the upper energy change
			val upperEnergy = {
				val before = in._2.gamma.get(compName).get.energy;
				val after = out._2.gamma.get(compName).get.energy;
				after - before
			}
			
			//Get the lower time change
			val lowerTime = {
				val before = in._1.gamma.get(compName).get.timestamp;
				val after = out._1.gamma.get(compName).get.timestamp;
				after - before
			}
			
			//Get the upper time change
			val upperTime = {
				val before = in._2.gamma.get(compName).get.timestamp;
				val after = out._2.gamma.get(compName).get.timestamp;
				after - before
			}
			
			//Add these results
			energyResult += (compName -> (lowerEnergy, upperEnergy));
			timeResult += (compName -> (lowerTime, upperTime));
			stateMap += (compName -> (Seq.newBuilder[(String, Polynomial, HCMOperation)], Seq.newBuilder[(String, Polynomial, HCMOperation)]));
		}
		
		//Get the global timestamp change
		val lowerGlobal = out._1.t - in._1.t;
		val upperGlobal = out._2.t - in._2.t;
		
		//Return the finished signature
		new Signature(energyResult, timeResult, stateMap, (lowerGlobal, upperGlobal))
	}
	
  /** Performs the functions of both "r()" and "e()" in the FOPARA Paper
    *
    * @param t The new timestamp for components (should be in the past)
    * @param out Component states representing the cost of evaluating the loop-body "many times"
    * @param pre Component states before evaluating the loop-body
    * @return A new set of component states with updated time and energy information
    */

  def computeEnergyBound(out: GlobalState, in: GlobalState, rf: Polynomial): GlobalState = (
    out.gamma.transform((comp,g) => g.update(g.timestamp min in.t, in(comp).energy + (out(comp).energy - in(comp).energy) * rf)),
    in.t + (out.t-in.t)*rf
  )

  /** Performs the functions of both "r()" and "e()" in the *Technical Report*
    *
    * @param t The new timestamp for components (should be in the past)
    * @param out Component states representing the cost of evaluating the loop-body after having done it "many times"
    * @param in  Component states representing the cost of evaluating the loop-body for the first time
    * @param pre Component states before evaluating the loop-body
    * @return A new set of component states with updated time and energy information
    */

  def computeEnergyBound_TR(out: GlobalState, in: GlobalState, pre: GlobalState, rf: Polynomial): GlobalState = (
    out.gamma.transform((comp,g) => g.update(g.timestamp min pre.t, (in(comp).energy-pre(comp).energy) +
      (out(comp).energy-in(comp).energy)*(rf-1) + pre(comp).energy)),
    pre.t + (in.t-pre.t)*rf
  )

  /** Performs energy analysis of the function 'program'
    *
    * @param entryPoint the function to analyse
    * @return The resulting global state after analysing the program
    */
  def analyse(entryPoint: String = "program"): (GlobalState, GlobalState) = {
    /** Compute fixed points of componentstates within a while-loop
     *
     * @param init set-of-componentstates (without global time)
     * @param expr controlling condition of while-loop
     * @param stm  main body of while loop
     * @param env  current environment mapping variables to expressions (pass-thru variable)
     */
    def fixPoint(init: States, expr: Expression, stm: Statement)(implicit env: Environment): States = {
      /** Throw away the temporal information (time, energy usage) and replace it with
          that of the initial state */
      def nontemporal(st: States) = st.transform((_,state)=>state.reset)

      /** The function we are going to iterate */
      def f(st: States) =
        nontemporal(analyse(analyse((st,Polynomial(0)), expr)._2, stm)._2.gamma)

      /** Starting values for the iteration */
      var cur   = nontemporal(init)
      var lub   = cur

      /** Find the fixpoint */
      val seen  = mutable.Set(cur)
      var limit = Config.fixPatience
      do {
        if({limit-=1; limit} <= 0)
          eh.fatalError(new ECAException("Model error: not all component delta functions have fixed points."))
        cur  = f(cur)
        lub  = lub.transform((name,st)=>st.lub(cur(name)))
      } while(seen.add(cur))

      /** Return the lub using original time and energy info */
      lub.transform((name,st)=>st.update(init(name).timestamp, init(name).energy))
    }
    
    def fixlbPoint(init: States, expr: Expression, stm: Statement)(implicit env: Environment): States = {
      /** Throw away the temporal information (time, energy usage) and replace it with
          that of the initial state */
      def nontemporal(st: States) = st.transform((_,state)=>state.reset)

      /** The function we are going to iterate */
      def f(st: States) =
        nontemporal(analyse(analyse((st,Polynomial(0)), expr)._1, stm)._1.gamma)

      /** Starting values for the iteration */
      var cur   = nontemporal(init)
      var glb   = cur

      /** Find the fixpoint */
      val seen  = mutable.Set(cur)
      var limit = Config.fixPatience
      do {
        if({limit-=1; limit} <= 0)
          eh.fatalError(new ECAException("Model error: not all component delta functions have fixed points."))
        cur  = f(cur)
        glb  = glb.transform((name,st)=>st.glb(cur(name)))
      } while(seen.add(cur))

      /** Return the lub using original time and energy info */
      glb.transform((name,st)=>st.update(init(name).timestamp, init(name).energy))
    }

    /** Convert an Expression to ECAValue, and complain if this is not possible */
    def resolve(expr: Expression): Polynomial = expr match {
      case Literal(x) => x
      case VarRef(x) => x
      case Add(l, r) => resolve(l) + resolve(r)
      case Subtract(l, r) => resolve(l) - resolve(r)
      case Multiply(l, r) => resolve(l) * resolve(r)
      case Divide(l, r) =>
        val rhs = resolve(r)
        if(rhs.split.length != 1 || rhs.divisor != (Seq.empty, 1)) {
          eh.error(new ECAException(s"Cannot divide by this expression.", r.position))
          resolve(l)
        } else
          resolve(l) / rhs
      case Exponent(l, r) =>
        val rhs = resolve(r)
        if(!rhs.vars.isEmpty) eh.error(new ECAException("Integer required as an exponent.", r.position))
        resolve(l) ** rhs.coef().toInt
	  case e : PrimaryExpression => e.repr
      case _ => eh.error(new ECAException("Could not resolve this value.", expr.position)); 0
    }

    /** Energy consumption analysis
     *
     * @param G       tuple of set-of-componentstates and the global timestamp
     * @param node    AST node under consideration
     * @param env  current environment mapping variables to expressions (pass-thru variable)
     * @return        updated tuple of set-of-componentstates and global timestamp
     *
     */
    def analyse(G: GlobalState, node: ASTNode)(implicit env: Environment): (GlobalState, GlobalState) = node match {
      case FunDef(name, parms, body)    => 	cenv.clear;
											
											//Initialize signature stuff
											cenv.setTract("rootu");
											analyse(G,body)
      case Skip()                       => (G, G)
      case If(pred, thenPart, elsePart) => val Gpre = if (Config.beforeSync) G.sync else G
                                           var G2 = analyse(Gpre,pred)
                                           G2 = (G2._1.update("CPU","ite"),G2._2.update("CPU","ite"))
                                           val G3 = (analyse(G2._1,thenPart), analyse(G2._2,thenPart))
                                           val G4 = (analyse(G2._1,elsePart), analyse(G2._2,elsePart))
                                           if(Config.afterSync)
                                            ((G3._1._1.sync min G4._1._1.sync min G3._2._1.sync min G4._2._1.sync).timeshift, 
                                            (G3._2._2.sync max G4._2._2.sync max G3._1._2.sync max G4._1._2.sync).timeshift)
                                           else
                                             (G3._1._1 min G4._1._1 min G3._2._1 min G4._2._1, 
                                                 G3._2._2 max G4._2._2 max G3._1._2 max G4._1._2)

      /*case While(pred, Some(rf), _, consq)
        if Config.techReport            => val Gpre = if (Config.beforeSync) G.sync else G
                                           var G2 = analyse(Gpre,pred)
                                           G2 = (G2._1.update("CPU","w"), G2._2.update("CPU","w"))
                                           val G3 = (analyse(G2._1,consq),analyse(G2._2,consq)) 
                                           val G3fix = (fixPoint(G3._2._2.gamma, pred, consq), G3._2._2.t)
                                           val G3fixlb = (fixlbPoint(G3._1._1.gamma, pred, consq), G3._1._1.t)
                                           val G4 = analyse(analyse(G3fix, pred).update("CPU","w"), consq)
                                         //val G4 = analyse(analyse(G3fix, pred), consq) // this is bug-compatible with the TR
                                           val iters = resolve(foldConstants(rf, env))
                                           if(Config.afterSync)
                                             computeEnergyBound_TR(G4.sync, G3.sync, Gpre, iters).timeshift
                                           else
                                             computeEnergyBound_TR(G4, G3, Gpre, iters)*/

      case While(pred, Some(rfl), Some(rf), consq) => val Gpre = if (Config.beforeSync) G.sync else G
                                           val Gfix = (fixPoint(Gpre.gamma, pred, consq), Gpre.t)
                                           val Gfixlb = (fixlbPoint(Gpre.gamma, pred, consq), Gpre.t)
                                           var G2 = analyse(Gfix,pred)
                                           var G2l = analyse(Gfixlb,pred)                                           
                                           G2 = (G2._1.update("CPU","w"), G2._2.update("CPU","w"))
                                           G2l = (G2l._1.update("CPU","w"), G2l._2.update("CPU","w"))
                                           val G3 = (analyse(G2._1,consq),analyse(G2._2,consq))
                                           val G3l = (analyse(G2l._1,consq),analyse(G2l._2,consq))
										   val rfsub = cenv.applyArrayLengths(rf)
										   val rflsub = cenv.applyArrayLengths(rfl)
                                           val iters = resolve(foldConstants(rfsub, env))
                                           val iterslb = resolve(foldConstants(rflsub, env))
                                           if(Config.afterSync)
                                             (computeEnergyBound((G3l._1._1.sync min G3l._2._1.sync), Gpre, iterslb).timeshift,
                                                 computeEnergyBound(G3._2._2.sync max G3._1._2.sync, Gpre, iters).timeshift)
                                           else
                                             (computeEnergyBound(G3l._1._1 min G3l._2._1, Gpre, iterslb),
                                                 computeEnergyBound(G3._2._2 max G3._1._2, Gpre, iters))

      case Composition(stms)            => (stms.foldLeft(G)(analyseFirst), stms.foldLeft(G)(analyseSecond))
      case Assignment(_, expr)          => var G2 = analyse(G,expr)
      									   (G2._1.update("CPU","a"), G2._2.update("CPU","a"))
      								   
      case FunCall(fun, args)
        if fun.isPrefixed               => val component = fun.prefix.get
                                           if(!G.gamma.contains(component)) {
                                             eh.error(new ECAException(s"Component not found: $component", node.position)); (G, G)
                                           } else
                                             (args.foldLeft(G)(analyseFirst).update(component, fun.name),
                                                 args.foldLeft(G)(analyseSecond).update(component, fun.name))

      case FunCall(fun, args)           => val funDef = program.functions(fun.name)
										   
										   //val unresolved = analyse(G, funDef.body)(env)
										   //if (!cenv.hasSignature(fun.name)) cenv.addFunctionSignature(fun.name, extractSignature((G, G), unresolved))
										   
                                           val resolvedArgs = args.map(foldConstants(_, env))
                                           val binding = funDef.parameters.map(n => VarRef(n)) zip resolvedArgs
										   val oldcenv = cenv
										   cenv = cenv.copy()
                                           val res = analyse(G, funDef.body)(env ++ binding)
										   cenv = oldcenv
										   res

      case stm:Annotated                => val annotatedEnv = stm.annotations.foldLeft(env) {
                                             case (env, (name, expr)) => env+(name->foldConstants(expr, env))
                                           }
                                           analyse(G, stm.underlying)(annotatedEnv)
						   
      case e:NAryExpression             => (e.operands.foldLeft(G)(analyseFirst).update("CPU", "e"),
    		  									e.operands.foldLeft(G)(analyseSecond).update("CPU", "e"))
												
	  case acc:ArrayAccess => (acc.accoperands.foldLeft(G)(analyseFirst).update("CPU", "macc"),
    		  									acc.accoperands.foldLeft(G)(analyseSecond).update("CPU", "macc"))
	  case acc:StructAccess => (acc.accoperands.foldLeft(G)(analyseFirst).update("CPU", "macc"),
    		  									acc.accoperands.foldLeft(G)(analyseSecond).update("CPU", "macc"))
	  case acc:UnionAccess => (acc.accoperands.foldLeft(G)(analyseFirst).update("CPU", "macc"),
    		  									acc.accoperands.foldLeft(G)(analyseSecond).update("CPU", "macc"))
      case _:PrimaryExpression          => (G, G)
	  
	  case ArrayDeclaration(name, subtype, length) => cenv.addArrayLength(name, length);
										analyse(G.update("CPU", "mdec"), length)
	  
	  case ArrayAssign(name, index, value) => var G2 = analyse(G, index);
											(analyseFirst(G2._1, value).update("CPU", "a").update("CPU", "macc"), analyseSecond(G2._2, value).update("CPU", "a").update("CPU", "macc"))	

	  case StructAssign(name, field, value) => analyse(G.update("CPU", "a").update("CPU", "macc"), value)
	  
	  case UnionAssign(name, field, value) => analyse(G.update("CPU", "a").update("CPU", "macc"), value)

      case w@While(pred, None, _,consq)   => eh.fatalError(new ECAException("Cannot analyse boundless while-loop", node.position))
	  
	  case Break() => (G, G)
	  
	  case Continue() => (G, G)
    }
    
    def analyseFirst (G: GlobalState, node: ASTNode)(implicit env: Environment): GlobalState = (
          analyse(G, node)._1
    )
    def analyseSecond (G: GlobalState, node: ASTNode)(implicit env: Environment): GlobalState = (
          analyse(G, node)._2
    ) 
	
    val initialState = if (components.contains("CPU")) {
		GlobalState.initial(components)
	} else {
		println("\nWarning: No CPU component found, defaulting to a non-consuming one!");
		GlobalState.initial(Map("CPU" -> Pentium0) ++ components)
	}
    
	//Get function signatures for all except the entrypoint
	for (f <- program.functions) {
		if (!f._2.name.equals(entryPoint)) {
			val rest = analyse(initialState, f._2.body)(Map.empty);
			val rest_sync = (rest._1.sync, rest._2.sync);
			cenv.addFunctionSignature(f._2.name, extractSignature((initialState, initialState), rest_sync));
		}
	}
	
	//Analyse the entrypoint
	val root         = program.functions.getOrElse(entryPoint, throw new ECAException(s"No $entryPoint function to analyse."))
    val finalState   = analyse(initialState, root)(Map.empty)
    finalState._1.sync
    finalState._2.sync
	
	//Print signatures
	cenv.printSignatures
	
	//Return
    finalState
  }
}

object EnergyAnalysis {

  /** For debugging (and exposition) purposes, a CPU which computes everything
    * instantly and consumes no power. (You know you want one!)
    */

  object Pentium0 extends DSLModel("CPU") {
    define T (e = 0, a = 0, w = 0, ite = 0, mdec = 0, macc = 0)
    define E (e = 0, a = 0, w = 0, ite = 0, mdec = 0, macc = 0)
  }

  /** The main function you want to use for debugging the analysis;
    * this is not intended to be user-friendly on purpose.
    */

  def main(args: Array[String]){
    import nl.ru.cs.ecalogic.config
    import parser.Parser
    import java.io.File
    import scala.io.Source

    val fileName = config.Options(args).headOption.getOrElse("program.eca")
    val noCPU = args.last == "nocpu"

    val file = new File(fileName)
    val source = Source.fromFile(file).mkString
    val errorHandler = new DefaultErrorHandler()//source = Some(source), file = Some(file))
    val program = new Parser(source, errorHandler).program()
    errorHandler.successOrElse("Parse errors encountered.")

    import model.examples._
    import model.examples.DemoComponents._
    val components = Map("Stub"->StubComponent, "BAD"->BadComponent, "Sensor"->Sensor, "Radio"->Radio) ++ (if(noCPU) Map("Stub"->StubComponent) else Map.empty)

    val checker = new SemanticAnalysis(program, components, errorHandler)
    checker.functionCallHygiene()
    checker.variableReferenceHygiene()
    errorHandler.successOrElse("Semantic errors; please fix these.")

    val consumptionAnalyser = new EnergyAnalysis(program, components, errorHandler)
    println(consumptionAnalyser.analyse().toString)
  }
}
