package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case  LoadEnv(s: String) => {
                if (stack.isEmpty) {throw new RuntimeException(s"ErrLoadEnv: LoadEnv on Empty Operand Stack!")}
                else {((stack.tail), (s,stack.head) :: env)}
            }
            case  StoreEnv(s: String) => {
                val a = env.find(_._1 == s)
                if (a != None) {
                    (a.get._2 :: stack, env)
                }
                else{throw new RuntimeException(s"ErrStoreEnv: could not find identifier")}
            }
            case  PopEnv => (stack, env.tail)
            case  PushNumI(f: Double) =>  (Num(f)::stack, env)
            case  PushBoolI(b: Boolean) => (Bool(b) :: stack, env)
            case PopI => {
                if (stack.isEmpty) {throw new RuntimeException(s"ErrPopI: PopI on Empty Operand Stack!")}
                else {(stack.tail, env)}}
            case  AddI => {
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrAddI: AddI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head
                    val b = stack.tail.head
                    val result = Num(a.getDoubleValue + b.getDoubleValue)
                    (result :: stack.tail.tail, env)
                }
            }

            case  SubI => {
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrSubI: SubI on Empty Operand Stack!")
                }
                else
                    {
                        val a = stack.head
                        val b = stack.tail.head
                        val result = Num(b.getDoubleValue - a.getDoubleValue)
                        (result :: stack.tail.tail, env)
                    }
            }
            case  MultI => {
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrMultI: MultI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head
                    val b = stack.tail.head
                    val result = Num(a.getDoubleValue * b.getDoubleValue)
                    (result :: stack.tail.tail, env)
                }
            }
            case  DivI => {
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrDivI: DivI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head
                    val b = stack.tail.head
                    if(b.getDoubleValue == 0) {throw new RuntimeException(s"DivAddI: cannot divide by 0")}
                    val result = Num(b.getDoubleValue / a.getDoubleValue)
                    (result :: stack.tail.tail, env)
                }
            }
            case  ExpI => {
                if(stack.isEmpty) {throw new RuntimeException(s"ErrLogI: LogI on Empty Operand Stack!")}
                else{
                val a = stack.head.getDoubleValue
                val result = Num(math.exp(a))
                (result :: stack.tail, env)}
            }
            case  LogI => {
                if (stack.isEmpty) {throw new RuntimeException(s"ErrLogI: LogI on Empty Operand Stack!")}
                else {
                        val a = stack.head.getDoubleValue
                    if(a < 0) {throw new RuntimeException(s"ErrLogI: LogI on Negative Value")}
                    else {
                        val result = Num(math.log(a))
                        (result :: stack.tail, env)
                        }
                    }
            }
            case  SinI => {
                if (stack.isEmpty) {
                    throw new RuntimeException(s"ErrSinI: SinI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head.getDoubleValue
                    val result = Num(math.sin(a))
                    (result :: stack.tail, env)
                }
            }
            case  CosI => {
                if (stack.isEmpty) {
                    throw new RuntimeException(s"ErrCosI: CosI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head.getDoubleValue
                    val result = Num(math.cos(a))
                    (result :: stack.tail, env)
                }
        }
            case  GeqI => {
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrGeqI: GeqI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head
                    val b = stack.tail.head
                    val result = Bool(b.getDoubleValue >= a.getDoubleValue)
                    (result :: stack.tail.tail, env)
                }
            }
            case  EqI  =>
                if (stack.isEmpty || stack.tail.isEmpty) {
                    throw new RuntimeException(s"ErrEqI: EqI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head
                    val b = stack.tail.head
                    val result = Bool(a == b)
                    (result :: stack.tail.tail, env)
                }
            case  NotI => {
                if (stack.isEmpty) {
                    throw new RuntimeException(s"ErrNotI: NotI on Empty Operand Stack!")
                }
                else {
                    val a = stack.head.getBooleanValue
                    val result = Bool(!a)
                    (result :: stack.tail, env)
                }
            }
            case  CSkipI(numToSkip: Int)  =>{
                if(stack.isEmpty) {throw new RuntimeException(s"ErrCSkipI: CSkipI on Empty Operand Stack!")}
                else{
                    val a = stack.head.getBooleanValue
                    if(a == true)
                        {(stack.tail, env)}
                    else{
                        if(stack.length < numToSkip)
                            {throw new RuntimeException(s"ErrCSkipI: trying to jump farther than stack length!")}
                        else
                            {(stack.drop(numToSkip), env)}
                    }
                }

            }
            case  SkipI(numToSkip: Int)  => {
                if (stack.isEmpty) {
                    throw new RuntimeException(s"ErrSkipI: SkipI on Empty Operand Stack!")
                }
                else {

                        if (stack.length < numToSkip) {
                            throw new RuntimeException(s"ErrSkipI: trying to jump farther than stack length!")
                        }
                        else {
                            (stack.drop(numToSkip), env)
                        }

                }

            }
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}