package edu.colorado.csci3155.project1
import org.scalatest.FunSuite
import Conversions._ 

class CompilerTest extends FunSuite {
    def printInstructionList(lst: List[StackMachineInstruction], name: String): Unit = {
        info(s"Your result for CompilerTest: $name")
        info("-------------------------------")
        lst.foreach(i => info(i.toString))
        info("-------------------------------")
    }

    test (testName = "simple expression 1") {
        /* compile constant expression 1.0 */
        val e = Const(1.0)
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "simple expression 1")
        assert(lst == List(PushNumI(1.0)))
    }

    test("simple expression 2") {
        /* Compile 1 + 2.5 */
        val e = Plus(Const(1.0), Const(2.5))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "simple expression 2")
        assert(lst == List(PushNumI(1.0), PushNumI(2.5), AddI))
    }

    test("simple expression 3"){
        /* compile (1.5 + 2.4) - (2.5 * 2.5) */
        val e = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "simple expression 3")
        assert(lst == List(PushNumI(1.5), PushNumI(2.4), AddI, PushNumI(2.5), PushNumI(2.5), MultI, SubI))
    }

    test("simple expression 4") {
        /* compile log ( exp( (1.5 + 2.4) - (2.5 * 2.5)) + exp (1.0 + 2.5) ) */
        val e1 = Minus(Plus(Const(1.5), Const(2.4)), Mult(Const(2.5), Const(2.5)))
        val e2 = Plus(Const(1.0), Const(2.5))
        val e3 = Log(Plus(Exp(e1), Exp(e2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e3)
        printInstructionList(lst, "simple expression 4")
        val lst1 = List(PushNumI(1.5), PushNumI(2.4), AddI, PushNumI(2.5), PushNumI(2.5), MultI, SubI)
        val lst2 = List(PushNumI(1.0), PushNumI(2.5), AddI)
        val expected = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI)
        assertResult(expected){lst}
    }

    test("LetBinding + Ident Test 1") {
        /* compile let x = 1 in x */
        val e1 = Let("x", Const(1.0), Ident("x")) //let x = 1 in x
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        printInstructionList(lst, "LetBinding + Ident Test 1")
        val expected = List(PushNumI(1.0), LoadEnv("x"), StoreEnv("x"),PopEnv)
        assertResult(expected){lst}
    }

    test("LetBinding + Ident Test 2") {
        /* compile let x = 1.5 in x + x */
        val e1 = Let("x", Const(1.5), Plus(Ident("x"), Ident("x"))) 
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        printInstructionList(lst, "LetBinding+ Ident Test 2")
        assert(lst == List(PushNumI(1.5), LoadEnv("x"), StoreEnv("x"), StoreEnv("x"), AddI, PopEnv))
    }

    test("LetBinding + Ident Test 3") {
        /* compile let x = 1.5 in let y = x + 1 in x * y */
        val x = Ident("x")
        val y = Ident("y")
        val innerLet = Let("y", Plus(x, Const(1.0)), Mult(x, y))
        val e1 = Let("x", Const(1.5), innerLet) //let x = 1.5 in let y = 1 + x in x * y
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        printInstructionList(lst, "Let Binding + Ident Test 3")
        val expected =  List(PushNumI(1.5),
                             LoadEnv("x"), 
                             StoreEnv("x"),
                             PushNumI(1.0),
                             AddI, 
                             LoadEnv("y"), 
                             StoreEnv("x"),
                             StoreEnv("y"),
                             MultI, 
                             PopEnv, 
                             PopEnv)
        assertResult(expected){lst}
    }

    test(testName = "LetBinding + Ident Test 4") {
        /* Compile let x = (let x = 1 in x + 1) in x + 2 */
        val x = Ident("x")
        val innerLet = Let("x", Const(1.0), Plus(x, Const(1)))
        val e1 = Let("x", innerLet, Plus(x, Const(2)))
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)
        printInstructionList(lst, "LetBinding + Ident Test 4")
        val expected = List (
            PushNumI(1.0),
            LoadEnv("x"),
            StoreEnv("x"),
            PushNumI(1.0),
            AddI,
            PopEnv, 
            LoadEnv("x"),
            StoreEnv("x"),
            PushNumI(2.0),
            AddI,
            PopEnv,
        )
         assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(math.abs(res.getDoubleValue - 4.0) <= 1E-05)
    }

      test(testName = "LetBinding + Ident Test 5") {
        /* let x = 1 in 
            let y  = (let x = 5.0 in x * 3) in 
              x - y 
        */
        val x = Ident("x")
        val y = Ident("y")
        val innerLet = Llet ("x") ~ 5.0 in x * 3 
        val e1 = Llet ("x") ~ 1.0 in (Llet("y") ~ innerLet in x - y )
        val lst = StackMachineCompiler.compileToStackMachineCode(e1)

        printInstructionList(lst, "LetBinding + Ident Test 5")
        val expected = List(
            PushNumI(1.0), 
            LoadEnv("x"),
            PushNumI(5.0), 
            LoadEnv("x"), 
            StoreEnv("x"), 
            PushNumI(3.0),
            MultI,
            PopEnv, 
            LoadEnv("y"),
            StoreEnv("x"),
            StoreEnv("y"),
            SubI, 
            PopEnv, 
            PopEnv
        )
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(math.abs(res.getDoubleValue + 14.0) <= 1E-05)
    }

    test(testName = "If-then-else-test-1") {
       /* compile let x = 25 in if (x >= 25) then x + 25.5 else 43 */
       val x = Ident("x")
       val e = If (x >= 25) Then (x + 25.5) Else (43)
       val e2 = Llet("x")~25 in e 
       info("Expression: " + e2) 
       val lst = StackMachineCompiler.compileToStackMachineCode(e2)
       printInstructionList(lst, "If-then-else-test-1")
       val expected = List (
        PushNumI(25.0), 
        LoadEnv("x"), 
        StoreEnv("x"),
        PushNumI(25.0),
        GeqI, 
        CSkipI(4), 
        StoreEnv("x"), 
        PushNumI(25.5), 
        AddI, 
        SkipI(1), 
        PushNumI(43.0), 
        PopEnv
       )
       assertResult(expected){lst}
       val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
       assert(math.abs(res.getDoubleValue - 50.5) <= 1E-05)

    }
    test(testName = "If-then-else-test-2") {
       /* compile let y = 24.5 in let x = y + 45 in if (x >= 25) then x + 25.5 else 43 */
       val x = Ident("x")
       val y = Ident("y")
       val e = If (x >= 25) Then (x + 25.5) Else (43)
       val e2 = Llet("x")~(y + 45) in e 
       val e3 = Llet("y")~24.5 in e2
       val lst = StackMachineCompiler.compileToStackMachineCode(e3)
       printInstructionList(lst, "if-then-else-test2")
       val expected = List( PushNumI(24.5), 
         LoadEnv("y"), 
         StoreEnv("y"), 
         PushNumI(45.0), 
         AddI,
         LoadEnv("x"), 
         StoreEnv("x"),
        PushNumI(25.0),
        GeqI, 
        CSkipI(4), 
        StoreEnv("x"), 
        PushNumI(25.5), 
        AddI, 
        SkipI(1), 
        PushNumI(43.0), 
        PopEnv,
        PopEnv
       )
       assertResult(expected){lst}
       val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
       assert(math.abs(res.getDoubleValue - 95.0) <= 1E-05)
    }
    test(testName = "test-and-simple-1") {
        /* 1 >= 0 && 5 >= -4 */
        val e = And(Geq(1, 0), Geq(5, -4)) // We have implicit conversion from numbers to constants
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "test-and-simple-1")
        val expected= List( PushNumI(1.0), 
                PushNumI(0.0), 
                GeqI, 
                CSkipI(4), 
                PushNumI(5.0), 
                PushNumI(-4.0), 
                GeqI, 
                SkipI(1), 
                PushBoolI(false))
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(res.getBooleanValue == true)
    }
    test(testName = "test-and-simple-2") {
        /* 1 >= 5 && 5/0 */
        val e = And(Geq(1, 5), Div(5,0)) // We have implicit conversion from numbers to constants
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "test-and-simple-2")
        val expected= List( PushNumI(1.0), 
                PushNumI(5.0), 
                GeqI, 
                CSkipI(4), 
                PushNumI(5.0), 
                PushNumI(0.0), 
                DivI, 
                SkipI(1), 
                PushBoolI(false))
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(res.getBooleanValue == false)
    }

    test(testName = "test-and-simple-3") {
        /* 5 >= 1 && -4 >= 5 */
        val e = And(Geq(5, 1), Geq(-4, 5)) // We have implicit conversion from numbers to constants
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "test-and-simple-3")
        val expected= List( PushNumI(5.0), 
                PushNumI(1.0), 
                GeqI, 
                CSkipI(4), 
                PushNumI(-4.0), 
                PushNumI(5.0), 
                GeqI, 
                SkipI(1), 
                PushBoolI(false))
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(res.getBooleanValue == false)
    }
    test(testName = "test-or-simple-1") {
        /* 1 >= 5 || 5 >= -4 */
        val e = Or(Geq(1, 5), Geq(5, -4)) // We have implicit conversion from numbers to constants
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "test-or-simple-1")
        val expected= List( PushNumI(1.0), 
                PushNumI(5.0), 
                GeqI, 
                CSkipI(2),
                PushBoolI(true), 
                SkipI(3), 
                PushNumI(5.0), 
                PushNumI(-4.0), 
                GeqI)
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(res.getBooleanValue == true)
    }

    test(testName = "test-or-not-1") {
        /* 5 >= 1 || 0/0 >= 0 */
        val e = Not(Or(Geq(5, 1), Geq(Div(0, 0), 0))) // We have implicit conversion from numbers to constants
        val lst = StackMachineCompiler.compileToStackMachineCode(e)
        printInstructionList(lst, "test-or-not-1")
        val expected= List( PushNumI(5.0), 
                PushNumI(1.0), 
                GeqI, 
                CSkipI(2),
                PushBoolI(true), 
                SkipI(5), 
                PushNumI(0.0), 
                PushNumI(0.0),
                DivI, 
                PushNumI(0.0), 
                GeqI,
                NotI)
        assertResult(expected){lst}
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst)
        assert(res.getBooleanValue == false)
    }

    
}
