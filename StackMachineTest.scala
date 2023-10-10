package edu.colorado.csci3155.project1

import org.scalatest.FunSuite

class StackMachineTest extends FunSuite {

    def testValueOfIdent(env: StackMachineEmulator.RuntimeStack, x: String, f: Double) = {
        val v: Value  = env.find{ 
            case (s, v) => s == x
        } match {
            case Some((_, v)) => v
            case _ => { 
                assert(false, s"Failed to find identifier $x in environment");
                Num(0.0)
            }
        }
        assert( math.abs(v.getDoubleValue -f) <= 1E-06, s"Expected: f, obtained $v")
    }

    test("stack machine test 1") {
        val lst1 = List(PushNumI(2.5), PushNumI(3.5), AddI)
        val (res, _) = StackMachineEmulator.emulateStackMachine(lst1)
        assert(res.getDoubleValue == 6.0)
    }

    test("stack machine test 2") {
        val lst1 = List(PushNumI(2.5), PushNumI(3.5), AddI, ExpI, LogI)
        val (res, f) = StackMachineEmulator.emulateStackMachine(lst1)
        assert(math.abs(res.getDoubleValue - 6.0) <= 1e-05)
    }

    test("stack machine test 3") {
        val lst1 = List(PushNumI(3.5), PushNumI(2.5), PushNumI(4.5), PushNumI(5.2), AddI, LoadEnv("x"), LoadEnv("y"), LoadEnv("z"), StoreEnv("y"))
        val (res, fenv) = StackMachineEmulator.emulateStackMachine(lst1)
        testValueOfIdent(fenv, "x", 9.7)
        testValueOfIdent(fenv, "y", 2.5)
        testValueOfIdent(fenv, "z", 3.5)
    }
    

    test("stack machine test 4") {
        val lst4 = List(PushNumI(3.5), PushNumI(2.5), PushNumI(4.5), PushNumI(5.2),  LoadEnv("x"), LoadEnv("y"), LoadEnv("z"), LoadEnv("w"),
                         StoreEnv("y"), StoreEnv("w"), AddI, LoadEnv("res1"), StoreEnv("x"), StoreEnv("z"), MultI)
        val (res2, fenv) = StackMachineEmulator.emulateStackMachine(lst4)
        testValueOfIdent(fenv, "x", 5.2)
        testValueOfIdent(fenv, "y", 4.5)
        testValueOfIdent(fenv, "z", 2.5)
        testValueOfIdent(fenv, "w", 3.5)
        testValueOfIdent(fenv, "res1", 8.0)
        assert( math.abs(res2.getDoubleValue - 5.2 * 2.5) <= 1E-06)
    }

    test("stack machine test 5") {
        val lst1 = List(PushNumI(1.5), PushNumI(2.4), AddI, PushNumI(2.5), PushNumI(2.5), MultI, SubI)
        val lst2 = List(PushNumI(1.0), PushNumI(2.5), AddI)
        val lst3 = lst1 ++ List(ExpI) ++ lst2 ++ List(ExpI) ++ List(AddI) ++ List(LogI) 
        val (res, f) = StackMachineEmulator.emulateStackMachine(lst3)
        assert(math.abs(res.getDoubleValue - 3.50287) <= 1E-04)
    }


}
