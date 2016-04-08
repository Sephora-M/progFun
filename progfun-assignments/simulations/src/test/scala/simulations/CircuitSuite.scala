package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("demux example"){
    
  val in, c2, c1, c0, out0, out1, out2, out3, out4, out5, out6, out7 = new Wire
  in.setSignal(true)
  
  //c2.setSignal(true)
  c1.setSignal(true)
  c0.setSignal(false)
  demux(in, List(/*c2,*/c1,c0), List(/*out7,out6,out5,out4,*/out3,out2,out1,out0))
  run
  println(/*" 7:" + out7.getSignal + " 6: " + out6.getSignal + " 5:" + out5.getSignal + " 4:"+ out4.getSignal
      + */" 3:" + out3.getSignal+ " 2:" + out2.getSignal+ " 1:" + out1.getSignal + " 0:" + out0.getSignal)
  assert(out2.getSignal === true)
  }
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 2")
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  
}
