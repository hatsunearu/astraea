package astraea

import spinal.core._
import spinal.lib._

import scala.math._
import spinal.core.sim._

class CicDecimator(rmn: (Int, Int, Int), input_width: Int, bits_to_lose: Array[Int]) extends Component {

    val (r,m,n) = rmn
    val log2 = (x: Double) => (log(x) / log(2))
    val total_bits = ceil(n * log2(r*m) + input_width - 1).toInt
    val stage_widths = bits_to_lose.map( x => if (x < 0) { total_bits } else { total_bits - x })

    assert(bits_to_lose.length == n * 2 + 1, "bits_to_lose must have a length equal to n * 2 + 1")
    assert(m == 1 || m == 2, "m must be either 1 or 2")
    assert(n <= r, "n must be <= r")

    println(s"[CIC Decimator] R: $r, M: $m, N: $n")
    println(s"[CIC Decimator] Total bits: ${total_bits.toString}")


    // #### IO ####
    val io = new Bundle {
        val input = in SInt(input_width bits)
        val output = out SInt((total_bits - bits_to_lose.last) bits)
        val output_valid = out Bool
    }




    // Integrators
    var prev_int_reg = io.input.resize(total_bits)
    for (cur_width <- stage_widths.slice(0, stage_widths.length / 2)) {
        // assert that register width is monotonically decreasing
        assert(prev_int_reg.getWidth - cur_width >= 0)

        val integrator = new CicIntegrator(cur_width)

        // truncate previous stage's output to input of current stage
        integrator.io.input := (prev_int_reg >> (prev_int_reg.getWidth - cur_width)).resized
        val cur_output = SInt(cur_width bits)
        cur_output := integrator.io.output
        prev_int_reg = cur_output
    }
    val integrator_output = prev_int_reg


    // counts up from 0 to R-1 that goes around and services each of the N stage combs
    val state_counter = Reg( UInt( log2Up(r) bits)) init 0
    state_counter := state_counter + 1
    io.output_valid := False
    when (state_counter === r-1) {
        state_counter := 0
    }
    // io.output_valid asserted after the last comb is serviced at state_counter === n-1
    // if n === r (the highest n can be wrt r), then it would have wrapped around, so assert when === 0
    if (n == r) {
        when (state_counter === 0) {
            io.output_valid := True
        }
    }
    else {
        when (state_counter === n) {
            io.output_valid := True
        }
    }

    var prev_comb_reg = integrator_output
    for ((cur_width, ind) <- stage_widths.slice(stage_widths.length / 2, stage_widths.length - 1).zipWithIndex) {
        assert(prev_comb_reg.getWidth - cur_width >= 0)

        val prev_output = (prev_comb_reg >> (prev_comb_reg.getWidth - cur_width)).resized

        val comb = new CicComb(cur_width, m)

        comb.io.input := prev_output
        comb.io.enable := (state_counter === ind)

        prev_comb_reg = comb.io.output
    }

/*

    // Combs
    //var prev_comb_reg = RegNext(integrator_output) init(0) simPublic()
    var prev_comb_reg = integrator_output
    // take the second half of the stage widths as an iterator w/ index (except the last one)
    for ((cur_width, ind) <- stage_widths.slice(stage_widths.length / 2, stage_widths.length - 1).zipWithIndex) {
        // assert that register width is monotonically decreasing
        assert(prev_comb_reg.getWidth - cur_width >= 0)

        // truncate previous output to cur_width and initialize output of current stage
        val prev_output = (prev_comb_reg >> (prev_comb_reg.getWidth - cur_width)).resized
        val cur_output = Reg( SInt(cur_width bits)) init 0
        
        // perform subtraction with Z^-M and push along delay elements
        if (m == 1) {
            val delay1 = Reg( SInt(cur_width bits) ) init 0
            when ( state_counter === ind  ) {
                cur_output := prev_output - delay1
                delay1 := prev_output
            }
        }
        else {
            val delay1 = Reg( SInt(cur_width bits) ) init 0
            val delay2 = Reg( SInt(cur_width bits) ) init 0
            when ( state_counter === ind ) {
                cur_output := prev_output - delay2
                delay1 := prev_output
                delay2 := delay1
            }
        }

        prev_comb_reg = cur_output
    }
*/
    // truncate last stage
    io.output := (prev_comb_reg >> (prev_comb_reg.getWidth - stage_widths.last)).resized
}

class CicIntegrator(width: Int) extends Component {
    val io = new Bundle {
        val input = in SInt(width bits)
        val output = out SInt(width bits)
    }
    val accumulator = Reg(SInt(width bits)) init(0)

    io.output := accumulator
    accumulator := io.input + accumulator
}

class CicComb(width: Int, m: Int) extends Component {
    val io = new Bundle {
        val input = in SInt(width bits)
        val output = out SInt(width bits)
        val enable = in Bool
    }

    val delay1 = Reg(SInt(width bits)) init(0)
    val delay2 = Reg(SInt(width bits)) init(0)
    val output_reg = Reg(SInt(width bits)) init(0)
    io.output := output_reg

    if (m == 1) {
        when (io.enable === True) {
            output_reg := io.input - delay1
            delay1 := io.input
        }
    }

    else if (m == 2) {
        when (io.enable === True) {
            output_reg := io.input - delay2
            delay1 := io.input
            delay2 := delay1
        }
    }
    else {
        throw new UnsupportedOperationException("m should be 1 or 2")
    }
}
