package astraea

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.io.Source
import scala.util.Random
import com.typesafe.config.{ Config, ConfigFactory }
import java.io.File
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer


class AstraeaBuildContext(filename: String) {
  val config = ConfigFactory.parseFile(new File(filename))

  val cic_decim_cfg = new CicDecimConfig(this)
}

class CicDecimConfig(ctx: AstraeaBuildContext) {
  val (r, m, n) = (
  ctx.config.getConfig("cic_decimator").getInt("r"), 
  ctx.config.getConfig("cic_decimator").getInt("m"),
  ctx.config.getConfig("cic_decimator").getInt("n"))

  val cic_input_width = ctx.config.getConfig("cic_decimator").getInt("input_width")
  val truncation = ctx.config.getConfig("cic_decimator").getIntList("truncation").asScala.map((e: Integer) => e.toInt).toArray
  val impulse_response = ctx.config.getConfig("cic_decimator").getIntList("impulseresponse").asScala.map((e: Integer) => e.toInt).toArray
}

object AstraeaCicDecim {
  def main(args: Array[String]): Unit = {

    val ctx = new AstraeaBuildContext("../test.json")
    val cfg = ctx.cic_decim_cfg
    val config = SimConfig.withWave.workspacePath("./sim/cic_decimator")

    // get a model without any truncation
    val compiled_notrunc = config
      .compile(new CicDecimator((cfg.r,cfg.m,cfg.n), cfg.cic_input_width, Array.fill[Int](cfg.truncation.length)(0)))

    val compiled_trunc = config
      .compile(new CicDecimator((cfg.r,cfg.m,cfg.n), cfg.cic_input_width, cfg.truncation))

    test_impulse_response_notrunc(ctx, compiled_notrunc)
    test_snr(ctx, compiled_notrunc, compiled_trunc)

  }

  // tests impulse response of the filter without any truncation
  def test_impulse_response_notrunc(ctx: AstraeaBuildContext, compiled_notrunc: SimCompiled[CicDecimator]) {

    val cfg = ctx.cic_decim_cfg
    // impulse response test w/ no truncation
    for (i <- 0 until cfg.r) {
      compiled_notrunc.doSim("impulseresponse") { dut =>
        println(s"Testing CIC Decimator Impulse Response with delay after valid= $i")
        val measured_impulse_response = new ListBuffer[Int]()

        dut.clockDomain.forkStimulus(period=10)

        // apply pulse at i cycles after the output_valid assertion
        dut.io.input #= 0
        dut.clockDomain.waitActiveEdgeWhere(dut.io.output_valid.toBoolean)
        dut.clockDomain.waitSampling(i)
        dut.io.input #= 1
        dut.clockDomain.waitSampling(1)
        dut.io.input #= 0

        // wait two output_valid assertions
        if (dut.io.output.toInt != 0) {
          println(s"Output changed unexpectedly!: delay = $i")
        }
        // the wait after the pulse could have already put us on the output valid edge
        if (!dut.io.output_valid.toBoolean) {
          dut.clockDomain.waitActiveEdgeWhere(dut.io.output_valid.toBoolean)
        }
        if (dut.io.output.toInt != 0) {
          println(s"Output changed unexpectedly!: delay = $i")
        }
        dut.clockDomain.waitActiveEdgeWhere(dut.io.output_valid.toBoolean)

        // sample responses and store them into the buffer
        var impulse: Int = -1
        while (impulse != 0) {
          dut.clockDomain.waitActiveEdgeWhere(dut.io.output_valid.toBoolean)
          impulse = dut.io.output.toInt

          if (impulse != 0) {
            measured_impulse_response += impulse
          }
        }

        println(s"delay = $i result: ${measured_impulse_response.toList.mkString(", ")}")

        // verify against the expected impulse response
        val expected_impulse_response = cfg.impulse_response.drop(cfg.r-i-1).grouped(cfg.r).map(_.head).toList
        if (expected_impulse_response != measured_impulse_response.toList) {
          println(s"!!!!! Test FAILED: Impulse Response doesn't match up: delay = $i")
        }
        else {
          println(s"Impulse Response test passed with delay = $i")
        }
      }
    } 
  }

  def test_snr(ctx: AstraeaBuildContext, compiled_notrunc: SimCompiled[CicDecimator], compiled_trunc: SimCompiled[CicDecimator]) {
    val seed = 393939 //Random.nextInt()
    assert(compiled_trunc.dut.io.input.getWidth == compiled_notrunc.dut.io.input.getWidth, "input with of the truncated and non truncated version must be the same")

    val trunc_output = ListBuffer[Int]();
    val notrunc_output = ListBuffer[Int]();

    // 2nd order function to collect samples given the destination listbuffer
    val collect_samples = { samples: ListBuffer[Int] => 
      { dut: CicDecimator => 
        dut.clockDomain.forkStimulus(period=10)
        dut.io.input #= 0

        // wait two active edges before feeding input in
        dut.clockDomain.waitSamplingWhere(dut.io.output_valid.toBoolean)
        dut.clockDomain.waitSamplingWhere(dut.io.output_valid.toBoolean)


        for (i <- 0 to 100000) {
          dut.io.input #= Random.nextInt((dut.io.input.maxValue - dut.io.input.minValue + 1).toInt) + dut.io.input.minValue
          dut.clockDomain.waitSampling(1)
          if (dut.io.output_valid.toBoolean) {
            samples += dut.io.output.toInt
          }
        }
      }
    }

    compiled_notrunc.doSim("test_snr_notrunc", seed)(collect_samples(notrunc_output))
    compiled_trunc.doSim("test_snr_trunc", seed)(collect_samples(trunc_output))

    /*
    import java.io._
    val writer1 = new PrintWriter(new File("notrunc_output"))
    notrunc_output.foreach(x => writer1.println(x))

    val writer2 = new PrintWriter(new File("trunc_output"))
    trunc_output.foreach(x => writer2.println(x))

    writer1.close()
    writer2.close()
*/
    val (signal_pwr, noise_pwr) = notrunc_output.zip(trunc_output).map(
      {case (x, y) => ((x * x), ((x-y) * (x-y)))}).unzip
    
    
    val average_snr = signal_pwr.sum / noise_pwr.sum
    
    printf(s"average_snr: ${10*math.log10(average_snr)}")


    
  }
}



object AstraeaTest {
  def main(args: Array[String]): Unit = {
    SimConfig
      .withWave
      .workspacePath("./sim")
      //.compile(new CicDecimator((5,1,5), 12, Array[Int](-1, 1, 3, 4, 5, 5, 6, 7, 8, 8, 11)))
      .compile(new CicDecimator((5,1,5), 12, Array[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
      //.compile(new CicDecimator((5,1,5), 12, Array[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11)))
      .doSim{ dut =>
        dut.clockDomain.forkStimulus(period=10)

        for (i <- 0 until 5) {
          dut.io.input #= 0
          dut.clockDomain.waitActiveEdgeWhere(dut.io.output_valid.toBoolean)
          dut.clockDomain.waitSampling(i)
          //dut.io.input #= (1 << 11) - 1
          dut.io.input #= 1
          dut.clockDomain.waitSampling()
          dut.io.input #= 0
          dut.clockDomain.waitSampling(100)
          dut.clockDomain.assertReset()
          sleep(10)
          dut.clockDomain.deassertReset()
        }
        
      }/*
      SimConfig
        .withWave
        .workspacePath("./sim")
        //.compile(new CicDecimator((5,1,5), 12, Array[Int](-1, 1, 3, 4, 5, 5, 6, 7, 8, 8, 11)))
        .compile(new CicDecimator((5,1,5), 12, Array[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
        .doSim{ dut =>
          dut.clockDomain.forkStimulus(period=10)
          dut.prev_comb_reg #= 100
          dut.clockDomain.waitSampling(50)
        }
        */
  }
}