import org.scalatest.FunSuite

class ShotTest extends FunSuite {

  test("testFromTopInclination") {
    //internal angle units (full circle = 2^16, up = 0x4000, down = 0xC000)
    val up: Short = 0x4000.toShort
    assert(Shot.fromTopInclination(up) === 90.0)

    val down: Short = 0xC000.toShort
    assert(Shot.fromTopInclination(down) === -90.0)

    val full: Short = scala.math.pow(2, 16).toShort
    assert(Shot.fromTopInclination(full) === 0.0)

    val zero: Short = 0
    assert(Shot.fromTopInclination(zero) === 0.0)

    val plusOne: Short = (0x4000.toShort + scala.math.pow(2, 16) / 360.0).toShort
    assert(scala.math.abs(Shot.fromTopInclination(plusOne) - 91.0) < 0.001)
  }

  //  test("testFromTopFlags") {
  //
  //  }
  //
  //  test("testApply") {
  //
  //  }
  //
  //  test("testApply") {
  //
  //  }
  //
  //  test("testUnapply") {
  //
  //  }
  //
  //  test("testFromTopRoll") {
  //
  //  }
  //
  //  test("testFromTopAzimuth") {
  //
  //  }
  //
  //  test("testFromTopDist") {
  //
  //  }

}
