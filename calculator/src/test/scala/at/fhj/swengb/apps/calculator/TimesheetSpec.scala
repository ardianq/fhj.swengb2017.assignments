package at.fhj.swengb.apps.calculator

import java.nio.file.{Files, Paths}

import org.scalatest.WordSpecLike

class TimesheetSpec extends WordSpecLike {
  "Timesheet-calculator" should {
    "not " in {
      assert(Files.exists(Paths.get("C:\\workspace\\fhj.swengb2017.assignments\\calculator\\timesheet-calculator.adoc")))
    }
  }
}
