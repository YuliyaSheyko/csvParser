package parser.csv

import org.scalatest.WordSpec
import org.scalatest.matchers.should.Matchers
import parser.csv.Main.{CsvOrgManager, Organization}

class CsvManagerTest extends WordSpec with Matchers {
  val content = Iterator(
    "l0,,l1|l2|l3",
    "l1,l0,",
    "l2,l0,l4|l5",
    "l3,l0,",
    "l4,l2,",
    "l5,l2,",
    "n1,,n2|n3"
  )
  val noRootContent = Iterator("l1,l0,", "l2,l0,l4|l5", "l4,l2,", "l5,l2,")
  val newManager = new CsvOrgManager(content)

  "CsvOrgManager" when {
    "getRootOrganizations" should {
      "return list of root organizations" in {
        newManager.getRootOrganizations() shouldBe Vector(
          Organization("l0", None, List("l1", "l2", "l3")),
          Organization("n1", None, List("n2", "n3"))
        )
      }
    }
  }
  "CsvOrgManager" when {
    "getRootOrganizations" should {
      "return empty list" in {
        val myManager = new CsvOrgManager(noRootContent)
        myManager.getRootOrganizations() shouldBe Vector()
      }
    }
  }
  "CsvOrgManager" when {
    "getOrgTree" should {
      "return tree of organizations" in {
        val name = "l0"
        newManager.getOrgTree(name) shouldBe List(
          Organization("l0", None, List("l1", "l2", "l3")),
          Organization("l1", Some("l0"), List()),
          Organization("l2", Some("l0"), List("l4", "l5")),
          Organization("l4", Some("l2"), List()),
          Organization("l5", Some("l2"), List()),
          Organization("l3", Some("l0"), List())
        )
      }
    }
  }
}
