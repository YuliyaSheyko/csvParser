package parser.csv

import scala.io.Source

object Main extends App {

  case class Organization(name: String,
                          parent: Option[String],
                          children: List[String])

  trait OrgManager {
    def getRootOrganizations(): Seq[Organization]
    def getOrgTree(name: String): Seq[Organization]
  }

  class CsvOrgManager(lines: Iterator[String]) extends OrgManager {

    private val organizations = lines.map { lineToOrganizationData }.toVector
    private val orgSearch = organizations.map { org =>
      org.name -> org
    }.toMap

    private def lineToOrganizationData(line: String) = {
      val chunks = line.trim.split(",")
      Predef.assert(chunks.size >= 2, s"Illegal CSV line $line")
      val name = chunks(0)
      val parent = if (chunks(1).isBlank) None else Some(chunks(1))
      val child =
        if (chunks.size == 2) List() else chunks(2).split("\\|").toList
      Organization(name, parent, child)
    }

    override def getRootOrganizations(): Seq[Organization] =
      organizations.filter {
        _.parent.isEmpty
      }

    override def getOrgTree(name: String): Seq[Organization] = {
      val rootOrg = orgSearch(name)
      rootOrg :: rootOrg.children.flatMap { child =>
        getOrgTree(child)
      }
    }
  }

  val stream = getClass.getResource(args.head)
  val source = Source.fromFile(stream.getFile)
  val lines = source.getLines

  val manager = new CsvOrgManager(lines)

  println(manager.getRootOrganizations())
  println(manager.getOrgTree(manager.getRootOrganizations().head.name))

}
