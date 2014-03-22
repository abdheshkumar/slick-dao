package scala.slick.dao

import scala.slick.jdbc.meta.{MQName, MTable}
import java.io.FileWriter
import scala.slick.model.Model
import scala.slick.model.codegen.SourceCodeGenerator
import scala.slick.ast.ColumnOption
import scala.io.Source

/**
 * Created by Dmytro_Kovalskyi on 20.03.2014.
 */
object Generators {
  private val scheme = "public"

  private implicit def session = DBConnection.databasePool.createSession()

  def createGenerator(tableName: String, folder: String, packag: String, daoClass: String) =
    new Generator(tableName, folder, packag, daoClass)

  class Generator(tableName: String, folder: String, packag: String, daoClass: String) {

    import scala.slick.jdbc.meta.createModel

    def generateDao(updateFactory: Boolean = false, factoryPath: Option[String] = None) {
      val mTable = MTable(MQName(None, Some(scheme), tableName), "TABLE", null, None, None, None)
      val model = createModel(Seq(mTable), DBConnection.profile)
      val codeGen = new DaoSourceGenerator(model)
      val code = packageUp(codeGen, packag, daoClass, updateFactory)
      writeResult(s"$folder/$packag/${codeGen.entityName}.scala", code)
    }

    private def writeResult(path: String, data: String) {
      val writer = new FileWriter(path)
      writer.write(data)
      writer.close()
    }


    def packageUp(generator: DaoSourceGenerator, packag: String, daoClass: String, genFactory: Boolean) = {
      assert(generator.tables.size == 1)
      val table = generator.tables.head
      val (pkName,pkType) = table.columns.filter(_.model.options.contains(ColumnOption.PrimaryKey)).map{
        col => (col.rawName,col.rawType.toString)
      }.head
      val factory = if(genFactory) s"  def get$daoClass: I$daoClass = new $daoClass()" else ""
      s"""package $packag
import scala.slick.dao.DBConnection.profile.simple._
import scala.slick.dao._
${generator.code}

trait I$daoClass extends AbstractDAO with CRUDable[${table.TableClass.name}, ${pkType}]

class $daoClass(implicit innerSession: Session) extends I$daoClass {

  val entities: TableQuery[${table.TableClass.name}] = TableQuery[${table.TableClass.name}]

  def selectBy(entity: ${table.EntityType.name}) = {
    for (e <- entities if e.${pkName} === entity.${pkName}) yield e
  }

  def selectById(id: ${pkType}) = {
    for (e <- entities if e.${pkName} === id) yield e
  }
  $factory
}"""
    }
  }

  class DaoSourceGenerator(model: Model) extends SourceCodeGenerator(model) {
    // override mapped table and class name
    override def entityName = _.toLowerCase.toCamelCase

    override def tableName = _.toLowerCase.toCamelCase + "s"

    // remove ForeignKeyAction import
    override def code = super.code.substring(super.code.indexOf("\n"))

    // override table generator
    override def Table = new Table(_) {

      override def PlainSqlMapper = new PlainSqlMapper {
        override def enabled: Boolean = false
      }

      override def TableValue = new TableValue {
        override def enabled: Boolean = false
      }

      override def Index = new Index(_) {
        override def enabled: Boolean = false
      }

      override def TableClass = new TableClass {
        override def optionEnabled: Boolean = false
      }
    }
  }
}
