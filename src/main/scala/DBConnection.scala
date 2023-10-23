import java.sql.{DriverManager, ResultSet}
import java.sql.ResultSetMetaData
import java.sql.Types

class DBConnection(val connStr:String, val user:String, val password:String, val driver: String ) {

  var connection = createConnection(connStr, user, password, driver)

  def createConnection(connStr: String, user: String, password: String, driver: String) = {
    Class.forName(driver)
    DriverManager.getConnection(connStr, user, password)
  }

  def checkTableExist(tableName: String): Boolean = {
    val meta = connection.getMetaData()
    val rs = meta.getTables(null, null, tableName.toLowerCase, null)
    val exist = if (rs.next()) {
      true
    } else {
      false
    }
    exist
  }

  def selectCount(table: String, instance_id: Int): Int = {
    try {
      connection = createConnection(connStr, user, password, driver)
      val stmt = connection.prepareStatement(s" select count(*)  from $table where id > $instance_id ")
      val rs = stmt.executeQuery()
      val counter = if (rs.next()) rs.getInt("count") else 0
      stmt.close()
      counter
    } catch {
      case e => {
        throw e
      }
    } finally {
      connection.close()
    }
  }

  def getRows(instance_id: Int, windowSize: Int, table: String): List[List[String]] = {
    try {
      connection = createConnection(connStr, user, password, driver)
      val stmt = connection.prepareStatement(s"select * from $table  where id> $instance_id order by id limit $windowSize")
      val rs = stmt.executeQuery()
      val rows = rowsToList(rs, List.empty[List[String]])
      stmt.close()
      rows
    } catch {
      case e => {
        throw e
      }
    } finally {
      connection.close()
    }
  }


  def rowsToListHelper(rs: ResultSet, resultSetMetaData: ResultSetMetaData): List[String] = {
    var appendedList = List.empty[String]
    var i = 1
    while (i != resultSetMetaData.getColumnCount + 1) {
      val type_ = resultSetMetaData.getColumnType(i)
      val value = type_ match {
        case Types.CHAR => rs.getString(i)
        case Types.VARCHAR => rs.getString(i)
        case Types.INTEGER => rs.getInt(i)
        case Types.DOUBLE => rs.getDouble(i)
        case Types.FLOAT => rs.getFloat(i)
      }
      appendedList = appendedList.appended(value.toString)
      i = i + 1
    }
    appendedList

  }

  def rowsToList(rs: ResultSet, resultList: List[List[String]]): List[List[String]] = {
    if (rs.next()) {
      val resultSetMetaData = rs.getMetaData
      val toAppend = rowsToListHelper(rs, resultSetMetaData)
      val ty = resultList match {
        case ls => toAppend :: ls
        case _ => List(toAppend)
      }
      rowsToList(rs, ty)
    } else {
      resultList
    }
  }


  def createTable(tableName: String, columnNane: List[String]): Int = {
    try {
      connection = createConnection(connStr, user, password, driver)
      val query = s"CREATE TABLE $tableName  (id SERIAL,  ${columnNane.mkString(" int4 NOT NULL ,")} int4 NOT NULL , primary key (id)  )  "
      val stmt = connection.prepareStatement({
        query
      })
      stmt.executeUpdate()
      1
    } catch {
      case e => {
        throw e
      }
    } finally {
      connection.close()
    }
  }


  def insertDb(columns: List[String], table: String, data: List[String]): Int = {
    try {
      connection = createConnection(connStr, user, password, driver)
      // create the statement, and run the update query
      val query = s"insert into $table (${columns.mkString(", ")}) values ( ${data.mkString(", ")} )"
      val stmt = connection.prepareStatement({
        query
      })
      stmt.executeUpdate()
      1
    } catch {
      case e => {
        throw e
      }
    } finally {
      connection.close()
    }
  }
}



