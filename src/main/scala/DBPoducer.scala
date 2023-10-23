import scala.io.Source

class DBProducer(sleepTime: Int, counterTreshold: Int) extends Thread {
  val DBConnection = new DBConnection("jdbc:postgresql://localhost:5432/project_demodb","doruk","doruk","org.postgresql.Driver")

  def coulmnsToList(line: String): List[String] = {
    val cols = line.split(",").map(_.trim).toList match {
      case id::given_label:: modelResults => id:: s"\'$given_label\'":: modelResults
    }
    cols
  }


  override def run() {
    var counter = 0
    val bufferedSource = Source.fromFile("C:\\Users\\user\\Desktop\\StajProj\\untitled\\tazi-se-interview-project-data.csv")
    val schema = bufferedSource.getLines().next().split(",").map(_.trim).toList
    for (line <- bufferedSource.getLines) {
      val cols = coulmnsToList(line: String)
      counter = counter + 1
      DBConnection.insertDb( schema,"public.cont_data_source",cols)
      if (counter == counterTreshold) {
        counter = 0
        Thread.sleep(sleepTime)
      }
    }
  }
}

