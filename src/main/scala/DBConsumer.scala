import cats.implicits._

class DBConsumer(mlModel:MlModel) {
  val TableName = "public.myModelConf"
  val tableNameToCheck = "myModelConf"
  val DBConnection = new DBConnection("jdbc:postgresql://localhost:5432/project_demodb","doruk","doruk","org.postgresql.Driver")
  val MlModel = mlModel
  val columnNames = (mlModel.targetClass,mlModel.targetClass).mapN( (x,y) => (x,y))
  val columnNamesFlatten = columnNames.map(classLables => classLables match {
    case (l1,l2) => l1.concat(l2)
  })

  val createTableSchema = if (DBConnection.checkTableExist(tableNameToCheck) == false) {
    DBConnection.createTable(TableName, columnNamesFlatten)
  }

  var  indexConfusion =  DBConnection.selectCount(TableName,0 )



  def recursiveConsume(windowSize: Int): Any = {
    if (DBConnection.selectCount("public.cont_data_source", indexConfusion) >= 1000) {
      val rows = DBConnection.getRows(indexConfusion, windowSize, "public.cont_data_source")
      val windowResults = getActulAndPredictions(mlModel, rows, List.empty[List[Int]])
      val confusionTable = mlModel.createConfusionMatrix(windowResults)
      DBConnection.insertDb(columnNamesFlatten.map(a=>a.toString), TableName, confusionTable.map(a => a.toString))
      indexConfusion = indexConfusion + 1
      recursiveConsume(windowSize)
    } else {
      recursiveConsume(windowSize)
    }
  }

  def getActulAndPredictions(mlModel:MlModel, results: List[List[String]], resultList:List[List[Int]] ): List[List[Int]]  ={
    if(results.length != 0) {
      val actPredList =  results match {
        case head :: rest => {
          head match {
            case id :: actual_label :: resulOfModels => {
              val rL = resultList match {
                case ls => List(mlModel.parseTargetClass(actual_label), mlModel.predict(resulOfModels.map(a => a.toDouble))) :: ls
                case _ => List(List(mlModel.parseTargetClass(actual_label), mlModel.predict(resulOfModels.map(a => a.toDouble))))
              }
              getActulAndPredictions(mlModel, rest, rL)
            }
          }
        }
      }
      actPredList
    }
    else {
      resultList
    }
  }

}

