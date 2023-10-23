import scala.annotation.tailrec


class MlModel(modelWeights:List[Double], targetClasses : List[String]) {
  val numberofClass = targetClasses.length
  val numbOfModels= modelWeights.length
  val modelsWeights = modelWeights
  val normalizeFactor = modelsWeights.sum
  val classLabels =(0 to numberofClass-1).toList
  val targetClass=targetClasses


  def parseTargetClass(className: String): Int ={
    targetClasses.indexOf(className)
  }

  def calculateClassPoint(modelN_A: List[Double] ):Double = {
    val model=  modelN_A.zip(modelsWeights).map {
      case (a , b) => (a * b )
    }
    model.sum/ normalizeFactor
  }


  def populateModelScores(modelResults: List[Double],classNumber: Int, lsDouble: List[Double] ): List[Double] = {
    if (modelResults.length != 0) {
      val toAppend  = modelResults(classNumber)
      populateModelScores(modelResults.drop(numberofClass), classNumber, toAppend :: lsDouble)
    } else {
      lsDouble
    }
  }

  def predict(modelResults: List[Double]): Int = {
    val l  = classLabels.map( classnum => populateModelScores(modelResults,classnum,List.empty[Double])
    )
    val a =  l.map(a => calculateClassPoint(a.reverse))
    a.indexOf(a.max)
  }

  def createConfusionMatrix(actualPredictResults: List[List[Int]]): List[Int] = {
    val row =List.fill(numberofClass)(0)
    val Confmatrix = List.fill(numberofClass)(row)
    createConfusionMatrixHelper(actualPredictResults,Confmatrix )
  }
  @tailrec
  private def createConfusionMatrixHelper(actualPredictResults: List[List[Int]],confmatrix: List[List[Int]]): List[Int] = {
    if (actualPredictResults.length == 0) {
      confmatrix.flatten
    } else {
      val toRet = actualPredictResults match {
        case List(actual, predicted) :: tail => {
          val aa = confmatrix(actual).updated(predicted,confmatrix(actual)(predicted)+1   )
          val bb = confmatrix.updated(actual,aa)
          bb
        }
      }
      createConfusionMatrixHelper(actualPredictResults.tail, toRet)

    }
  }
}
