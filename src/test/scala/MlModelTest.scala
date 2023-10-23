import org.scalatest.Args
import org.scalatest.funsuite.AnyFunSuite

import scala.Float


class MlModelTest extends AnyFunSuite {

  test("3 class  3 model") {// 0.th class
    val weights = List(0.5,0.6,0.7)
    val mlModel = new MlModel( weights,List("A","B","C"))
    val model1_A = (0.5)
    val model2_A = (0.5)
    val model3_A = (0.5)
    val model1_B = (0.4)
    val model2_B = (0.3)
    val model3_B = (0.2)
    val model1_C = (0.1)
    val model2_C = (0.2)
    val model3_C = (0.3)
    val modelRe = List(model1_A,model1_B,model1_C,model2_A,model2_B,model2_C,model3_A,model3_B,model3_C)
    val result = mlModel.predict(modelRe)
    assert(result==0)
  }
  test("3 model 2 class") {// 0.th class
    val weights = List(0.5,0.6,0.7)
    val mlModel = new MlModel( weights, List("A","B"))
    val model1_A = (0.6)
    val model2_A = (0.5)
    val model3_A = (0.7)
    val model1_B = (0.4)
    val model2_B = (0.5)
    val model3_B = (0.3)
    val modelRe = List(model1_A,model1_B,model2_A,model2_B,model3_A,model3_B)
    val result = mlModel.predict(modelRe)
    assert(result==0)
  }
  test("4 model 2 class") { // 0.th class
    val weights = List(0.5,0.6,0.7,0.9)
    val mlModel = new MlModel(weights,List("A","B"))
    val model1_A = (0.6)
    val model2_A = (0.5)
    val model3_A = (0.5)
    val model4_A = (0.7)
    val model1_B = (0.4)
    val model2_B = (0.5)
    val model3_B = (0.5)
    val model4_B = (0.3)
    val modelRe = List(model1_A,model1_B,model2_A,model2_B,model3_A,model3_B,model4_A,model4_B)
    val result = mlModel.predict(modelRe)
    assert(result==0)
  }
  test("4 model 2 class result 1 ") {
    val weights = List(0.5,0.6,0.7,0.9)
    val mlModel = new MlModel( weights,List("A","B"))
    val model1_A = (0.2)
    val model2_A = (0.3)
    val model3_A = (0.5)
    val model4_A = (0.3)
    val model1_B = (0.8)
    val model2_B = (0.7)
    val model3_B = (0.5)
    val model4_B = (0.7)
    val modelRe = List(model1_A,model1_B,model2_A,model2_B,model3_A,model3_B,model4_A,model4_B)
    val result = mlModel.predict(modelRe)
    assert(result==1)
  }

  test("createConfusion full of B"){
    val weights = List(0.5,0.6,0.7)
    val mlModel = new MlModel(  weights, List("A","B"))
    val confMat=  mlModel.createConfusionMatrix(List(List(1,1),List(1,1),List(1,1),List(1,1)))
    assert(confMat.equals(List(0,0,0,4)))
  }

  test("createConfusion full of A"){
    val weights = List(0.5,0.6,0.7)
    val mlModel = new MlModel(weights, List("A","B"))
    val confMat=  mlModel.createConfusionMatrix(List(List(0,0),List(0,0),List(0,0),List(0,0)))
    assert(confMat.equals(List(4,0,0,0)))
  }
  test("createConfusion random"){
    val weights = List(0.5,0.6,0.7,0.9)
    val mlModel = new MlModel( weights, List("A","B","C"))
    val confMat=  mlModel.createConfusionMatrix(List(List(1,2),List(1,1),List(0,2),List(1,1)))
    println(confMat)
  }

}

