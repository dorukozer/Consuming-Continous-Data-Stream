object Demo extends App {
  val Datasource = new Thread(new DBProducer( 5000,500))
  Datasource.start()
  val modelWeights = List(0.5,0.6,0.7)
  val classLables  = List("A","B")
  val mlModel = new MlModel( modelWeights, classLables)
  val DBConsumer = new DBConsumer(mlModel)
  DBConsumer.recursiveConsume(1000)
