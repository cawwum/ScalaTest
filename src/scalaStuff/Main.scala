package scalaStuff

object Main
{
    val n1 = List(1,2)
    val n2 = List(5)

    def main(args:Array[String]):Unit =
    {
        val ppScala = new PracticeProblemsScala()

        ppScala.streamMultiply(ppScala.oness(),ppScala.twoss(),1,0) take 20 foreach println
        //ppScala.streamMultiply2(ppScala.oness(),ppScala.twoss(),0) take 20 foreach println
    }
}
