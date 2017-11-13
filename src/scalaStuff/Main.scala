package scalaStuff

object Main
{
    val n1 = List(1,2)
    val n2 = List(5)

    def main(args:Array[String]):Unit =
    {
        val ppScala = new PracticeProblemsScala()

        //ppScala.listAddNoReverse(n1,n2,0) take 20 foreach println
        //ppScala.ones() take 25 foreach println
        //ppScala.digits(5) take 25 foreach println
        //ppScala.streamAdd(ppScala.ones(),ppScala.digits(1),0) take 10 foreach println

        ppScala.streamSub(ppScala.ones(),ppScala.one) take 20 foreach println
    }
}
