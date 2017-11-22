package scalaStuff

object Main
{
    def main(args:Array[String]):Unit =
    {
        println("RADIX (in decimal)")
        val radixInput:Int = scala.io.StdIn.readInt()
        val ppScala = new PracticeProblemsScala(radixInput)

        //find out if split causes errors?
        def toIntStream(nlist:List[String]):Stream[Int] = nlist match
        {
            case Nil => ppScala.zeroes
            case h::t => Stream.cons(Integer.parseInt(h,radixInput),toIntStream(t))
        }

        println("1ST INTEGER")
        val n1Input:List[String] = scala.io.StdIn.readLine() sliding 1 toList
        val n1 = toIntStream(n1Input.reverse)

        println("2ND INTEGER")
        val n2Input:List[String] = scala.io.StdIn.readLine() sliding 1 toList
        val n2 = toIntStream(n2Input.reverse)

        println("OPERATION (+,-,*)")
        val op:String = scala.io.StdIn.readLine()

        op match
        {
            case "+" => println(streamToString(ppScala.streamAdd(n1,n2,0) take 10))
            case "-" => println(streamToString(ppScala.streamSub(n1,n2) take 10))
            case "*" => println(streamToString(ppScala.streamMultiply(n1,n2,1,0) take 10))
            case _ => println("NOT A VALID OPERATION!")
        }

        def streamToString(ns:Stream[Int]):String = ns match
        {
            case Stream.Empty => ""
            case h#::t => streamToString(t)+h
        }
        //ppScala.streamMultiply(ppScala.oness(),ppScala.twoss(),1,0) take 20 foreach println
        //ppScala.streamMultiply2(ppScala.oness(),ppScala.twoss(),0) take 20 foreach println
    }
}
