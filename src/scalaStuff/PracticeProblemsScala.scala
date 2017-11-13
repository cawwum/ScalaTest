package scalaStuff

class PracticeProblemsScala
{
    def last(list: List[Any]): Any = list match
    {
        case Nil => Nil
        case hd :: Nil => hd
        case hd :: tl => last(tl)
    }

    def penultimate(list: List[Any]): Any = list match
    {
        case Nil => Nil
        case hd :: Nil => Nil
        case hd :: tl => if (tl.length == 1) hd
        else penultimate(tl);
    }

    def nth(k: Int, list: List[Any]): Any = (k, list) match
    {
        case (_, Nil) => Nil
        case (0, hd :: _) => hd
        case (_, hd :: tl) => nth(k - 1, tl)
    }

    def length(list: List[Any]): Int =
    {
        def lengthHelper(list: List[Any], acc: Int): Int = list match
        {
            case Nil => acc
            case hd :: tl => lengthHelper(tl, acc + 1)
        }

        lengthHelper(list, 0)
    }

    /*
    def reverse(list:List[Any]):List[Any] =
    {
        def reverseHelper(list:List[Any],acc:List[Any]):List[Any] = list match
        {
            case Nil => acc
            case hd::tl => reverseHelper(tl,hd::acc)
        }

        reverseHelper(list,Nil)
    }

    def palindrome(list:List[Any]):Boolean =
    {
        val reverseList:List[Any] = reverse(list)
        list.equals(reverseList)
    }
*/ def badlength(list: List[Any]): Int = list match
    {
        case Nil => 0
        case hd :: tl => 1 + badlength(tl)
    }

    def flatten(list: List[Any]): List[Any] = list match
    {
        case Nil => Nil
        case (hd: List[_]) :: tl => flatten(hd) ::: flatten(tl)
        case (hd :: tl) => hd :: flatten(tl)
    }

    def streamRange(lo: Int, hi: Int): Stream[Int] =
    {
        if (lo >= hi) Stream.empty
        else Stream.cons(lo, streamRange(lo + 1, hi))
    }

    //padic integers
    //p - must be in a prime base ... switching between >10 bases and their representations ... ?
    //... maybe just start with 2,3,5,7 ....
    //simple base addition thing ....
    /*

    specify base, and how to count with that base ...

     */

    /*
    def listAdd(n1: List[Int], n2: List[Int]): List[Int] =
    {
        def addHelper(n1: List[Int], n2: List[Int], acc: List[Int], carry: Int): List[Int] = (n1, n2) match
        {
            case (Nil, Nil) => acc.reverse
            case (Nil, n2) => (n2.reverse ::: acc).reverse
            case (n1, Nil) => (n1.reverse ::: acc).reverse
            case (h1 :: t1, h2 :: t2) => if (carry + h1 + h2 < 10) addHelper(t1, t2, (carry + h1 + h2) :: acc, 0)
            else addHelper(t1, t2, ((carry + h1 + h2) % 10) :: acc, (carry + h1 + h2) / 10)
        }

        addHelper(n1, n2, Nil, 0)
    }

    def listAddNoReverse(n1: List[Int], n2: List[Int], carry: Int): List[Int] = (n1, n2) match
    {
        case (Nil, Nil) => Nil;
        case (Nil, n2) => n2;
        case (n1, Nil) => n1;
        case (h1 :: t1, h2 :: t2) =>
        {
            val total = carry + h1 + h2;
            if (total < 10) (total) :: listAddNoReverse(t1, t2, 0)
            else ((total) % 10) :: listAddNoReverse(t1, t2, total / 10)
        }
    }

    class p with simple addition/subtraction
    class padic Stream[p]


     */

    def streamAdd(n1:Stream[Int],n2:Stream[Int],carry:Int):Stream[Int] = (n1,n2) match
    {
        case (Stream.Empty,Stream.Empty) => Stream.empty;
        case (Stream.Empty,_) => n2;
        case (_,Stream.Empty) => n1;
        case (h1#::t1,h2#::t2) =>
        {
            val total = carry + h1 + h2
            if (total < 10) Stream.cons(total,streamAdd(t1,t2,0))
            else Stream.cons(total%10,streamAdd(t1,t2,total/10))
        }
    }

    def nines():Stream[Int] = Stream.cons(9,nines())

    //minuend & subtrahend
    def streamSub(min:Stream[Int],sub:Stream[Int]):Stream[Int] = (min,sub) match
    {
        case (Stream.Empty,Stream.Empty) => Stream.empty
        case (Stream.Empty,_) => streamSub(nines(),sub)
        case (_,Stream.Empty) => min
        case (mh#::mt,sh#::st) => if(mh >= sh) Stream.cons(mh-sh,streamSub(mt,st)) else Stream.cons(10+mh-sh,streamSub(borrow(mt),st))
    }

    def borrow(min:Stream[Int]):Stream[Int] = min match
    {
        case Stream.Empty => nines()
        case h#::t => if(h > 0) Stream.cons(h-1,t) else Stream.cons(9,borrow(t))
    }

    def ones(): Stream[Int] =
    {
        Stream.cons(0, ones())
    }

    val one:Stream[Int] = Stream.cons(1,Stream.empty)

    def digits(digit: Int): Stream[Int] =
    {
        val currentdigit: Int =
            if (digit < 10) digit
            else 0

        Stream.cons(currentdigit, digits(currentdigit + 1))
    }
}

