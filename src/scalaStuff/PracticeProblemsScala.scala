package scalaStuff

class PracticeProblemsScala(p:Int)
{
    def zeroes:Stream[Int] = Stream.cons(0,zeroes)
    def pminus1():Stream[Int] = Stream.cons(p-1,pminus1())

    def streamAdd(n1:Stream[Int],n2:Stream[Int],carry:Int):Stream[Int] = (n1,n2) match
    {
        case (Stream.Empty,Stream.Empty) => Stream.empty;
        case (Stream.Empty,_) => n2;
        case (_,Stream.Empty) => n1;
        case (h1#::t1,h2#::t2) =>
        {
            val total = carry + h1 + h2
            if (total < p) Stream.cons(total,streamAdd(t1,t2,0))
            else Stream.cons(total%p,streamAdd(t1,t2,total/p))
        }
    }

    //minuend & subtrahend
    def streamSub(min:Stream[Int],sub:Stream[Int]):Stream[Int] = (min,sub) match
    {
        case (Stream.Empty,Stream.Empty) => Stream.empty
        case (Stream.Empty,_) => streamSub(pminus1(),sub)
        case (_,Stream.Empty) => min
        case (mh#::mt,sh#::st) => if(mh >= sh) Stream.cons(mh-sh,streamSub(mt,st)) else Stream.cons(p+mh-sh,streamSub(borrow(mt),st))
    }

    def borrow(min:Stream[Int]):Stream[Int] = min match
    {
        case Stream.Empty => pminus1()
        case h#::t => if(h > 0) Stream.cons(h-1,t) else Stream.cons(p-1,borrow(t))
    }

    //call with n2 reversed up to count, and addacc as 0
    //returns (digit,carry)
    def lineMultiply(n1:Stream[Int],n2:Stream[Int],addacc:Int,carry:Int):(Int,Int) = (n1,n2) match
    {
        case (Stream.Empty,Stream.Empty) => ((addacc+carry)%p,(addacc+carry)/p)
        case (Stream.Empty,h2#::t2) => lineMultiply(Stream.empty,t2,addacc,carry)
        case (h1#::t1,Stream.Empty) => lineMultiply(t1,Stream.empty,addacc,carry)
        case (h1#::t1,h2#::t2) => lineMultiply(t1,t2,addacc+h1*h2,carry)
    }

    //call with count starting at 1
    def streamMultiply(n1:Stream[Int],n2:Stream[Int],count:Int,carry:Int):Stream[Int] =
    {
        lineMultiply(n1 take count,n2 take count reverse,0,carry) match
        {
            case (a,b) => Stream.cons(a,streamMultiply(n1,n2,count+1,b))
        }
    }




    //test streams
    def ones(): Stream[Int] = Stream.cons(1, ones())
    def twos():Stream[Int] = Stream.cons(1,Stream.cons(2,Stream.cons(3,Stream.cons(4,Stream.cons(5,zeroes)))))
    def oness():Stream[Int] = Stream.cons(1,Stream.cons(0,Stream.cons(1,Stream.cons(0,Stream.cons(1,zeroes)))))
    def twoss():Stream[Int] = Stream.cons(0,Stream.cons(0,Stream.cons(0,Stream.cons(1,Stream.cons(0,zeroes)))))
    val one:Stream[Int] = Stream.cons(1,Stream.empty)
    def digits(digit: Int): Stream[Int] =
    {
        val currentdigit: Int =
            if (digit < 10) digit
            else 0

        Stream.cons(currentdigit, digits(currentdigit + 1))
    }































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



    //shift thing doesn't work, as it wants to evaluate the full length of multiplymap

    def streamMultiply2(n1:Stream[Int],n2:Stream[Int],shift:Int):Stream[Int] = n1 match
    {
        case h#::t =>
            {
                def appendzeroes(n:Int,ns:Stream[Int]):Stream[Int] = n match
                {
                    case 0 => ns
                    case _ => Stream.cons(0,appendzeroes(n-1,ns))
                }

                streamAdd(appendzeroes(shift,multiplymap(h,n2,0)),streamMultiply2(t,n2,shift+1),0)
            }
    }

    def multiplymap(n:Int,ns:Stream[Int],carry:Int):Stream[Int] = ns match
    {
        case Stream.Empty => Stream.empty
        case h#::t => Stream.cons((carry+n*h)%10,multiplymap(n,t,(carry+n*h)/10))
    }


}

