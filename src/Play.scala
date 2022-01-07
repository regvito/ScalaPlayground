import Play.MyList.sum

object Play {

  def fact(x : Int) : Int = {
    def go(y: Int, acc:Int) : Int = {
      if (y <= 0) acc
      else go(y-1, y*acc)
    }
    go(x, 1)
  }
   def sq(x : Int) : Int = x * x

   def formatResult(name : String, x : Int, f: Int => Int) : String = {
       val msg = "The %s of %d is %d."
       msg.format(name, x, f(x))
     }

    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      def loop(x : Int) : Boolean = {
        if (x + 1 >= as.length ) true
        else if (ordered(as(x), as(x+1))) loop(x+1)
        else false
      }
      loop(0)
    }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {(a: A) => {(b:B) => f(a,b)} }
  def add(x:Int, y:Int) : Int = x+y
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {(a:A, b:B) => f(a)(b)}
  def compose[A,B,C](f: B => C, g: A => B): A => C = {(a:A) => f(g(a))}

  sealed trait MyList[+A]
  case object Nil extends MyList[Nothing]
  case class Cons[+A](head : A, tail : MyList[A]) extends MyList[A]

  object MyList {
    def sum(ints:MyList[Int]) : Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }
    // allows for MyList(x,y,z) since that is syntactic sugar for MyList.apply(x,y,z)
    def apply[A](as: A*): MyList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l:MyList[A]) : MyList[A] = l match {
      case Nil => Nil
      case Cons(x,xs) => xs
    }
    def setHead[A](a:A,l:MyList[A]) : MyList[A] = l match {
      case Nil => Nil
      case Cons(x,xs) => Cons(a,xs)
    }
    def drop[A](l: MyList[A], n: Int): MyList[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(x,xs) => drop(xs,n-1)
      }
    }
    def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
      case Nil => Nil
      case Cons(x,xs) => if (f(x)) dropWhile(xs,f) else Cons(x,dropWhile(xs,f))
      // these 2 below works only for matches in the beginning of list, not all of list
//      case Cons(h,t) if f(h) => dropWhile(t, f)
//      case _ => l
    }

    /* trace of reverse using foldLeft
      foldLeft(List(1,2,3),                                Nil) (_ + _)
      foldLeft(Cons(1, Cons(2, Cons(3, Nil))),             Nil) (_ + _)
      foldLeft(Cons(2, Cons(3, Nil)),              Cons(1,Nil)) (_ + _)
      foldLeft(Cons(3, Nil),               Cons(2,Cons(1,Nil))) (_ + _)
      foldLeft(Nil,                Cons(3,Cons(2,Cons(1,Nil)))) (_ + _)
      Cons(3,Cons(2,Cons(1,Nil)))
    */
    def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, MyList[A]())((acc,h) => Cons(h,acc))

    // foldRight: process each element starting with last element first
    // List(1, 3, 8).foldRight(100)(_ - _) == 1 - (3 - (8 - 100)) == -94
    def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
    def foldRightTail[A,B](as: MyList[A], z: B)(f: (A, B) => B): B =  {
      foldLeft(MyList.reverse(as), z)((b,a) => f(a,b))
    }
    // foldLeft: process each element starting with 1st element
    // List(1, 3, 8).foldLeft(100)(_ - _) == ((100 - 1) - 3) - 8 == 88
    def foldLeft[A,B](l: MyList[A], acc: B)(f: (B, A) => B): B = l match {
      case Nil => acc
      case Cons(h,t) => foldLeft(t, f(acc,h))(f)
    }

    def append[A](l : MyList[A], z : MyList[A]) : MyList[A] = foldLeft(MyList.reverse(l), z)((acc,h) => Cons(h,acc))
    def appendWithRight[A](l : MyList[A], z : MyList[A]) : MyList[A] = foldRight(l, z)( (a,b) => Cons(a,b))

    // or def concat(..)
    def flatten[A](l : MyList[MyList[A]]) : MyList[A] = {
      foldRight(l, Nil:MyList[A])(append)
    }

    def map[A,B](l: MyList[A])(f: A => B): MyList[B] =
      foldRight(l, Nil:MyList[B]) ((h,t) => Cons(f(h),t))

    def flatMap[A,B](l: MyList[A])(f: A => MyList[B]): MyList[B] =
      flatten(map(l)(f))


  } // end MyList object

      def main (args : Array[String]) : Unit = {
        println(formatResult("square", 3, sq))
        println(formatResult("factorial", 3, fact))
        val myarray = Array(1,2,3,4,5)
        val myarray2 = Array(1,2,4,3,5)
        val myarray3 = Array(1)
        val myarray4 = Array(1,2)
        val myarray5 = Array(11,2)
        val msg = "is sorted=%b"

        val compareInts = (x:Int,y:Int) => x <= y
        println("list " + myarray.mkString(", ") + " " + msg.format(isSorted(myarray, compareInts)))
        println("list " + myarray2.mkString(", ") + " " + msg.format(isSorted(myarray2, compareInts)))
        println("list " + myarray3.mkString(", ") + " " + msg.format(isSorted(myarray3, compareInts)))
        println("list " + myarray4.mkString(", ") + " " + msg.format(isSorted(myarray4, compareInts)))
        println("list " + myarray5.mkString(", ") + " " + msg.format(isSorted(myarray5, compareInts)))

        val curriedAdd = curry(add)
        println("add 3 + 7=" + curriedAdd(3)(7))
        val uncurriedAdd = uncurry(curriedAdd)
        println("add 3 + 7=" + uncurriedAdd(3,7))

        val composedSqFact = compose(fact, sq)
        println("2*2!=" + composedSqFact(2))

        val x = MyList(1,2,3,4,5) match {
          case Cons(x, Cons(2, Cons(4, _))) => x
          case Nil => 42
          case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
          case Cons(h, t) => h + sum(t)
          case _ => 101
        }
        println("matches " + x)
        val y = MyList(1,1,2,3,4,5)
        println("sum=" + MyList.sum(y))
        println("tail=" + MyList.tail(y))
        println("setHead=" + MyList.setHead(0,y))
        println("drop 2=" + MyList.drop(y, 2))
        println("drop while 2=" + MyList.dropWhile(y, (a:Int) => a == 1))

        val sameList = MyList(1, 3, 8)
        val answerLeft = MyList.foldLeft[Int,Int](sameList, 100)((a, b)=>a-b)
        val same =  answerLeft == 88 && ((100 - 1) - 3) - 8 == 88
        println("same Left=" + same + " " + answerLeft)
        val answerRight = MyList.foldRight[Int,Int](sameList, 100)((a, b)=>a-b)
        val answerRightUsingLeft = MyList.foldRightTail[Int,Int](sameList, 100)((a, b)=>a-b)
        val sameRight =  answerRight == -94 && (1 - (3 - (8 - 100))) == -94
        println("same Right=" + sameRight + " " + answerRightUsingLeft + " " + answerRight)

        val left = MyList(1,2)
        val right = MyList(3,4,5)
        println("append: " + MyList.append(left, right))
        println("append with Right: " + MyList.appendWithRight(left, right))

        val listOfList = MyList(left,right)
        println("flatten " + listOfList + " to " + MyList.flatten(listOfList))

      }
// (100-8) = 92 - 3 = 89 - 1 = 88
}
