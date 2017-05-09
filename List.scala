//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}

def product(ds: List[Double]): Double = ds match { case Nil => 1.0
case Cons(0.0, _) => 0.0
case Cons(x,xs) => x * product(xs)
}

def apply[A](as: A*): List[A] =
if (as.isEmpty) Nil
else Cons(as.head, apply(as.tail: _*))
}

def tail[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) => xs
}

def setHead[A](l: List[A], a: A): List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) => Cons(a, xs)
}

def head[A](l: List[A]): A = l match {
  //case Nil => throw new Exception("")
  case Cons(x, _) => x
}

def drop[A](l: List[A], n: Int): List[A] = {
  if (n <= 0) l
  else drop(tail(l), n-1)
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
  if (l == Nil || (!f(head(l))) ) l
  else dropWhile(tail(l), f)
}

def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
  case Cons(h, t) if f(h) => dropWhile(t)(f)
  case _ => as
}

def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
  case Nil => a2
  case Cons(h, t) => Cons(h, append(t, a2))
}

def init[A](l: List[A]): List[A] = {
  def drop[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, drop(t))
  }

  drop(l)
}

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}
// def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
//   @annotation.tailrec
//   def loop(z: B, as: List[A]): B = {
//     if (as == Nil) z
//     else loop(f(z, head(as)), tail(as))
// }
//   loop(z, as)
// }
  @annotation.tailrec
def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    if (as == Nil) z
    else foldLeft(tail(as), f(z, head(as)))(f)
}

def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  foldLeft(foldLeft(as, List[A]())((x, y) => Cons(y, x)), z)((x, y) => f(y, x))
}

def product3(ns: List[Double]) = 
  foldLeft(ns, 1.0)(_ * _)

def sum3(ns: List[Int]) = 
  foldLeft(ns, 0)(_ + _)

def length2[A](as: List[A]): Int =
  foldLeft(as, 0)((x,y) => x+1)

def sum2(ns: List[Int]) =
  foldRight(ns, 0)((x,y) => x + y)

def product2(ns: List[Double]) =
  foldRight(ns, 1.0)(_ * _)

def length[A](as: List[A]): Int =
  foldRight(as, 0)((x,y) => y+1)


def addHead[A](as: List[A], a: A): List[A] = as match {
  case Nil => Cons(a, Nil)
  case Cons(x, xs) => Cons(a, Cons(x, xs))
}

def reverse[A](as: List[A]): List[A] = {
  @annotation.tailrec
  def loop[A](ns: List[A], as: List[A]): List[A] = {
    if (as == Nil) ns
    else loop(Cons(head(as), ns), tail(as))
  }

  loop(Nil, as)
}

def reverse2[A](as: List[A]): List[A] =
  foldLeft(as, List[A]())((x, y) => Cons(y, x))


def append2[A](a1: List[A], a2: List[A]): List[A] = 
  foldRight(a1, a2)(Cons(_, _)) 

def append3[A](a1: List[A], a2: List[A]): List[A] = 
  foldLeft(reverse2(a1), a2)((x, y) => Cons(y, x)) 

def flatten[A](as: List[List[A]]): List[A] =
  foldLeft(as, List[A]())(append3)

def flatten2[A](as: List[List[A]]): List[A] =
  foldLeft(as, List[A]())(append3)

def addOne(as: List[Int]): List[Int] =
  foldRight2(as, List[Int]())((x,y) => Cons(x+1, y) )


def ToString(as: List[Double]): List[String] = 
  foldRight(as, List[String]())((x,y) => Cons(x.toString, y))


def map[A,B](as: List[A])(f: A => B): List[B] = 
  foldRight(as, List[B]())((x,y) => Cons(f(x), y))


def filter[A](as: List[A])(f: A => Boolean): List[A] = 
  foldRight(as, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  flatten2(map(as)(f))

def addList(a1: List[Int], a2: List[Int]): List[Int] = {
  def loop(a1: List[Int], a2: List[Int], a3: List[Int]): List[Int] = {
    if (a1 == Nil || a2 == Nil) a3
    else loop(tail(a1), tail(a2), Cons(head(a1)+head(a2), a3))
  }
  reverse(loop(a1, a2, List[Int]()))
}


// def parellList[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = {
//   def loop[A, B, C](a: List[A], b: List[B], c: List[C])(f: (A, B) => C): List[C] = {
//     if (a1 == Nil && a2 == Nil) c
//     else loop(tail(a), tail(b), Cons(f(head(a), head(b)), c))(f)
//   }
//   reverse(loop(a1, a2, List[C]())(f))
// }

// def pyungList[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = {
//   def loop[A](a1: List[A], a2: List[A], a3: List[A])(f: (A, A) => A): List[A] = {
//     if (a1 == Nil && a2 == Nil) a3
//     else loop(tail(a1), tail(a2), Cons(f(head(a1), head(a2)), a3))
//   }
//   reverse(loop(a1, a2, List[A]())(f)) 
// }

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def inSequence[A](sup: List[A], sub: List[A]): Boolean = {
      if (sub == Nil) true
      else if (sup == Nil) false
      else if (head(sup) != head(sub)) false
      else inSequence(tail(sup), tail(sub))
  }
  
  if (sub != Nil && sup == Nil) false
  else if (sub == Nil) true
  else if (head(sup) == head(sub)) inSequence(tail(sup), tail(sub))
  else (hasSubsequence(tail(sup), sub))
}
