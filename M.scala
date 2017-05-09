
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc1: Int, acc2: Int): Int = 
    if (n<=1) acc1
    else go(n-1, acc2, acc1+acc2)

  go(n, 0, 1)
}

def findFirst[A](as: Array[A], p: A => Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int = 
  if (n >= as.length) -1
  else if (p(as(n))) n
  else loop(n+1)

  loop(0)
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean =
  if (n >= as.length) true
else if (ordered(as(n-1), as(n))) loop(n+1)
else false

if(as.length > 1)
  loop(1)
else 
  true
}


@annotation.tailrec
def s[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  
  if (as.length <= 1) true
  else if (!ordered(as.head, as.tail.head)) false
  else s(as.tail, ordered)
}



def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
  (a: A) => (b: B) => f(a,b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
(a: A) => f(g(a))

package fpinscala.datastructures

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
