//package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] 
{
  def map[B](f: A => B): Option[B] = this match { //리스트가 아님. 값 한 개만 들가기 때문에 맵함수를 한번만 적용하면 됨
      case None => None
      case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match { //껍데기 까거나 아무것도 안들어있으면 else 값
      case None => default
      case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = { 
    map(f) getOrElse None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match { //찌부 시키는 함수. 랩핑을 까고 안에 있는 값 반환
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = // 랩핑을 안 깐 값을 반환
    this map (Some(_)) getOrElse ob

  def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A=> Boolean): Option[A] = this match { 
    case Some(a) if (f(a)) => this
    case _ => None
  }

  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}


object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(Seq.flatMap(x => math.pow(x-mean(xs), 2))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}