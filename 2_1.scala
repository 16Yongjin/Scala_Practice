object M {
    def abs(n: Int): Int = 
        if (n < 0) -n
        else n

    private def f(x: Int) = {
        val msg = "Abs value of %d is %d"
        msg.format(x, abs(x))
    }

    def main(args: Array[String]): Unit = 
        println(f(-1040))
}