


def HangeulToInteger(han: String): Double = {

    val num = Map('일' -> 1, '이' -> 2, '삼' -> 3, '사' -> 4, '오' -> 5, '육' -> 6,'칠' -> 7, '팔' -> 8, '구' -> 9)
val unit1 = Map('십' -> 10, '백' -> math.pow(10,2), '천' -> math.pow(10,3))
val unit2 = Map('만' -> math.pow(10,4), '억'-> math.pow(10,8), '조' -> math.pow(10,12))

    def ZeroToOne(a: Int):Int = if (a == 0) 1 else a

    def h(s: String, t1: Int, t2: Int, sum: Int): Double = {
        if (num.contains(s.head)) h(s.tail, num.get(s.head).toDouble, t2, sum)
        else if (unit1.contains(s.head)) h(s.tail, t1, ZeroToOne(t1)*unit1.get(s.head).toDouble, sum)
        else if (unit2.contains(s.head)) h(s.tail, 0, 0, ZeroToOne(t1+t2)*unit2.get(s.head).toDouble )
        else sum+t1+t2
    }

    h(han, 0, 0, 0)
}