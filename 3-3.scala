var a =  Array(1,2,3,4,5)

for (i <- 0 until a.length) yield if (a(i) != a.length-1) {
  if(a(i+1)-a(i) == 1) {
    var t = a(i+1)
    a(i+1) = a(i)
    t
  } else {
    a(i)
  }
} else (a(i))


a
