val sum: (Int => Boolean) => (List[Int] => Int) =
p => {
case Nil
=> 0
case x :: xs if (p(x)) => x + sum(p)(xs)
case x :: xs
=> sum(p)(xs)
}
println(sum)
