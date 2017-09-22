class Roots(lst_roots: List[Int]) {
	val roots = lst_roots.toSet
	def this() = this(Nil)
	def + (r : Roots) = new Roots((roots | r.roots).toList)
	def * (r : Roots) = new Roots((roots & r.roots).toList)
	def unary_! () = createPol(roots.toList,Nil,Nil)
	def createPol(xs : List[Int], old_coef : List[Int], cur_coef : List[Int]): Polynom = {
		(xs,old_coef,cur_coef) match {
			case(x :: xs, Nil, Nil) => createPol(xs, List(1, -x), Nil)
			case(x :: xs, Nil, cur_coef) => createPol(xs, reverse(cur_coef,Nil),Nil)
			case(x :: xs, a :: old_coef, Nil) => createPol(x :: xs,  a :: old_coef, List(a))
			case(x :: xs, a1 :: a2 :: old_coef, cur_coef) => createPol(x :: xs, a2 :: old_coef, a2-a1*x :: cur_coef)
			case(x :: xs, an :: old_coef, cur_coef) => createPol(x :: xs, old_coef, -an*x :: cur_coef)
			case(Nil, old_coef, cur_coef) => new Polynom(old_coef)
		}
	}
}

class Polynom(lst_coef: List[Int]) {
	val coef = lst_coef
	def this() = this(Nil)
	def * (r : Roots) = findRoots(coef, r.roots.toList,Nil)
	def findRoots(coef : List[Int], roots : List[Int], res : List[Int]): Roots = {
		(coef,roots,res) match {
			case(coef, x :: xs, r) => findRoots(coef, xs, isRoot(coef, x, coef.length, 0) ::: r)
			case(coef, Nil, r) => new Roots(r)
		}
	}
	def isRoot(coef: List[Int], x: Int, l:Int, sum:Double): List[Int] = {
		(coef,x,l,sum) match {
			case(c :: coef, x, l, sum) => isRoot(coef, x, l-1, sum + c*Math.pow(x,l-1))
			case(coef, x, 0, sum) if (sum == 0) => List(x)
			case(coef, x, 0, sum) if (sum != 0) => List()
		}
	}
}


def reverse : (List[Int], List[Int]) => List[Int] = {
    case (Nil, res) => res;
    case (x::xs, res) => reverse(xs, x::res);
}

val r1 = new Roots(List(1,2,3))
val r2 = new Roots(List(2,3,4))
println("We have two sets of roots: ")
println(r1.roots)
println(r2.roots)

println("Union and intersect")
println((r1+r2).roots)
println((r1*r2).roots)

println("Polynom build on roots r1")
println((r1 !) .coef)

println("Roots from r1 which are roots in polynom build on r2")
println(((r2 !) * r1).roots)
