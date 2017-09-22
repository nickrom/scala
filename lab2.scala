class Polynom(lst_coef: List[Int]) {
	val coef = lst_coef
	def this() = this(Nil)
	def ! (r : Roots) = createPol(r.roots, Nil)
}

val createPol: (List[Int], List[Int]) {
	case(Nil, root :: roots) => createPol(1 :: root, roots)
	case(coef, root :: roots) => createPol(mul(Nil, coef,root, Nil) , roots)
	case(coef, Nil) => new Polynom(coef)
}

val mul: (List[Int], List[Int], Int, Int) {
	case(Nil,c :: coef,root, Nil) => mul(c, coef, root, c)
}
class Roots(lst_roots: List[Int]) {
	val roots = lst_roots
	def this() = this(Nil)
	def + (r : Roots) = new Roots(roots ::: r.roots)
	def * (r : Roots) = new Roots(roots.intersect(r.roots))
	def createPol() = {
		val len = roots.length
		createCoef(1 :: Nil, len, roots, len)
	}
}

val createCoef: (List[Int], Int, List[Int], Int) => List[Int] = {
	case (coef, len, roots, L) => createCoef(coef :: find_coef(roots,L-len))
	case (coef, Nil, roots, L) => coef	
}

def find_coef(roots: List[Int], iter: Int) {
	
}

val p1 = new Polynom()
println(p1.coef)
