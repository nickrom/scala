val getList: Int => List[Int] = 
	len => 
		if (len == 0) 
			Nil
		else
			len :: getList(len-1)

val reverse : (List[Int], List[Int]) => List[Int] = {
    case (Nil, res) => res;
    case (x::xs, res) => reverse(xs, x::res);
}

val split_rev: (List[Int], List[List[Int]]) => List[List[Int]] = {
	case (Nil, list) => list
	case (x::xs,list) => split_rev(xs, reverse(xs,Nil) :: list)
}

val split: (List[Int], List[List[Int]]) => List[List[Int]] = {
	case (Nil, list) => list
	case (x::xs,list) => split(xs, xs::list)
}


val frames: Int => (List[Int] => List[List[Int]]) = 
	len => {
		case Nil => Nil
		case list => list :: split(list, Nil) ::: split_rev(reverse(list, Nil),Nil)
	}
		
val len = 10
val list = getList(len)
val fr = frames(10)
val f = fr(list)
println(list)
println()
println(f)
