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

val reverse_list : (List[List[Int]], List[List[Int]]) => List[List[Int]] = {
    case (Nil, res) => res;
    case (x::xs, res) => reverse_list(xs, x::res);
}


val frames_rec: (List[Int], List[Int], List[Int], List[List[Int]], Int) => List[List[Int]] = {
    case(xs, x::cur, buf, frames, len) if (buf.length < len) => frames_rec(xs, cur, x :: buf, frames, len)
    case(x::xs, cur, buf, frames, len) if (buf.length >= len) => frames_rec(xs, xs, Nil, reverse(buf, Nil)::frames, len)
    case(x::xs, Nil, buf, frames, len) => frames_rec(xs, xs, Nil, frames, len)
	case(Nil,Nil,buf,frames,len) => frames
}

val frame: Int => (List[Int] => List[List[Int]]) = {
    len => {
       case Nil => Nil
       case all => reverse_list(frames_rec(all, all, Nil, Nil, len),Nil)
    }
}

val ls = getList(10)
val len = 2
val sl = frame(len)
val f = sl(ls);
println(ls)
println()
println(f)
