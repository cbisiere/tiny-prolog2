"Tower of Hanoi"

move(1,x,y,_) -> 
	outm("Move top disk from ")
	out(x) 
	outm(" to ") 
	outl(y) !;

move(n,x,y,z) ->
	inc(m,n)
	move(m,x,z,y)
	move(1,x,y,_)
	move(m,z,y,x);

inc(0,1) ->;
inc(1,2) ->;
inc(2,3) ->;
inc(3,4) ->;
inc(4,5) ->;
inc(5,6) ->;
inc(6,7) ->;
inc(7,8) ->;
inc(8,9) ->;

hanoi(n) ->
	move(n,source, target, auxiliary);

-> outml("*** Solving the Tower of Hanoi with 4 disks:") hanoi(4);
