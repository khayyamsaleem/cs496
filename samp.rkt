(run "
begin
type treeInt = 
| Nil()
| Node(int,treeInt,treeInt);
let isEmpty=
  proc(t:treeInt)
    case t of {
        <Nil>() -> 0,
        <Node> -> <Nil>()
        alkdjfal;kdfjal;kdf
        asldf;kjadfl;
    }
  ")
(run "
module m1
  interface
   [u : int]
  body
   [u = 44]
module m2
  interface
   [v : int]
  body
   [v = -(from m1 take u,11)]
open m1
in 
open m2
in
let v=150
in
-(u,v)")
