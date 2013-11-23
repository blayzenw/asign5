(*  Original version by Geoffrey Smith - http://users.cs.fiu.edu/~smithg/  *)

use "type.sml";

(* A modified term datatype to allow us to type our function arguments *)
type identifier = string;
datatype TE =
    Variable of identifier
  | Literal of int 
  | Plus of TE * TE 
  | Lambda of identifier * TE
  | Lett of identifier * TE * TE
  | App of TE * TE 
  | Rec of identifier * TE
  | Bool of bool 
  | IsZero of TE
  | IfThenElse of TE * TE * TE ;

exception Unimplemented
exception TypeError

(*  A substitution is just a function typ -> typ.  *)

fun identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

fun replace (a, t) (VAR b) =
      if a = b then t else VAR b
|   replace (a, t) (ARROW (t1, t2)) =
      ARROW (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    replace (a, t) t1 = t1

(*  occurs : string * typ -> bool  *)

fun occurs (a, VAR b)         = (a = b)
|   occurs (a, ARROW(t1, t2)) = occurs (a, t1) orelse occurs (a, t2)
|   occurs (a, _)             = false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)

fun unify (VAR a, t) =
      if VAR a = t then identity
      else if occurs (a, t) then raise Circularity
      else replace (a, t)
|   unify (t, VAR a)   = unify (VAR a, t)
|   unify (INT, INT)   = identity
|   unify (BOOL, BOOL) = identity
|   unify (ARROW(t1, t2), ARROW(t3, t4)) =
      let val s1 = unify (t1, t3)
	  val s2 = unify (s1 t2, s1 t4)
      in
	  s2 o s1
      end
|   unify (_, _) = raise Mismatch


(*  New type variable generator:  newtypevar () and reset ()  *)

local
  val letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
		 "n","o","p","q","r","s","t","u","v","w","x","y","z"]
  val cnt = ref 0
  val typevars = ref letters
in
  fun newtypevar () = (
    if null (! typevars) then (
      cnt := ! cnt + 1;
      typevars := map (fn s => s ^ Int.toString (! cnt)) letters
    ) else ();
    VAR (hd (! typevars)) before (typevars := tl (! typevars))
  )

  fun reset () = (
    cnt := 0;
    typevars := letters
  )
end

(*  Milner's algorithm W : (string -> typ) * TE -> (typ -> typ) * typ
 * 
 *  I have included the code for Literal and IfThenElse; you must complete
 *  the other cases!
 *)

fun W (E, Literal n)     = (identity, INT)
|   W (E, Variable x)    = (identity, E x)
|   W (E, Bool x)        = (identity, BOOL)
|   W (E, Plus(x,y))     =
	let
		val (s1, t1) = W(E, x)
		val s2 = unify(t1, INT)
		val (s3, t2) = W(s2 o s1 o E, y)
		val s4 = unify(s3 t2, INT)
	in
		(s4 o s3 o s2 o s1, ARROW(INT, INT))
	end
(*		
|   W (E, Lambda(i,t,e)) =
	let
		val newenv = update(E, id, newtypevar())
		val (s, t) = W(E, newenv, e)
	in
		(s, s ARROW(newenv,t))
|   W (E, Lett(l,e1,e2)) =
*)

|   W (E, App(e1,e2))    =
	let
		val (s1, t1) = W(E, e1)
		val (s2, t2) = W(s1 o E, e2)
		val s3 = unify(s2 t1, t2)
	in
		(s3 o s2 o s1, s3 t2)
(*
|   W (E, Rec(i,t,e))    =
*)
|   W (E, IsZero(x))     =
	let
		val (s1, t1) = W(E, x)
		val s2 = unify(t1, INT)
	in
		(s2 o s1, ARROW(INT, BOOL))
	end
	
|   W (E, IfThenElse (e1, e2, e3)) =
    let val (s1, t1) = W (E, e1)
		val s2 = unify(t1, BOOL)
		val (s3, t2) = W (s2 o s1 o E, e2)
		val (s4, t3) = W (s3 o s2 o s1 o E, e3)
		val s5 = unify(s4 t2, t3)
    in
		(s5 o s4 o s3 o s2 o s1, s5 t3)
    end

(*  Here's a driver program that uses W to find the principal type of e
 *  and then displays it nicely, using typ2str.
 *)

fun infer e =
  let val (s, t) = (reset (); W (emptyenv, e))
  in
    print ("The principal type is\n  " ^ typ2str t ^ "\n")
  end

val test1 = Lambda("f",
                Lambda("x",
                    App(Variable "f",
                        App(Variable "f", Variable "x"))));

(* fn f =&gt; fn g =&gt; fn x =&gt; f (g x) *)
val test2 = Lambda("f",
                Lambda("g",
                    Lambda("x",
                        App(Variable "f",
                            App(Variable "g", Variable "x")))));

(* fn b =&gt; if b then 1 else 0 *)
val test3 = Lambda("b",
                IfThenElse(Variable "b", Literal 1, Literal 0));
