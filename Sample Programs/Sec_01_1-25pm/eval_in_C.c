#include <stdio.h>
#include <stdlib.h>

/*
type expr = 
  | Add of expr * expr
  | Mul of expr * expr
  | Cnst of int
  | Neg of expr

let e1 = Add (Cnst 4, Mul( Cnst 2, Cnst 3))
let e2 = Mul (Cnst 4, Add( Cnst 2, Neg (Cnst 3)))

let rec eval e = 
  match e with
  | Add (l,r) -> eval l + eval r
  | Mul (l,r) -> eval l * eval r
  | Cnst v -> v
  | Neg ne -> 0 - (eval ne)
*/


/* A tag for each value consturctor in the disjoint union. */
enum tag {add, mul, cnst, neg} ;

struct expr ;

/* A struct for each non-trivial product type used by a value constructor. */
struct bin_op_struct {
    struct expr *l ;
    struct expr *r ;
} ;


/* A union holding all components, one for each constructor. */
union all_components { 
    struct bin_op_struct add_components ;
    struct bin_op_struct mul_components ;
    int v ;
    struct expr *ne ;
} ; 

/* A struct for the disjoint union type containing the tag and
   union of all constructor values. */
struct expr {
    enum tag expr_tag ;
    union all_components components ;
};

int eval (struct expr *e) {
    switch (e->expr_tag) {
    case add: {
        return ( eval (e->components.add_components.l) +
                 eval (e->components.add_components.r) ) ;
    }
    case mul: {
        return ( eval (e->components.mul_components.l) *
                 eval (e->components.mul_components.r) ) ;
    }
    case cnst: {
        return ( e->components.v ) ;
    }
    case neg: {
        return ( 0 - eval (e->components.ne) ) ;
    }
    }

}

int main () {

    /* defining the expression e1.  */

    struct expr *cnst_4 = malloc (sizeof (struct expr));
    cnst_4->expr_tag = cnst ;
    cnst_4->components.v = 4 ;

    struct expr *cnst_2 = malloc (sizeof (struct expr));
    cnst_2->expr_tag = cnst ;
    cnst_2->components.v = 2 ;

    struct expr *cnst_3 = malloc (sizeof (struct expr));
    cnst_3->expr_tag = cnst ;
    cnst_3->components.v = 3 ;

    struct expr *mul_expr = malloc (sizeof (struct expr));
    mul_expr->expr_tag = mul ;
    mul_expr->components.mul_components.l = cnst_2 ;
    mul_expr->components.mul_components.r = cnst_3 ;

    struct expr *add_expr = malloc (sizeof (struct expr));
    add_expr->expr_tag = add ;
    add_expr->components.add_components.l = cnst_4 ;
    add_expr->components.add_components.r = mul_expr ;


    /* evaluation of e1 */
    printf ("Result is %d.\n", eval (add_expr) ) ;


    /* Introduce a few errors and see what happens.
       - use l instead r somewhere
       - use the wrong tag.
    */
}
