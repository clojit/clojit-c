Clojure Bytecode Spec
======================

    B	C	A	OP
    D	    A	OP

## Tables

- Global Symbol Table
- Constant Number Table
- Constant String Table
- Constant Keyword Table

## Constant Table Read Ops

    OP      A       D

    CSTR    dst     str
    CNUM    dst     num
    CKEY    dst     keyword

Reads D from const. table and writes it into destination slot A.

## Global Table Ops

    OP       A       D
    
    NSGETV   -      var
    NSGETS   -      str
    
    NSSETV   var    var
    NSSETS   var    str
    
Gets a symbol D from the namespace environment, or sets the symbol D to the value of variable A.

## Variable Slots

All variables are stored in a function local variable slot. TODO: Figure out how
this is supposed to work for function calls.

ISSUE: In the current design, the variables are accessed using a 8-bit index. This
limits the number of local variables to 256. This is not a problem for Lua, as it
allows no more than 200 local variables, but might be a problem for a general
programming language such as Clojure.

## Binary Ops

    OP      A   B   C
    ADDVV	dst	var	var	    A = B + C
    SUBVV	dst	var	var	    A = B - C
    MULVV	dst	var	var	    A = B * C
    DIVVV	dst	var	var	    A = B / C
    MODVV	dst	var	var	    A = B % C
    POW	    dst	var	var	    A = B ^ C
    
    ISLT	dst var	var	    A = B < C
    ISGE	dst var	var     A = B ≥ C
    ISLE	dst var var	    A = B ≤ C
    ISGT	dst var var	    A = B > C
    ISEQ	dst var var	    A = B == C
    ISNEQ	dst var var	    A = B ≠ C
    
Executes the operation on the numbers B and C. 
For the math ops: If one of the operands is a float, the result is of type float. If both operands are longs, the result is of type long.

For the comparision ops: If the operands are not of the same type, the other value is converted to float for comparison. Equals is only true if both operands have the same type.

## Unary Ops

    OP      A       D
    MOV	    dst	    var	    Copy D to A
    NOT	    dst	    var	    Set A to boolean not of D
    NEG	    dst	    var	    Set A to -D (negate)

## Jumps

    OP      A       D
    JUMP    -       addr
    JUMPF   var     addr
    JUMPT   var     addr
    
Jumps to the relative target address `addr` if the value in slot `var` is truthy or falsy. The `addr` is in amount of bytecode instructions.

## Function Calls

    OP      A       D
    CALL    base    lit
    RET     var     -

'lit' is the number of arguments.

base is a offset on the variable belt, so that the slot nr 'base' is a reference
to the function, 'base+1' is the first argument, 'base+lit' is the last
argument and so on.

The CALL instruction will set up the variable belt for the callee so that
all parameters are in the right place. This means that for the callee its
return address is in slot 0, the function object for itself in slot 1 and its
arguments in slot 2 and following slots.

The RET instruction will copy the value in `var` into the designated slot for
the caller. RET will read out the return address from slot 0, replace it with 
the contents of `var` and then jump to the return address.

    OP      A       B       C
    APPLY   var     var     -

Calls the function stored on variable slot A, and applies the elements from
vector B as argument for the function.


TODO: Figure out if local variables of the caller are allowed to be stored 
after 'base'.

## Closures and Free Variables

    OP      A       D
    FNEW    dst     jump
    
FNEW creates new closure for the function referenced by `jump` in the variable
slot `dst`.

    OP          A       B       C
    SETFREEVAR  dst     src     idx
    GETFREEVAR  dst     src     idx
    
SETFREEVAR sets the free variable `idx` of closure `dst` to the value of
the variable in `src`.
GETFREEVAR copies the value of the free variable `idx` from the closure `src`
into the variable in `dst`.

## Tail Recursion and Loops

    OP          A       B       C
    LOOP        -       -       -
    BULKMOV     dst     src     len
    
LOOP is a no-op to indicate the beginning of a loop. At the end of a loop,
a JUMP instruction is used to jump back to the beginning of the loop (i.e. where
the LOOP instruction is). BULKMOV copies `len` variables starting from `src` into
`dst`, `dst+1`, ..., `dst+len`. BULKMOV is used to reset the loop variables.

Tail recursion is implemented using the JUMP instruction, the function jumps
back to the start of the function. The compiler emits the code to set-up the
new arguments of the function.

## Arrays

    OP          A       B       C
    NEWARRAY    dst     size
    GETARRAY    dst     src     idx     dst[idx] = src
    SETARRAY    dst     src     idx     dst = src[idx]
    
Creates a new array of size `size` in the variable slot `dst`. The element size
of arrays is 64bit. The array index `idx` is read from a variable slot.

## Function Def

    OP      A       D
    FUNCF   lit     -
    FUNCV   lit     -
    
FUNCF & FUNV define a function with 'lit' fixed arguments. The FUNCV opcode 
defines a function which has an additional vararg argument. 
(defn [a b & c] ...) == FUNCV 2
    
## TODO

 * Define type layout and creation (vtables).
 * Representation of stack values: pointers, integers (NaN tagging).
 * Define byte code for defining and creating protocols.
 * Runtime extension of types (reify).


Resources
---------

###LuaJit:
http://wiki.luajit.org/Bytecode-2.0
http://wiki.luajit.org/Optimizations

###Java:
http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.2
http://en.wikipedia.org/wiki/Java_bytecode

###Clojure:
http://clojure.org/special_forms
http://clojure.org/datatypes
http://clojuredocs.org/clojure_core/clojure.core/deftype

https://github.com/clojure/clojurescript/blob/master/src/clj/cljs/compiler.clj
https://github.com/clojure/clojurescript/blob/master/src/clj/cljs/core.clj

https://github.com/clojure/tools.reader
https://github.com/clojure/tools.analyzer

https://github.com/halgari/clojure-py

http://clojure-py.blogspot.de/

###Guile

With Guile 2.2 they will have a VM with Register Bytecode, see:
http://wingolog.org/archives/2013/11/26/a-register-vm-for-guile


Problems:
----------

Clojure was designed to run on a host. Both JVM Clojure and ClojureScript (JS) assume some functionality to be present on the host platform.

These features either have to be inplmented in Clojure ontop of the current special froms or we have to provide native inpmnetation (clojure-py).

Ideas:
---------

There is only one looping construct in clojure, yet we often have more infomration from standard macros (dotimes ...)
. Maybe we can somehow use these information and give to the VM. We also have information about the length of loops over collection (map inc [1 2 3]). We have constant access to the size of persistent collection, this information might help the VM.



Desings:
--------
