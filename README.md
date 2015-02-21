#InDependent


This project seeks to develop a gradual, dependently typed language as a proof-of-concept for the usefulness of such a system. In its present state, it achieves this through the interweaving of the dependently typed lambda calculus with JavaScript, a common dynamically typed language.

Many parts of the internal architecture were informed by Adrej Bauer's post here: http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/.

Note that this project is very much a work in progress and may contain bugs and be changed at any time.

##Syntax:


Valid statements in InDependent are of one of the following forms:

    --comments are any line that starts with two dashes (--)

To bind the value <expr> to a, we use the following.
Bindings in InDependent are immutable, so variables may only be bound once.

    a = <expr>

To explicitly specify the type of a term, you can include a type signature directly above it. The compiler will check to see if the type of the expression matches the signature and throw an exception if it does not.

    b : <expr>
    b = <expr>

------------
###Expressions

Expressions are described using the dependently typed lambda calculus. To those unfamiliar with the name this may sound complex, but the syntax is surprisingly simple and concise.

Anonymous functions are written as \x : T.y where x is the function parameter, T is the type of x, and y is the return value. A function that takes any Int and returns it can be written like so:

    \a : Int.a

Function application is written by separating the function and its arguments with spaces and surrounding them with parenthesis like so.

    (f a)

For functions with multiple parameters, we can still use a single set of parenthesis.

    (f a b)

Function types are written using arrow syntax, so A -> B is the type of a function from an A to a B. Here's the catch: sometimes, the *value* of the parameter can appear in the *type* of the result. When we want to use the value of the parameter in the return type, we can simply specify it like so: (a:A) -> B. This is what makes the language dependently typed.

Finally, since everything has a type, types have their own types! If Int is a regular type, then its type is written Type1. Why the 1? Well, Type1 also has to have a type, and its type is Type2 and so on.

-----------

A native statement tells the compiler that an expression of a given type exists in the compiled environment, allowing for JavaScript interoperability.

    native a : <expr>

A data statement creates an abstract data type with the given and exactly the given constructors. The type of the data statement may be specified. If it is not, then the ADT type is assumed to be of type Type1.

    data MyDataType : TypeType {
        cons1 : <expr>
        ...
    }    

Constructor arguments are determined by their type. An ADT representing a two dimensional point might be created in the following way:

    data Point{
        pt : Int -> Int -> Point
    }

Finally, arbitrary target-language code (for now JavaScript) can be inlined as a statement by surrounding it with the "{\*" and "\*}" symbols.

    {*
    var jsVar = pt(1, 2)
    var jsVar2 = b
    
    *}

As shown above, ADT constructors and variables can also be used directly in JavaScript.

##Using the Compiler

The compiler has a number of different flags and settings.

###Required
Exactly one of the following arguments must be passed to the compiler each time it is run.

"print" prints the parsed code as it is represented by the compiler.

"validate" attempts to validate the input code and prints both the internal representation and any errors that cause it to fail validation.

"compile" parses and validates the input code then, if it is valid, outputs compiled JavaScript.

###Optional

"-i" changes the input from a command-line string (the default) to a file specified as the next argument to the compiler.

"-o" similarly changes the output target from the command line to a specified file.

Example:

    ./inde -i MyFile.inde -o gen/MyFile.js compile

##Installation

To build the compiler from source, you need the Haskell platform. If you have not installed Haskell, go here: https://www.haskell.org/platform/

Once you have Haskell, run the following command to build from source:

    ghc -o inde DLRunner.hs

###Troubleshooting

If it complains that you do not have Parsec installed, make sure you have cabal, then run the following:

    cabal install parsec
