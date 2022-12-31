MiniML write up

My extension of choice was a lexically-scoped environmental model evaluator. Since OCaml is a lexically scoped language, I thought it appropriate to ensure that there was an option for evaluation to be governed by the lexical structure of the expressions, as opposed to soley being evaluated based in the dynamic ordering of the expression.

The important distinction comes in the expression given to us in the Project Outline:

let x = 1 in
let f = fun y -> x + y in 
let x = 2 in f 3 ;;

In our dynamically scoped environment, because f is applied to 3 when the most recent assignment to x is 2, the output is 5. However, in our lexical scope, the value is 3, since the function is defined when 1 is assigned to 1. To summarize, the output is 4 when the environment in force is the environment at function definition, and the output is 5 when the environment in force is the environment at function application.

To implement this, I altered the evaluation of functions and applications. For functions, the function was made into a closure of that function and the current environment it is in. Now, when we evaluate an application, the function can be evaluated in the environment that it was defined in, as opposed to be evaluated in the most recent envioronment upon application.

In order to ensure that there was minimal repeated code, I made a function defualt_eval, which takes in an additional parameter of type scope, where I defined the type scope to either be Lexical or Dynamic. I was then able to separate the slight differences between Lexical and Dynamic by just using a simple if ... then in order to separate between if the evaluation is lexically or dynamically scoped. This allowed me to just use one line of code to implement both the eval_d and the eval_l functions.

I then made a slight change to the miniml file, in order to print the result of any given evaluation in all 3 scopes, so that we can learn about the differences between them. As such, the result of evaluating the above expression in my evaluator is: 

==> Substitution: 4
==> Dynamic: 5
==> Lexical: 4