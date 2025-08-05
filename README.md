This script will model any classically dynamic system using the calculus of variations. It calculates the kinetic and potential energy in an arbitrary coordinate system and prepares the constrained/unconstrained Euler Lagrange equations through a built in function with a display.
This readme has a verbose section on the pure functions that start the code, just in case this gets abandoned and I forget how it works. Skip to the very end if you just want to know what these functions do.

DERIVATIVES
This script assumes that all total derivatives are total time derivatives. We assume that the coordinates which describe our system are independent of each other in order to construct the Euler Lagrange equations anyhow, and so this assumption isn't bad, it just may surprise you that Dt[anything] shows up in dot notation.

CORE FUNCTIONS
There is a closed cell grouped under the ClearAll["Global`*"] block. You can expand it by clicking on it's tiny bookend, dropping down the cell menu from the top, hovering cell properties, and checking open. This cell group has the functions for the Euler Lagrange equations and chain rule integration pattern matching. Either way, just make sure that if you clear Global, that you click that tiny bookend and run the closed (or open if you leave it open) section with these crucial functions so they don't go away with everything else.

PURE FUNCTIONS
The functions at the very beginning are personal QOL. Any function with #'s is a pure function. That is, it takes a varying number of input arguments. In deciphering these definitions, check the leftmost block before the first & to see what's coming out in general, then read them from right to left to see what chain of sinister events the input variables go through. For the simplest function Whateva:=#&, calling Whateva[yeya] will spit back 'yeya'. Calling Whateva[{yeya,nahh}] spits {yeya,nahh}. The hashtag is a very cool lil shortcut. It can even be used in regular expressions, like Integrate[InsaneIntegrand,{boringlims}] could be written as Integrate[#,{boringlims}]&[InsaneIntegrand]. This way if you have a bunch of wild integrals you're doing one line after the other, you can bring out the integrands to the right and separate them from a mess of expressions inside your Integrate function. Even Integrate[RandomFactors*FunctiononIntegrand[#],{lims}]&[Integrand] is kosher. Just make sure there's an & around. It signifies that this thing is waiting on some input from the right.
In the case of a function like Whateva, it's the argument that comes in 'from the right'. In other words, Whateva[yeya] is the same thing as #&[yeya]. One more thing before I glance over the gore of the psycho functions. The # in Whateva is one degree deeper than I'm letting on. In the case of Whateva[{yeya,nahh}], the # has an index at [[1]] and [[2]] such that
Whateva := #[[2]] &
Whateva[{yeya, nahh}] outputs nahh.

If you were to write 
Whateva := # &
Whateva[yeya, nahh]	(not a vector input)
you get yeya. If you want to find nahh, you'd define Whateva as #2& (not #[[2]]). In short these pure functions take any number of arguments that you want. The finished product doesn't have to use all or any of em. Whatever number input argument you want to access in the expression to the left of the & is stored in #thatnumber.
Lastly, ##& gives back all the input arguments at once. Doing {##}& is really useful as it returns them in a vector. From there you can do really tricky stuff.

PSYCHO FUNCTIONS
So using the #, inputs flow from right to left. Your function can be more complicate, like Mag for instance.
Mag := If[VectorQ[#], Sqrt[# . #], #] &;
Calling Mag[Ex] takes Ex (some expression that may be a vector, may not be) and bring it all the way to the left. Ex then goes through If[VectorQ(uery)[Ex], (return this if it is) Sqrt[Ex . Ex], (or if it's not) Ex], so that If you want to get the magnitude of something and plot it for instance, and you don't care to check if it's a vector or not, you can do that real quick.
If you're a real psycho, though, you'll chain these things together. Check out QA (quick array).
QA := Table[#1, {#2, #3}] &[#1, If[AnyMatch[{1, Null}, #1], i, Vars[#1][[1]]], #2] &;
Looking at the left most block (before the first &) this one makes a vector/table/list (I mean all these are just vectors). Calling QA[c,3] returns the vector {1,2,3}, just like Table[c,{c,3}] (just a for loop btw >:L) would do. Doing QA[c^2,3] gives {1,4,9}, QA[D[Cos[t], {t, n}], 3]={-Sin[t], -Cos[t], Sin[t]} (it's important that n comes before t alphabetically in this one btw. QA is going to look for a variable to iterate over and it'll pick alphabetically)
From the right, now, to the left, it stores the first value coming in from the input, #1 in the first space. In the second space it has a flag for when #1 (the first input argument from when you call QA[1st arg, 2nd arg,...]) is either 1 or Null (which you also get from just leaving it empty, like QA[,3]) and stores a constant i if it is or Vars[#1][[1]] if it ain't. Not to drag too much, I don't really have simple examples here, but to be clear Vars just takes in any expression and reduces it to a list of the variables used in the expression. Vars[Cos[t*Exp[Sqrt[n^2]]]] returns back {n,t} (that alphabetical order) to ya if you want to set them to one or something. That's why there's a [[1]] in Vars[#1][[1]]. It's because Vars returns a vector/list. SO, the first block in QA from right to left has three parts and takes in two inputs from QA, #1 & #2. The first part is just the first input argument, #1. The second part is a constant i if #1 is 1 or null or, if not, it's whatever variable was used to write the first input argument (the lowest alphabetically if many were used). Finally, the third part is just the second input argument, #2. After the input arguments pass from right to left through the first block, the & on the left side of that block pulls from whatever those input arguments evaluated to and pushed them to the next block to the left. For example:
QA[c^2,3] takes [c^2,3] into [#1, If[AnyMatch[{1, Null}, #1], i, Vars[#1][[1]]], #2]. This evaluates as [c^2, If[AnyMatch[{1, Null}, c^2], i, Vars[c^2][[1]] ], 3] = [c^2, If[False, i, {c}[[1]] ], 3] = [c^2, c, 3]. Now this gets pulled into Table[#1, {#2, #3}] on the left, where #1, #2, & #3 have evolved through the conditions in the first block into {c^2, c, 3}. The last block then evaluates with [c^2, c, 3] into Table[#1, {#2, #3}], going to Table[c^2 ,{c, 3}] = {1,4,9}.

If you want to test and experiment around, copy QA's definition from the MeFuncs package into global. Making these, I did tests like this constantly:
QA2 := {##} &;
QA2[1, 3]

QA2 := {##} &[#1, If[AnyMatch[{1, Null}, #1], i, Vars[#1][[1]]], #2] &;
QA2[1, 3]

QA2 := Table[#1, {#2, #3}] &[#1, If[AnyMatch[{1, Null}, #1], i, Vars[#1][[1]]], #2] &;
QA2[1, 3]

All in one evaluation block, you can see how the input arguments evolve as they pass along the chain. With these psycho strategies, you can do really really nice things, like this one I use constantly in the end of the very first block: Times@@{##}/.#1->1 (Apply multiplication to everything in the list ahead, which is all the input arguments, but set the first one to 1). In the case of S (my version of simplify which is very easy to get out)
S := If[#2 === 3, #1 /. g21[], #1] &[If[AnyMatch[{2, 3}, #2], FactorTerms[#1, CsQ[#1]], #1], #2] &[If[AnyMatch[{0, Null}, #2], #1,Refine[FullSimplify[#1], Variables[#1] \[Element] Reals] /. Abs[q_] :> q], #2] &[#1, (Times @@ ({##} /. #1 -> 1))] &; 
This looks insane but, trust me, it's not that bad. Start from the right most / last block. S takes an expression to simplify, but there's still space reserved in this first block for a second input though. You can test this, like how I showed above, and that the input arguments evolve through the first block as [1st input (expression to simplify), (1st input -> 1)*Product of 2nd-infinite'th input]. If there is no 2nd, or infinite'th input, this second piece is just 1 from #1. If the second input is 0, or if it's just blank, like S[Ex,], then the second piece here becomes 0 or null. Moving down the chain, you can watch as this flag is carried along to the finish. In this case, if the original input was S[Ex,0] or just S[Ex,], the expression is not simplified. You can really easily switch this on and off by evaluating at S[Ex] then tapping S[Ex,] to see what simplification the function is making. If the second argument was 1, the thing simplifies as normal. If it was 2, any numbers or constants (defined by a SetAttributes[{m, g}, Constant] line) are factored out. If it's 3, the flag is checked at the very end and the expression has the replacement rule g21 (go to 1) applied onto it, which sets any constant (not numbers, variables that have been defined to be constants) to 1 in the expression.
One last note. [#1, (Times @@ ({##} /. #1 -> 1))] &; at the start is flashy, but it's not just for fashion. If you use [#1, #2] &; at the very beginning, the whole thing won't move if you don't always call your function with two arguments. It's eagerly expecting that #2 input argument. I don't like that.
Anyways. Mess with em. g21 is the most psycho. You have to use it like Ex/.g21[], so it has an expression to hang out with, but it takes either no input arguments, g21[{r}] if you want to let r go to 1 too, or g[{r,a[0]},] if you want r and a[0] to go to 1 and you want to toggle by adding or removing that comma at the end.

* Toggle with a comma or a ,0 at the end.
** Adds an extra function for extra input arguments.
*** Both

CsQ: ConstantQuery / grab constants from an expression.
Flatn**: Applies Flatten. Also deletes any instances of x's in a list if you call Flatn[list,x] where x is whatever you want.
FNCheck: A tool to check if a custom function is behaving right by comparing it to a built in one.
Intg: Returns the integrand from some expression
Intl: Returns the limits of integration
Mag: Gives the magnitude
RP*: Replace. RP[Ex,{x0,y0},{xf,yf}] lets all instances of x0 and y0 -> xf and yf. %%%%% MVP %%%%%
RPT*: Same as RP, you can just use it as a tag, like expression /.RPT[] if it makes things more readable
SS**: Solve but doesn't return the stupid {x->whatever} output that solve does. Tab through multiple solutions with the third argument.
S***: Simplify at varying levels.
g21***: Crude simplification tool. Goes to 1. Automatically recognizes constants in an expression if they have the constant attribute.
g20***: Crude simplification tool. Goes to 0.
Trm: Gives the list of addends in and expression.
Vars**: Returns the most fundamental variables in an expression. Uses differentiation to get rid of constants if the second input argument is null or 0.
d: Total derivative.
PRM**: Parametrize variables. Unparametrize (if the expression isn't a total mess, integrals are hard) with 1 in the second input argument.
