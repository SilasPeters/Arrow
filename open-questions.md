# Open questions

## Exercise 4
Happy supports both right-recursive grammar and left-recursive grammar.
Specifically, Happy prefers left-recursive grammars, as they claim the following:

> Happy is is more efficient at parsing left-recursive rules, [since] they result
> in a constant stack-space parser, whereas right-recursive rules require stack space
> proportional to the length of the list being parsed.

They also state that, when using left-recursive grammars, Happy generates lists in
reverse order. Thus, after parsing, all lists where this is not preffered have to
be reversed.

Comparing this against the classical parser-combinators as explained in the lessons,
preffering left-recursive grammars is quite odd. Classic parser-combinators will
loop infinetly on left-recursive grammars, as you would infinetly 'backtrack' to
previous tokens as you never consume one. The parser will not change state.

Happy explains how they solve LR grammars:

> Problematic recursions are detected as zero-span reductions in a state which
> has a goto table entry looping to itself. A special symbol is pushed to the
> stack on the first such reduction, and such reductions are done at most once
> for any token alternative for any input position. When popping from the stack,
> if the last token being popped is such a special symbol, then two stack tails
> are returned: one corresponding to a conventional pop (which removes the symbol)
> and the other to a duplication of the special symbol (the stack is not changed,
> but a copy of the symbol is returned). This allows sufficient copies of the empty
> symbol to appear on some stack, hence allowing the parse to complete.

## Exercise 10

It is better to call recursive commands at the end of command sequance if you want to reduce the maximum stack size.
First we observe that executing a command removes it, reducing the stack size by one.
For non recursive commands this is all they do with the stack size. But recursive commands add a list of commands to the stack on execution. This list can be empty, or one, but often wil contain many commands. If 20 commands are called of wich one is a recursive command referencing 21 non recursive commands we can see a large difference in maximum stacksize depending on the order the commands are called. If the recursive command is called first the stack size is first reduced to 19, then 21 commands are added to gett a stack size of 40. If the recursize command is called last the stack only reaches a maximum size of 21.




