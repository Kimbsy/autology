We needed a way to be able to have raw C code in our autology program. This poses a big problem for a normal interpreted lisp, the first thing we'd normally do is read the file as a string, then use our Clojure `read-string` to transform it into a tree of lists. Unfortunately C code is not valid Clojure so the reading breaks for any number of reasons (thinking braces are a map instead of a code block and complaining of an odd number of forms, seeing the semicolons in the for loop header as commenting out the rest of the line! etc.).

So we wrap the body of the `with-*i*` expression in a string (we need to remember to triple escape any original double quote characters, like in a `printf` statement), this requires recursively slurping up the characters in the file while keeping track of parentheses balancing so we stop before leaving the scope of the new interpreter. Then we can reconstruct our program as a single string, then pass this safe version to clojure's `read-string`.


(looking at the for loop header being treated as a comment because we're using clojure syntax highlighting)
> by the time we get to this semicolon we're not longer in kansas baby! we're in C! or at least our budget approximation of runtime simulated C.


- mention that this is _not_ how you would normally interpret a C program, lexer, parser, compiler etc.


> So we have to `read-string` the new evaluator and pass it to eval. Yes I know that's not safe, but we're writing a language that can modify it's own interpretation, we're a good way past safe a this point.


> theoretically you could dump random bytes into a file, then manipulate autology's interpreter to recognise that file as a program that solves any problem. you could even have a single random file and two interpreters which both recognise it as a valid program that solve different problems!

> here we have one interpreter that's looking for a Lisp-like tree, here's another which is looking for a script-like sequence of imperative statements. here's one that takes a file of random bytes and uses it as a seed to generate music. Ok so that's not really a programming language right? that#s just a program that reads input data and does stuff with it ...... <they're the same picture.jpg>


make funny joke about lambda days attendees maybe not being familiar with "for loops", describe them as a form of stateful imperative recursion.
