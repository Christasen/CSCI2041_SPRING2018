# How to use OCaml

When using OCaml on the CSELabs machines, the following should get you
started.

### Test OCaml installation.

Execute the following:
```
% which ocaml
```

This should display the path ``/usr/local/bin/ocaml``.

Also execute the following:
```
% which utop
```

This should display the path ``/usr/bin/utop``.

Finally, if the first command was successful, execute:
```
% ocaml --version
```

This should indicate that version 4.06.0 is installed.  If an older
version is installed please let a TA know.  Include the name of the
computer on which this old version is installed.  Ask them to contact
Eric about the problem.


### Use OCaml

Go back to the lab 1 directory in your individual repository.

Perhaps by the following commands: 
```
% cd
% cd csci2041/repo-user0123/Lab_01
```

Start the OCaml interpreter:
```
% ocaml
```

At the Ocaml prompt (#) enter the following (do type "#use", not just
"use"): 
```
#  #use "fib.ml" ;;
```
Note that the prompt is "#" and directives to the interpreter to load
files and quit also begin with a "#". 

If this is the original program, then OCaml will report an error:
> File "fib.ml", line 10, characters 30-31:
> Error: Unbound value n 

Quit OCaml using the "quit" command as illustrated below:
```
# #quit ;;
```

Use an editor of your choice (emacs, vim, gedit, etc.) to replace 
the variable `n` with the correct variable `x`.

Also, replace the text between dots in the comment with your name if
you've not already done so.

Save the file and repeat the steps above to start OCaml and load the file.

Now compute the 5th Fibonacci number by typing the following:
```
# fib 5 ;;
```

There is a bug in this program.  It will return `16` instead of the
correct answer of `5`.  Let's fix that bug now.


### Using utop

Many find the ``utop`` interface to the OCaml interpreter more
convenient.  This is run simply by typing ``utop``.  The commands are
the same as for the simple OCaml interpreter described above.
