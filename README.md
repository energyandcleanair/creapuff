# creapuff
A R package to generate input for CALMET, CALPUFF and what not


### Naming conventions
Since this is our first internal co-develooed package, I suggest the folowing conventions:

- variables names are lower case and use underscore: e.g. my_new_variable
- function names use underscore as well, and ideally includes a verb
- functions can be grouped using a prefix separated by a dot. For instance, calmet related functions could start with `calmet.` e.g. `calmet.generate_input()`