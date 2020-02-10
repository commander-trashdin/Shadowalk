# Shadowalk
A custom storyteller-based system, heavy development stage.

# Manual

# What can it do now:

Creatures can be defined like this:

   ```common-lisp
    (defcharacter Name race
     (stats (2 1 1 3 7 2 2 2 2))
     (size 2)
     (skills (:acrobatics 5 :running 5))
     (feat-list ())
     (inventory ())) 
     ```
Stats go in order strength, agility, constituion, charisma, perception, intuition, will, logic, reaction.
Skills do not have to be mentioned, if they are 0.
Defines a new creature, adds it to the *creatures* hash-table
 and adds %definition to the log file to avoid cyclic log inputs.

Actions are defined like this:

``` common-lisp
(make-action run Lex :modifier 2 :body (6 strength running 5))
```
Creature should exist, otherwise error..
If :body is provided, it redefines (or defines new) action. Body has a form of (dice stat skill check against this)
Then action is defined (as a function) and added to the log.
Then it gets executed (with a provided modifier -- 0 by default)

## License
CCA

