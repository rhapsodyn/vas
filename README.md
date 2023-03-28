# Vas Programming Language

(Ja)vas(cript) is some weird but sane subset of early javascript ([es3](https://www.ecma-international.org/wp-content/uploads/ECMA-262_3rd_edition_december_1999.pdf) i suppose).

It is DESIGNED to be:
1. Simple enough to handcraft.
1. Stack based (no garbage-collection no reference-counter). 
1. No deps (`norm` is sexy, `anyhow` is handy, `smartstring` can boost, but no thanks).

**CAUTION:** Just some kinda toy language.

## Weird

1. Lack of: `throw-try-catch-finally`, `while` (just using `for(;;)`), `switch-case`, `anonymous function` etc.
1. Not functional: seperate `function` with `value`, no `callback`s.
1. SemiColon is not optional!
1. Array == Object (just `table`s, like [Lua](https://www.lua.org/pil/11.1.html)), but only indexing by `number` or `string`.

## Sane

``` javascript
var a; // a == null
```

`undefined` is gone

``` javascript
true === false; // not ok
```

`===` is gone

``` javascript
function notCtor() {}
new notCtor(); // not ok
```

`new` and `prototype` are gone

``` javascript
var a = 1;
var b = false;
var c = null;
var d = "string";

a + a; // num + num ok
d + d; // string + string ok
b || b; // bool || bool ok

// all else are not ok
```

dynamic but not THAT dynamic

``` javascript
function evil() {
	a = 1;
}

evil();
```

would throw `undefined variable` instead of assigning to global

## Learnings:
1. function eval (local scope affairs) is hard to impl correctly
1. postfix is best for expression parsing (to my taste)

## TODOs
1. stdlib
1. complicated data structures (object? array?)
1. repl
1. interpreter:
	1. some ast node have to be `clone`d all the time, how to share them?
1. virtual machine
