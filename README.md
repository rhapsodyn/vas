# Vas Programming Language

(Ja)vas(cript) is some weird but sane subset(not really, just looks like) of early javascript ([es3](https://www.ecma-international.org/wp-content/uploads/ECMA-262_3rd_edition_december_1999.pdf) i suppose).

It is DESIGNED to be:

1. Simple enough to handcraft.
1. Stack based (no garbage-collection no reference-counter). 
1. No deps (`norm` is sexy, `anyhow` is handy, `smartstring` can boost, but no thanks).

**CAUTION:** Just some i-have-to-write-a-language-once-in-my-life project.

## Weird

1. Lack of: `throw-try-catch-finally`, `while` (just using `for(;;)`), `switch-case`, `anonymous function` etc.
1. Lack of: `+=`, `++`, `%`, `&` etc.
1. Not functional: seperate `function` with `value`, no `callback`s.
1. Data is Data, function is not data or `function object`: `var a = {b: 1}` ok, but `var a = {b: function() {}}` not.
1. Array == Object (just `table`s, like [Lua](https://www.lua.org/pil/11.1.html)), but only indexing by `number` or `string`.
1. ~~SemiColon is not optional!~~

## But Sane

``` javascript
var a; // a == null
```

`undefined` is gone.

``` javascript
true === false; // not ok
```

`===` is gone.

``` javascript
function notCtor() {}
new notCtor(); // not ok
```

`new` and `prototype` are gone.

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

dynamic but not THAT dynamic.

``` javascript
function evil() {
	a = 1;
}

evil();
```

would throw `undefined variable` instead of assigning to global.

## TODOs
- [ ] more & more test (~30 tests NOW)
- [ ] stdlib
- [x] complicated data structures (object? array?)
	- [x] just tables
- [ ] object literals
- [ ] repl
- [ ] interpreter:
	- [ ] some ast node have to be `clone`d all the time, how to share them?
	- [ ] function call pass param by value, AKA too many `clone`s.
- [ ] virtual machine

