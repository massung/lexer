# The LEXER Package

The `LEXER` package is a very simple and small regular expression library for [LispWorks](http://www.lispworks.com). It follows the pattern matching syntax used by [Lua](http://www.lua.org) for simple regular expression functionality: matching, searching, splitting, and replacing. You can see the syntax for these patterns [here](http://www.lua.org/pil/20.2.html). It also has a lexer macro that tokenizes strings and is designed to be used in conjuction with the [`PARSERGEN`](http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-433.htm) package built into [LispWorks](http://www.lispworks.com).

# Lua-style Regular Expressions

The two exposed classes are `re` and `re-match`. The `re` class doesn't expose any functionality, but the `re-match` class exposes the following reader functions:

	#'match-string
	#'match-captures
	#'match-pos-start
	#'match-pos-end

# Creating a Regular Expression

To create a `re` object, you can either use the `compile-re` function or the `#/` dispatch macro.

	CL-USER 1 > (compile-re "%d+")
	#<RE "%d+">

	CL-USER 2 > #/%d+/
	#<RE "%d+">

The `compile-re` function allows you to pass some extra options to the regular expression.

	(compile-re pattern &key case-fold multi-line)

If `case-fold` is non-nil then pattern matching on that expression will be case-insensitive. If `multi-line` is non-nil then the `$` (end of input) pattern will match newlines and the `.` pattern will not match newlines.

*Hint: when using the `#/` character dispatch macro, use `\` (backslash) to escape the `/` and other characters that might mess with LispWorks's syntax coloring.*

# Pattern Matching

Once you have a regular expression object, you can send it off to one of the pattern matching functions:

	(match-re re string &key start end exact)
	(find-re re string &key start end all)
	(split-re re string &key start end all coalesce-seps)
	(replace-re re with string &key start end all)

The `match-re` function attempts to match (from `start`) the expression exactly as it is. If `exact` is non-nil then `match-re` will return `nil` if the pattern doesn't match to the end of the string. It returns a `re-match` if the pattern matches.

The `find-re` function will scan through the string and look for any match to the pattern. If `all` is non-nil, then it will return a list of all matches found, otherwise it simply returns the first match (or `nil`).

The `split-re` function will find a match and then split the input string at the match. If `all` is nil, then 2 values are returned: the `subseq` left of the match and the `subseq` to the right of the match. If `all` is non-nil, then a list of subsequences are returned. If `coalesce-seps` is non-nil then any subsequence that is an empty string will be left out of the results. `coalesce-seps` is ignored if `all` is `nil`. If `all` is `nil` and the separator pattern is not found, then just a single value (the original string) is returned.

The `replace-re` function searches for the pattern within the string and then calls the `with` function, passing it the `re-match` found. The `with` function then returns a new string to replace. If `all` is non-nil, then all patterns within `string` are replaced.

# Examples

Trying to match a simple pattern with a string...

	CL-USER > (match-re #/%d+/ "abc 123")
	NIL

	CL-USER > (match-re #/%d+/ "123 abc" :exact t)
	NIL

	CL-USER > (match-re #/%d+/ "123 abc")
	#<RE-MATCH "123">

	CL-USER > (match-pos-end *)
	3

Looking for a pattern somewhere in a string...

	CL-USER > (find-re #/%d+/ "abc 123 def")
	#<RE-MATCH "123">

	CL-USER > (find-re #/%d+/ "123 4 5678" :all t)
	(#<RE-MATCH "123"> #<RE-MATCH "4"> #<RE-MATCH "5678">)

Splitting a string on a pattern...

	CL-USER > (split-re #/,/ "1,2,3")
	"1"
	"2,3"

	CL-USER > (split-re #/,/ "1 2 3")
	"1 2 3"

	CL-USER > (split-re #/,/ "1,2,3" :all t)
	("1" "2" "3")

	CL-USER > (split-re #/,/ "1,,,,2,,3" :all t)
	("1" "" "" "" "2" "" "3")

	CL-USER > (split-re #/,/ "1,,,,2,,3" :all t :coalesce-seps t)
	("1" "2" "3")

Replacing parts of a string matching a pattern...

	CL-USER > (replace-re #/%d/ #'(lambda (m) "*") "1 2 3")
	"* 2 3"

	CL-USER > (replace-re #/%d/ #'(lambda (m) "*") "1 2 3" :all t)
	"* * *"

# Captures

Using parenthesis in a pattern will cause the matching text to be captured as a sub-expression in the returned `re-match` object. The `match-captures` function will return a list of all the captured strings in the match.

	CL-USER > (match-re #/(%d+)(%a+)/ "123abc" :exact t)
	#<RE-MATCH "123abc">
	
	CL-USER > (match-captures *)
	("123" "abc")

Captures can be nested. The order in which they are captures is the order in which they are *closed*. For example:

	CL-USER > (match-re #/((%a)(%a+))/ "testing")
	#<RE-MATCH "testing">

	CL-USER > (match-captures *)
	("t" "esting" "testing")

*Reminder: you can always use the `match-string` function to get at the full text that was matched.*

# The `with-re-match` Macro

Once you have a `re-match` object, while you can get at the captures and matched string yourself, the `with-re-match` macro is very helpful in letting you use that information in a more readable manner.

	(with-re-match ((var match-expr) &body body)

The `var` is the symbol that will be lexically bound to the results of `match-expr`. Once inside `body`, however, There will be 10 additional symbols: `$$`, `$1`, `$2`, `$3`, ... `$9`. These are bound to the `match-string` and the `match-capture` strings.

	CL-USER > (with-re-match (m (find-re #/{([^}]+)}/ "this {is a} test"))
	            (print $$)
	            (print $1))
	"{is a}"
	"is a"
	"is a"
	T

The first two results are the `print` outputs. `$$` was bound to the `match-string` of the match and `$1` was bound to the first captured submatch.

The last two results are the multiple return values of `body` and `T` indicates that the match was non-nil. Had `find-re` returned `nil`, then `body` would not have executed and `nil` would have been returned. This distinguishes itself from when `body` returns `nil`, but the match was found.

	CL-USER > (flet ((initial (m)
	                   (with-re-match (v m)
	                     (format nil "~a." $1))))
	            (replace-re #/(%a)%a+%s*/ #'initial "Lisp In Small Pieces" :all t))
	"L.I.S.P."

# Tokenizing With `deflexer`

The pattern matching functionality provided - while very useful - is only a small part of what the `LEXER` package was designed for.

[LispWorks](http://www.lispworks.com) comes with a fantastic [`PARSERGEN`](http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-433.htm) package that - given a grammar - will create a function to parse a series of tokens (see the [`defparser`](http://www.lispworks.com/documentation/lw60/LW/html/lw-301.htm#pgfId-886013) function). 

The `LEXER` package comes with a similar macro: `deflexer`. The `deflexer` macro is called with a set of token/body pairs and produces a function that - when handed a string - returns a new function that can be sent to a parser, which will then lazily match a new token each subsequent call.

A simple example:

	CL-USER > (deflexer my-lexer (:case-fold t :multi-line t)
	            ("%s+")
	            ("%d+"   (values :number (parse-integer $$)))
	            ("%a%w*" (values :ident $$))
	            ("="     :eq))
	MY-LEXER

	CL-USER > (my-lexer " x = 10 ")
	#<anonymous interpreted function 21B0FA8A>

Each time the anonymous function is called, it will return the next token in the stream. Patterns that have no token body associated with them (e.g. the whitespace example above) are skipped. The token patterns are tried, in-order, so if there is any ambiguity the first one will win out (read: be careful!).

*Note: whatever keyword options are passed to the lexer are also passed to all the token patterns compiled.*

Let's put the above lexer to some use by first creating a really simple grammar...

	CL-USER > (defparser my-parser
	            ((start let) $1)
                ((let :ident :eq :number) (list :let $1 $3)))
	MY-PARSER

We can now send our lexer (with source) to the parser...

	CL-USER > (my-parser (my-lexer "x = 10"))
	(:let "x" 10)
	NIL

And done!

# Error Handling

While the tokenizer function produced by `deflexer` reads tokens from a string, it doesn't actually parse the entire string and then keep handing out tokens. Instead, it's parsing the tokens on-demand each call. For this reason, when all the token patterns fail, a `lex-error` condition will be signaled that specifies where in the source string (line # and at which exact character offset) things have gone wrong.

	CL-USER > (my-lexer "!x = 10")
	#<anonymous interpreted function 21CE915A>

	CL-USER > (funcall *)

	Error: Parse error on line 1 near '!'
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.

Work is being added for additional restart options like editing the source, viewing the line the error occured on, skipping the character, etc.

# How It Works

Under-the-hood, each compiled pattern (`re` object) is actually a [Parsec](http://legacy.cs.uu.nl/daan/parsec.html)-like, monadic function that takes an input stream as its only argument. It then tracks whether the function returned `nil` (failed to match) or non-nil and how many characters from the stream were matched.

As an example, the pattern `"%a%w*"` would compile to the following monadic function:

	CL-USER > (lex::bind (lex::one-of lex::+letter+) (lex::many (lex::one-of lex::+alpha-numeric+)))
	#<Closure 1 subfunction of LEXER::BIND 21AAA112>

That closure can then be passed an input stream (wrapped in a state object):

	CL-USER > (with-input-from-string (s "abcd1234")
	            (funcall * (make-instance 'lex::lex-state :source s)))
	T

And we now see that the string `"abcd1234"` matches the pattern `"%a%w*"` and can look at the input stream to see how many characters were matched.

There is a bit more to it than the above (captures, etc.) but the above shows the majority of what's going on under-the-hood. The code is not very complex... feel free to dive in and take a look!

The `deflexer` macro builds a function that checks each of the token patterns in-order. The first one to succeed has its body executed and returned from the function. In pseudo-code, each call of the resulting function would look something like this:

	(block next-token
	  (tagbody
	   skip-token
	   (for-each pattern
	     (when (match-re pattern source)
           (if (null body)
               (go skip-token)
             (return-from next-token ,@body))))
       (error token-fail)))

# TODO List

There are still a couple things that I want to do:

* Add some good restarts to `lex-error`.
* Multiple, exclusive sets (e.g. `[%P%D]`).

None are especially hard, but they aren't a high priority at the moment. If one of these (or something else) is for you, let me know and I'll see what I can do.

# Thank You!

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others. If you find a bug or made a nice addition (especially if you improved performance!) please feel free to drop me a line and let me know at [massung@gmail.com](mailto:massung@gmail.com).