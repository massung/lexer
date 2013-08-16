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

Captures can be nested. The order in which they are captures is the order in which they are **opened**. For example:

	CL-USER > (match-captures (match-re #/(a(b(c)))(d)/ "abcd"))
	("abc" "bc" "c" "d")

Also, in addition to Lua-pattern captures, you can use typical regular expression ignored captures with `(?`:

	CL-USER > (match-captures (match-re #/(a(?b)+)(c)/ "abbbc"))
	("abbb" "c")

*Reminder: you can always use the `match-string` function to get at the full text that was matched and there's no need to capture the entire pattern.*

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

The `LEXER` package comes with a similar macro: `deflexer`:

	(deflexer lexer (&rest pattern-options) &body patterns)

The `deflexer` macro is called with a set of pattern/token pairs and produces a function that will parse the next token from a global `lexbuf` object.

A simple lexer example:

	CL-USER > (deflexer my-lexer (:case-fold t :multi-line t)
	            ("%s+")
	            ("="     (values :eq))
	            ("%a%w*" (values :ident $$))
	            ("%d+"   (values :int (parse-integer $$))))
	MY-LEXER

Every pattern in the lexer can have an optional form that will execute when the pattern is matched. If the form is omitted (or returns `nil`) then the token is skipped and the next token in the `lexbuf` is returned.

While inside the body of the pattern expression, all the `with-re-match` symbols are available to use (`$$`, `$1`, `$2`, ...). And each form should return a `token-class` and an optional `token-value`. These will be used to create the returned token (please see the [Interface to lexical anylizer](http://www.lispworks.com/documentation/lw61/LW/html/lw-321.htm) to understand why this is).

Now that we have a lexer function, the `tokenize` function can be called to create a `lexbuf` object and call the lexer until `lexbuf` has been exhausted.

	CL-USER > (tokenize #'my-lexer "x = 10")
	(#<LEXER::TOKEN IDENT "x">
	 #<LEXER::TOKEN EQ>
	 #<LEXER::TOKEN INT 10>)

*NOTE: The `tokenize` function also takes an optional `source` argument that can be used to identify where the string being parsed came from.*

If all the patterns in your lexer function fail to match, then a `lex-error` condition is signaled, letting you know exactly where the problem is located at.

	CL-USER > (tokenize #'my-lexer "x = $10" "REPL")
	Error: Lexing error on line 1 of "REPL" near "$"
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.

Now that we can generate a list of tokens, we can analyze them.

# Using PARSERGEN to Parse Tokens

Let's create a simple grammar.

	CL-USER > (defparser my-parser
	            ((start let) $1)
                ((let :ident :eq :int)
                 `(:let ,$1 ,$3))
                ((let :error)
                 (error "Invalid LET syntax")))
	MY-PARSER

Since our lexer returns tokens, and the parser expects multiple values for class/value, we need a helper function to spit out the next token. The `lexer` package comes with just such a helper function:

	(parse parser tokens)

Pass your `parsergen` function and list of tokens to `parse`, and it will hand the parser tokens (the class and value) as needed.

	CL-USER > (parse #'my-parser (tokenize #'my-lexer "x = 10"))
	(:LET "x" 10)
	NIL

In addition, if - while parsing - an error is signaled, it will be handled and re-signaled as a `lex-error`, indicating the error and where it occurred in the parse stream.

	CL-USER > (parse #'my-parser (tokenize #'my-lexer "x = 10 20"))
	Error: Invalid LET syntax on line 1 near "20"
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.
  
And done!

# Using Multiple Lexical Rules

Often times, you will be parsing text that has different lexical rules given the current context. For example, HTML allows embedding JavaScript between `<script>` tags.

This is easily handled since the lexer is actually a stack of rules. The functions `push-lexer` and `pop-lexer` are used in pattern bodies to dynamically switch the active lexer while tokenizing.

	;; push a new lexer, return a token
	(push-lexer lexer &optional class value)
	
	;; pop the current lexer, return a token
	(pop-lexer &optional class value)
	
Additionally, you may return a `token-class` and `token-value` while pushing or popping the current lexer.

*NOTE: Remember, if you return a `nil` token, the token is skipped. Use this feature to instantly start returning tokens from a new lexer!*

Let's give this a spin by creating a simple CSV parser. It should be able to parse integers and strings, and strings should be able to escape characters and contain commas.

First, let's define the CSV lexer:

	CL-USER > (deflexer csv-lexer ()
	            ("%s+")

	            ;; tokens
	            (","        (values :comma))
	            ("%-?%d+"   (values :int (parse-integer $$)))

	            ;; string lexer
	            ("\""       (push-lexer #'string-lexer :quote)))
	CSV-LEXER

Notice how when we hit a `"` character, we're going to push a new lexer and return a `:quote` token. The `:quote` token will signal to the grammar that we're now parsing a string.

Next, let's define our string lexer.

	CL-USER > (deflexer string-lexer (:multi-line t)
	            ("\""       (pop-lexer :quote))

	            ;; characters
	            ("\\n"      (values :chars #\newline))
	            ("\\t"      (values :chars #\tab))
	            ("\\(.)"    (values :chars $1))
	            ("[^\\\"]+" (values :chars $$))

	            ;; end of line/source
	            ("$"        (error "Unterminated string")))
	STRING-LEXER

This lexer has several interesting things going on. First, we see that when we find the next `"` character that we pop the lexer (returning to the CSV lexer) and also return a `:quote` token. Next, we can see that it handles escaped characters and then any number of characters up until the next backspace (`\\`) or quote (`"`). Finally, if it reaches the end of the line or file, it will signal an error.

*NOTE: The `#/$/` regular expression pattern will only match newlines if `:multi-line` is set to `T`.*

Let's try tokenizing to see what we get.

	CL-USER > (tokenize #'csv-lexer "1,\"hi\",2")
	(#<LEXER::TOKEN INT 1>
	 #<LEXER::TOKEN COMMA>
	 #<LEXER::TOKEN QUOTE>
	 #<LEXER::TOKEN CHARS "hi">
	 #<LEXER::TOKEN QUOTE>
	 #<LEXER::TOKEN COMMA>
	 #<LEXER::TOKEN INT 2>)

Perfect! Now let's write the grammar...

	CL-USER > (defparser csv-parser
	            ((start values) $1)

	            ;; a list of values
	            ((values value :comma values) `(,$1 ,@$3))
	            ((values value) `(,$1))

	            ;; a single value
	            ((value :quote chars) (coerce $2 'string))
	            ((value :int) $1)
	            ((value :error) (error "Illegal CSV"))

	            ;; strings to concatenate
	            ((chars :chars chars) (string-append $1 $2))
	            ((chars :quote) ""))

And let's give it a spin:

	CL-USER > (parse #'csv-parser (tokenize #'csv-lexer "1,\"hi\",2"))
	(1 "hi" 2)
	NIL
	
That's it!

# More (Usable) Examples

Here are some lexers used to parse various file formats. As with this package, they are released under the Apache 2.0 license and are free to use in your own projects.

* [JSON](http://github.com/massung/json)
* [XML](http://github.com/massung/xml)

More examples coming as I need them...

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
	   next-token
	   (for-each pattern
	     (when (match-re pattern source)
           (if (null body)
               (go next-token)
             (return-from next-token ,@body))))
       (error token-fail)))

# TODO List

There are still a couple things that I want to do:

* Add some good restarts to `lex-error`.
* Multiple, exclusive sets (e.g. `[%P%D]`).

None are especially hard, but they aren't a high priority at the moment. If one of these (or something else) is for you, let me know and I'll see what I can do.

# Thank You!

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others. If you find a bug or made a nice addition (especially if you improved performance!) please feel free to drop me a line and let me know at [massung@gmail.com](mailto:massung@gmail.com).