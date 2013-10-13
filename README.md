# The LEXER Package

The `lexer` package is a tokenizer for [LispWorks](http://www.lispworks.com) which makes heavy use of the [`re` package](http://github.com/massung/re).

## Creating a Lexer Function

[LispWorks](http://www.lispworks.com) comes with a fantastic [`parsergen`](http://www.lispworks.com/documentation/lw50/LWRM/html/lwref-433.htm) system that enables you to create a grammar using s-expressions (see the [`defparser`](http://www.lispworks.com/documentation/lw60/LW/html/lw-301.htm#pgfId-886013) function).

The `lexer` package has a similar function, `deflexer`, that can be used to tokenize a source string. Those tokens can be handed off to a parser grammar for analysis.

	(deflexer lexer &body patterns)

The `deflexer` macro is called with a set of pattern/token pairs and produces a function that will parse the next token from a global `lexbuf` object.

A simple lexer example:

	CL-USER > (deflexer my-lexer
	            ("%s+"   (values :next-token))
	            ("="     (values :eq))
	            ("%a%w*" (values :ident $$))
	            ("%d+"   (values :int (parse-integer $$))))
	MY-LEXER

*NOTE: If you don't understand the `$$` symbol in the example above, please see [this README](http://github.com/massung/re/README.md).*

Every pattern in the lexer can have an optional form that will execute when the pattern is matched. If the form is omitted (or returns `nil`), that signals to the tokenizer that the end of the source has been reached.

The keyword `:next-token` is special. When returned from a pattern form, then the token that was parsed is skipped and the next token in the stream is returned instead.

## Tokenizing

Now that we have a lexer function, the `tokenize` function can be called to parse a source string.

	(tokenize lexer string &optional source)

The `lexer` is our function and `string` is what will be tokenized. The `source` argument should be used to identify where `string` came from (e.g. a pathname), as it will be used in error reporting.

	CL-USER > (tokenize #'my-lexer "x = 10")
	(#<LEXER::TOKEN IDENT "x">
	 #<LEXER::TOKEN EQ>
	 #<LEXER::TOKEN INT 10>)

If all the patterns in your lexer function fail to match, then a `lex-error` condition is signaled, letting you know exactly where the problem is located at.

	CL-USER > (tokenize #'my-lexer "x = $10")
	Error: Lexing error on line 1 near "$"
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.

## Analyzing Tokens

Let's create a simple parser grammar.

	CL-USER > (defparser my-parser
	            ((start let) $1)
                ((let :ident :eq :int)
                 `(:let ,$1 ,$3))
                ((let :error)
                 (error "Invalid LET syntax")))
	MY-PARSER

Our lexer returns tokens, but the parser expects multiple values. The `lexer` package comes with a helper function that will pop the next token in our source and hand it to the parser in the correct format.

	(parse parser tokens)

The `parser` argument should be the grammar function created with `defparser`. The `tokens` is our list of tokens acquired via `tokenize`.

	CL-USER > (parse #'my-parser (tokenize #'my-lexer "x = 10"))
	(:LET "x" 10)
	NIL

Since each token knows where it came from, the `parse` function also will re-signal any condition thrown as a `lex-error` identifying where in the token stream the error occurred, as well as the lexeme of the token.

	CL-USER > (parse #'my-parser (tokenize #'my-lexer "x = 10 20"))
	Error: Invalid LET syntax on line 1 near "20"
	  1 (abort) Return to level 0.
	  2 Return to top loop level 0.
  
And done!

## Multiple Rules

Often, you will be parsing text that has different lexical rules given the current parsing context. For example, HTML allows embedding JavaScript between `<script>` tags, and in many languages quoted strings are a mini-DSL unto themselves.

Under-the-hood, all lexers are parsing the same `lexbuf` object bound to `*lexbuf*` and the lexer is actually a stack of lexing functions bound to `*lexer*`. This let's you change lexers dynamically while tokenizing using the following functions:

	;; push a new lexer, return a token
	(push-lexer lexer class &optional value)
	
	;; pop the current lexer, return a token
	(pop-lexer class &optional value)

	;; swap to a different lexer, return a token
	(swap-lexer lexer class &optional value)
	
Each of these will change the current lexer, and also return a token at the same time! This is very useful as the token can be used to signal to the grammar to change parsing rules.

*NOTE: Remember that `:next-token` is actually handled from within your lexer function! This means if you return `:next-token` while also changing lexers, the new lexer will not be called until after a complete token has been returned from your current lexer!*

Let's give this a spin by creating a simple CSV parser. It should be able to parse integers and strings, and strings should be able to escape characters and contain commas.

First, let's define the CSV lexer:

	CL-USER > (deflexer csv-lexer
	            ("%s+"      (values :next-token))

	            ;; tokens
	            (","        (values :comma))
	            ("%-?%d+"   (values :int (parse-integer $$)))

	            ;; string lexer
	            ("\""       (push-lexer #'string-lexer :quote)))
	CSV-LEXER

Notice how when we hit a `"` character, we're going to push a new lexer and return a `:quote` token. The `:quote` token will signal to the grammar that we're now parsing a string.

Next, let's define our string lexer.

	CL-USER > (deflexer string-lexer
	            ("\""       (pop-lexer :quote))

	            ;; characters
	            ("\\n"      (values :chars #\newline))
	            ("\\t"      (values :chars #\tab))
	            ("\\(.)"    (values :chars $1))
	            ("[^\\\"]+" (values :chars $$))

	            ;; end of line/source
	            ("%n"       (error "Unterminated string"))
	            ("$"        (error "Unterminated string")))
	STRING-LEXER

This lexer has several interesting things going on. First, we see that when we find the next `"` character that we pop the lexer (returning to the CSV lexer) and also return a `:quote` token. Next, we can see that it handles escaped characters and then any number of characters up until the next backspace (`\\`) or quote (`"`). Finally, if it reaches the end of the line or file, it will signal an error.

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
	            ((values :comma values) `(nil ,@$2))
	            ((values value) `(,$1))

	            ;; a single value
	            ((value :quote chars) (coerce $2 'string))
	            ((value :int) $1)
	            ((value :error) (error "Illegal CSV"))

	            ;; strings to concatenate
	            ((chars :chars chars) (string-append $1 $2))
	            ((chars :quote) ""))

And let's give it a spin:

	CL-USER > (parse #'csv-parser (tokenize #'csv-lexer "1,\"hello, world!\",,,2"))
	(1 "hello, world!" NIL NIL 2)
	NIL
	
That's it!

# More (Usable) Examples

Here are some lexers used to parse various file formats. As with this package, they are released under the Apache 2.0 license and are free to use in your own projects.

* [JSON](http://github.com/massung/json)
* [XML](http://github.com/massung/xml)
* [HTTP](http://github.com/massung/http)

More examples coming as I need them...

# Thank You!

If you get some good use out of this package, please let me know; it's nice to know your work is valued by others. If you find a bug or made a nice addition (especially if you improved performance!) please feel free to drop me a line and let me know at [massung@gmail.com](mailto:massung@gmail.com).