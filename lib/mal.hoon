/-    mal
/+    help

::  TODO:
::    switch the special apply stuff to a map and into ++eval for TCO
::    add more helper funtions, make ++ns use them (remember get-atom?)
::      things like "try*" look *terrible*
::    switch all those fucking reeds to (riff a b) smh
::    get rid of all the !! and asserts
::    switch to an actual sole app instead of this lib and gen thing
::    ~hoon ffi~ maybe somehow eventually

!:
::  this is really weird and i don't get why it's not just =+  mal  again
=+  [help . ^mal]
::
|%
++  parse-res
    (result mal-type tape)
::
++  mal-type
$?
  $nil
  $true
  $false
$%
  {$list p/(list mal-type)}
  {$vect p/(list mal-type)}
  {$atom p/@s}
  {$reff p/@}  ::  this is mal's `atom`. oops.
  {$symb p/tape}
  {$str p/tape}
  {$fun p/mal-lambda mac/?}
==
==
::
++  eval-err
    $?
      $@
        $?
        $empty-call
        $special-fail
        ==
      $%
        {$eval-bad-func mal-type}
        {$throw p/mal-type}
      ==
      ::
      {$parse tape}
    ==
::
++  mal-res  (result {mal-type _mal} eval-err)
::
++  table
  (map tape mal-type)
::
++  mal-lambda
  ::$-({(list mal-type) _mal} {mal-type _mal})
  _^?(|=({(list mal-type) _mal} *mal-res))
::
++  parser
  |%
  ++  spaces
    %+  cook  rome
    %+  cold  ~
    %-  star
    ;~  pose
      ace
      gap
      com
    ==
  ::
  ++  comment
    ::%+  cook  (hint %comment parse-res)
    %+  cook  rome
    %+  cold  ~
    ;~  plug
      sem
      (most (jest '\0a') qit)
    ==
  ::
  ++  ignored
    ;~(pose spaces comment)
  ::
  ++  special
    %+  cook  (hint %special parse-res)
    %+  cook  rome
    %+  stag  %symb
    (cook trip (mask "[]\{}'`~^@"))
  ::
  ++  string
    %+  cook  (hint %string parse-res)
    %+  rfix  [doq (rex "expected close quote" doq)]
    %+  cook  (rift |=(t/tape [%str t]))
    %-  rtar
    ;~  rose
      ;~  prix
        bas
        ;~  rose
          %+  cook  rome
          bas
          ::
          %+  cook  rome
          doq
          ::
          (rex "expected escape sequence" (cold '\0a' (jest 'n')))
          ::
          (easy [%err "expected escaped char"])
        ==
      ==
      ::
      %+  cook  rome
      ;~(ress doq bas prn)
    ==
  ::
  ++  symbol
    %+  cook  (hint %symbol parse-res)
    ;~  rose
      %+  cook  rome
      %+  cook
        |=  m/{$symb p/tape}
        ^-  mal-type
        ?:  =(p.m "nil")
          %nil
        ?:  =(p.m "true")
          %true
        ?:  =(p.m "false")
          %false
        m
      %+  stag  %symb
      %+  cook  (hint %symbol tape)  ::  type hint
      %-  plus
      ;~  pose
        (mask "!#$%&|*+-/:<=>?@^_~")
        low
        hig
        nud
      ==
    ==
  ::
  ++  atom
    %+  cook  (hint %atom parse-res)
    %+  cook  rome
    %+  stag  %atom
    ;~  pose
      (cook |=(u/@ (new:si & u)) dem)
      ;~  pfix
        hep
        (cook |=(u/@ (new:si | u)) dem)  ::  negative numbers
      ==
    ==
  ::
  ++  token
    %+  cook  (hint %token parse-res)
    ;~  rose
      ::  string parsing
      string
      ::  normal symbols
      symbol
    ==
  ++  read-str
    |=  s/tape
    ^-  parse-res
    %+  fall
      %+  trust  s
      %+  cook  (hint %read-str parse-res)
      read-form
    [%ok %nil]  ::  XX what are you supposed to return for ";; comm"?
  ::
  ++  read-list
    %+  cook  (rift |=(p/(list mal-type) [%list p]))
    %+  rfix  [pel (rex "expected )" ;~(pfix ignored per))]
    %+  cook  (hint %read-list-zing (result (list mal-type) tape))
    (rore ignored (knee *parse-res |.(read-form)))
  ::
  ++  read-vector
    %+  cook  (rift |=(p/(list mal-type) [%vect p]))
    %+  rfix  [sel (rex "expected ]" ;~(pfix ignored ser))]
    %+  cook  (hint %read-vector-zing (result (list mal-type) tape))
    (rost ignored (knee *parse-res |.(read-form)))
  ::
  ++  read-atom
    %+  cook  (hint %read-atom parse-res)
    ;~  rose
      atom
      symbol
      token
    ==
  ::
  ++  make-macro
    |=  {sym/cord mac/tape err/tape}
    %+  cook  (rift |=(m/mal-type [%list [%symb mac] m ~]))
    ;~  pfix
      (jest sym)
      ;~  rose
        (knee *parse-res |.(read-form))
        (easy [%err err])
      ==
    ==
  ::
  ++  read-macro
    ;~  rose
      (make-macro '\'' "quote" "expected quoted form")
      (make-macro '`' "quasiquote" "expected quasiquoted form")
      (make-macro '~@' "splice-unquote" "expected form to splice-unquote")
      (make-macro '~' "unquote" "expected form to unquote")
      (make-macro '@' "deref" "expected form to deref")
      ::  bleh with-meta needs two forms
    ==
  ::
  ++  read-form
    %+  cook  (hint %read-form parse-res)
    ::%+  cook  (rift flatten)
    ::%+  cook  (rift zing)
    ::%+  cook  (hint %read-form-zing (result (list mal-type) tape))
    ;~  pfix
      ignored
      ::
    ;~  rose
      read-macro
      ::
      read-list
      ::
      read-vector
      ::
      read-atom
      ::
    ==
    ==
  --
::
++  get-atom
  |=  arg/(list mal-type)
  ^-  {@s (list mal-type)}
  ?~  arg
    ~|  %empty-get-atom
    !!
  =/  first  i.arg
  ?.  ?=({$atom *} first)
    ~|  [%not-get-atom first]
    !!
  [p.first t.arg]
::
++  take
  |=  arg/(list mal-type)
  ^-  {mal-type (list mal-type)}
  ?~  arg
    ~|  %empty-take
    !!
  [i.arg t.arg]
::
:: i wrote an io monad for all this, but i lost it somewhere...
:: i re-wrote it and it worked pretty much first try, i'm basically a god
++  env
  |_  {level/(ioref _env) children/(list (ioref _env)) outer/(unit (ioref _env)) data/table}
  ::
  ++  abet  +<       ::  sample
  ::
  ++  this  ..abet   ::  core
  ::
  ++  new
    |=  {lvl/(ioref _env) outer/(unit (ioref _env)) binds/(list mal-type) exprs/(list mal-type)}
    ^-  _env
    =/  blank  ~(. env `_abet`[lvl ~ outer *table])
    |-
    ?:  &(=(~ binds) =(~ exprs))
      blank
    =^  name  binds  (take binds)
    ?>  ?=({$symb *} name)
    =^  val   exprs  (take exprs)
    $(blank (set:blank p.name val), binds binds, exprs exprs)
  ::
  ++  set
    |=  {key/tape value/mal-type}
    ^-  _this
    ~|  [%set-env in+level key+key]
    this(data (~(put by data) [b=key c=value]))
  ::
  ++  find
    |=  {envs/_(io _env) key/tape}
    ^-  (unit table)
    ~|  [%find-env in+level find+key]
    =/  res  (~(has by data) key)
    ?:  res
      (some data)
    ?~  =(outer ~)
      ~
    (find:(need (read-ioref:envs (need outer))) envs key)
  ::
  ++  get
    |=  {envs/_(io _env) key/tape}
    ^-  (unit mal-type)
    =/  e  (find envs key)
    ?~  e
      ~
    =/  val  (~(get by (need e)) key)
    ?~  val
      ~|  %got-no-key
      ~
    (some (need val))
  --
::
++  ns
  %-  my
  :~
    :-  "+"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      %-  rome  :_  this
      [%atom (sum:si a b)]
    ::
    :-  "-"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      %-  rome  :_  this
      [%atom (dif:si a b)]
    ::
    :-  "*"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      %-  rome  :_  this
      [%atom (pro:si a b)]
    ::
    :-  "/"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      %-  rome  :_  this
      [%atom (fra:si a b)]
    ::
    :-  "not"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  cond  args  (take args)
      %-  rome  :_  this
      ?:  ?=($true cond)
        %false
      ?:  ?=($false cond)
        %true
      %nil
    ::
    :-  "prn"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      =^  f  args  (take args)
      =/  str  -:(pr-str:mal f %.y)
      ::  fucking side-effect-free code, do you speak it? let's cheat.
      ~>  %slog.[1 leaf+str]
      %-  rome  :_  this
      %nil
    ::
    :-  "list"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      %-  rome  :_  this
      [%list args]
    ::
    :-  "list?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  f  args  (take args)
      %-  rome  :_  this
      ?:  ?=({$list *} f)  %true  %false
    ::
    :-  "empty?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  f  args  (take args)
      %-  rome  :_  this
      ?:  ?=({$list *} f)
        ?:  =(~ p.f)  %true  %false
      %false
    ::
    :-  "count"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  f  args  (take args)
      %-  rome  :_  this
      ?:  ?=({$list *} f)
        [%atom (sun:si (lent p.f))]
      [%atom --0]
    ::
    :-  "="  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  one  args  (take args)
      =^  two  args  (take args)
      %-  rome  :_  this
      ?:  =(one two)
        %true
      %false
    ::
    :-  "<"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      %-  rome  :_  this
      ?:  (lth one two)
        %true
      %false
    ::
    :-  "<="  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      %-  rome  :_  this
      ?:  (lte one two)
        %true
      %false
    ::
    :-  ">"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      %-  rome  :_  this
      ?:  (gth one two)
        %true
      %false
    ::
    :-  ">="  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      %-  rome  :_  this
      ?:  (gte one two)
        %true
      %false
    ::
    :-  "nil?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      %-  rome  :_  this
      ?:  ?=($nil val)
        %true
      %false
    ::
    :-  "true?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      %-  rome  :_  this
      ?:  ?=($true val)
        %true
      %false
    ::
    :-  "false?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      %-  rome  :_  this
      ?:  ?=($false val)
        %true
      %false
    ::
    :-  "symbol?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      %-  rome  :_  this
      ?:  ?=({$symb *} val)
        %true
      %false
    ::
    ::  "In step 5, if you did not add the original function (fn) to the structure returned from fn*, the you will need to do so now." ???
    ::
    :-  "apply"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  fun  args  (take args)
      ?.  ?=({$fun *} fun)
        ~|  %apply-not-fun
        !!
      =/  vals  (flatten:this args)
      (eval:this [%list (welp ~[fun] vals)])
    ::
    :-  "map"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  fun  args  (take args)
      =/  ret/(list mal-type)  ~
      =/  vals  (flatten:this args)
      |-
      ^-  mal-res
      ?~  vals
        (rome [[%list ret] this])
      ::  this is kinda really dumb
      =/  k  (eval:this [%list fun [%list [%symb "quote"] i.vals ~] ~])
      ?:  ?=({$err *} k)
        k
      =^  res  this  p.k
      ::~&  [%map-res res]
      $(ret (welp ret ~[res]), vals t.vals, this this)
    ::
    :-  "read-string"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      ~|  %need-str
      =^  str  args  (take args)
      ?>  ?=({$str *} str)
      =/  par  (read-str:parser p.str)
      ?:  ?=({$ok *} par)
        (rome [p.par this])
      ~|  [%bad-read-string par]
      !!
    ::
    :-  "slurp"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      ~|  %need-str
      =^  str  args  (take args)
      ?>  ?=({$str *} str)
      =/  urs  %+  cook
             |=(a/tape (rap 3 ^-((list @) a)))
           (star ;~(pose nud alf hep dot sig cab))
      =/  stab  =+  fel=;~(pfix fas (more fas urs))
                |=(zep/@t `path`(rash zep fel))
      =/  cont
        .^((list cord) %cx (tope [our.this %home %da now.this] (flop (stab (crip p.str)))))
      (rome [[%str (trip `@t`(role cont))] this])
    ::
    :-  "eval"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      ~|  %need-str
      =^  ast  args  (take args)
      ::  mal's scoping is fucking stupid, i'm sorry.
      ::  (do ((fn* () (def! a 1))) a) -> nil
      ::  (do ((fn* () (eval '(def! a 1)))) a) -> 1
      ::  please, explain to me why the fuck this is supposed to work

      ::  we have to let eval modify the top-level env? i think?
      ::  XX creates an ioref of 0, assumes the top-level is placed first!
      =^  res  this  (reed (eval:this(ctx 0) ast))
      (rome [res this])
    ::
    :-  "throw"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      [%err %throw val]
    ::
    :-  "atom"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      =^  token  atoms.this  (new-ioref:atoms.this val)
      ?>  ?=(@ token)
      (rome [`mal-type`[%reff `@`token] this])
    ::
    :-  "atom?"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      ?.  ?=({$reff *} val)
        (rome [%false this])
      ?>  ?=(@ p.val)
      =/  k  `(ioref mal-type)`p.val
      ?:  (~(has by refs.abet.atoms.this) k)
        (rome [%true this])
      (rome [%false this])
    ::
    :-  "deref"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      ?.  ?=({$reff *} val)
        ~|  'not an atom'
        !!
      ?>  ?=(@ p.val)
      =/  k  (need (read-ioref:atoms.this `(ioref mal-type)`p.val))
      (rome [k this])
      ::
      :-  "reset!"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  ref  args  (take args)
      =^  val  args  (take args)
      ?.  ?=({$reff *} ref)
        ~|  'not an atom'
        !!
      ?>  ?=(@ p.ref)
      =.  atoms.this  (write-ioref:atoms.this `(ioref mal-type)`p.ref val)
      (rome [val this])
    ::
    :-  "swap!"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  ref  args  (take args)
      =^  fun  args  (take args)
      ?.  ?=({$reff *} ref)
        ~|  'not an atom'
        !!
      ?>  ?=(@ p.ref)
      =/  k  (need (read-ioref:atoms.this p.ref))
      =/  ast  `mal-type`[%list (weld `(list mal-type)`~[[%list [%symb "quote"] fun ~] k] args)]
      =^  res  this  (reed (eval:this ast))
      =.  atoms.this  (write-ioref:atoms.this p.ref res)
      (rome [res this])
    ::
    :-  "cons"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =^  val  args  (take args)
      =^  lis  args  (take args)
      ?>  ?=({$list *} lis)
      (rome [[%list `(list mal-type)`[val p.lis]] this])
    ::
    :-  "concat"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =/  val/(list mal-type)  ~
      |-
      ?~  args
        (rome [[%list val] this])
      ?>  ?=({$list *} i.args)
      $(args t.args, val (weld val p.i.args))
    ::
    :-  "str"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      =/  build  ""
      |-
      ?~  args
        (rome [[%str build] this])
      ::  ?.  ?=({$str *} i.args)
      ::    ~|  %str-not-str
      ::    !!
      $(build (weld build -:(pr-str:this i.args %.n)), args t.args)
    ::
    :-  "bound"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      =/  lvl  ?~  args  0  (abs:si -:(get-atom args))
      ^-  mal-res
      %-  rome  :_  this
      [%list (turn (~(tap in ~(key by data:(need (read-ioref:envs.this lvl))))) |=(s/tape [%str s]))]
    ::
    :-  "level"  :-  %fun  :_  |  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  mal-res
      %-  rome  :_  this
      ?^  ctx.this
        ~|  'invalid ctx level'
        !!
      [%atom (sun:si `@`ctx.this)]
  ==
::
++  make-env
    ^-  _env
    =/  close/_env  (new:env 0 ~ ~ ~)
    ::  this is a bit funky looking...might be able to just reel/roll?
    =^  a  close  %^  spin  (~(tap by ns))
        |=  {{sym/tape val/mal-type} close/_env}
        [~ (set:close sym val)]
      close
    close
::
++  mal
  |_  {envs/_(io _env) atoms/_(io mal-type) ctx/(ioref _env) our/@p now/@da}
  ::  (aka poor man's io monad)
  ++  abet  +<
  ::
  ++  this  .                                           ::  entire core
  ::
  ++  set
    |=  {token/(ioref _env) bind/tape val/mal-type}
    ^-  _this
    =/  fenv  (need (read-ioref:envs token))
    =.  fenv  (set:fenv bind val)
    =.  envs.this  (write-ioref:envs token fenv)
    this
  ::
  ++  new
    |=  {our/@p now/@da}
    =/  envs/_(io _env)  (io _env)
    =^  top/(ioref _env)  envs  (new-ioref:envs make-env)
    =/  machine/_mal  ~(. mal envs (io mal-type) top our now)
    =/  preload
      %-  limo
      :~
        "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
        "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
        "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"
      ==
    machine
    ::|-
    ::?~  preload
    ::  machine
    ::$(machine +:(reed (rep:machine i.preload)), preload t.preload)
  ::
  ::  because all of these result into a [(some res] this], not actually TCO
  ::  which is probably a bad thing?
  ::  turn this into a (map tape $-((list mal-type) mal-res) and become in ++eval
  ::
  +-  special-apply
    |*  {s/mal-type f/_|.(*mal-res)}
    ^-  mal-res
    ?>  ?=({$list *} s)
    ?<  ?=($~ p.s)
    =/  args  `(list mal-type)`p.s
    =^  sym  args  (take `(list mal-type)`args)
    ?>  ?=({$symb *} sym)
    =/  prim  p.sym
    ::
    ?:  =(prim "try*")
      =^  code   args  (take args)
      =^  catch  args  (take args)
      ?.  &(?=({$list *} catch) ?=(^ p.catch) ?=({$symb *} i.p.catch) =(p.i.p.catch "catch*"))
        ~|  %try-not-catch
        !!
      =^  bind  t.p.catch  (take t.p.catch)
      =^  hand  t.p.catch  (take t.p.catch)
      ::  magic happens
      =/  safe  (mule |.((eval code)))
      ::  XX shit, do i actually need (result {mal-type _this} {mal-res this})?
      ::  this will lose writes to atoms inside the catch, which it might not expect
      ?:  ?=($| -.safe)
        ::  actually crashed
        ::  XX why the fuck am i still using reed for all these special forms smh
        %-  eval:this
          ^-  mal-type
          :*
            %list
            :~
              [%symb "let*"]
              [%list bind [%list (turn p.safe |=(b/tank [%str ~(ram re b)]))] ~]
              hand
            ==
          ==
      ?:  ?=({$err *} p.safe)
        ?:  ?=({$throw *} p.p.safe)
          :: this is cheating
          (eval:this [%list ~[[%symb "let*"] [%list bind p.p.p.safe ~] hand]])
        ::  usually looks terrible
        (eval:this [%list ~[[%symb "let*"] [%list bind [%str <p.p.safe>] ~] hand]])
      ::  actually good value
      p.safe
    ::
    ?:  =(prim "def!")
      ::  XX if overwriting a function, clear its env from the envs table
      =^  bind  args  (take args)
      =^  code  args  (take args)
      =^  res/mal-type  this  (reed (eval `mal-type`code))
      ?:  =(%nil res)
        ~|  %bad-bind
        %-  rome
        [%nil this]
      =/  key  ?>  ?=({$symb *} bind)
               p.bind
      ::  ~&  [%def-in ctx key]
      %-  rome
      [res (set ctx key res)]
    ::
    ?:  =(prim "defmacro!")
      ::  "This is very similar to the def! form, but before the evaluated value (mal function) is set in the environment, the is_macro attribute should be set to true."
      ::  you'll note that explanation does not, in fact, make sense
      =^  bind  args  (take args)
      =^  code  args  (take args)
      =^  res/mal-type  this  (reed (eval `mal-type`code))
      ?:  =(%nil res)
        ~|  %bad-macro
        %-  rome
        [%nil this]
      ?.  ?=({$fun *} res)
        ~|  %macro-not-fun
        !!
      =.  mac.res  %.y
      =/  key  ?>  ?=({$symb *} bind)
               p.bind
      ::  ~&  [%def-in ctx key]
      %-  rome
      [res (set ctx key res)]
    ::
    ?:  =(prim "let*")
      =^  bindings  args  (take args)
      ?>  ?=({$list *} bindings)
      =/  bindings/(list mal-type)  p.bindings
      =/  old-env  ctx
      ::
      =/  new-env  (new:env level=0 (some ctx) ~ ~)
      =^  token  envs  (new-ioref:envs new-env)
      =.  level.new-env  token
      =.  envs.this  (write-ioref:envs token new-env)
      ::
      |-
      ?.  =((lent bindings) 0)
        =^  name/mal-type  bindings  (take bindings)
        ?>  ?=({$symb *} name)
        =^  entr  bindings  (take bindings)
        =^  bind  this  (reed (eval entr))
        ?:  =(%nil bind)
          ~&  %bad-let
          $(this this, bindings bindings)
        ::~&  [%let-bind p.name bind]
        =.  this  (set ctx p.name bind)
        $(this this, bindings bindings)
      =^  close  args  (take args)
      =^  res    this  (reed (eval close))
      %-  rome
      [res this(ctx old-env)]
    ::
    ?:  =(prim "do")
      =/  args/(list mal-type)  t.p.s
      =/  f
        |=  {p/mal-type q/_mal}
        ^-  {mal-type _this}
        (reem (eval:q p))
      ::  XX i thought it returned every elem, only actually need a reel
      =^  res  this  (spin args f `_mal`this)
      =+  (rep:this "(prn (bound) )")
      %-  rome
      [(snag (dec (lent res)) res) this]
    ::
    ?:  =(prim "if")
      =^  cond  args  (take args)
      =^  tru   args  (take args)
      ::=^  res   this  (reed (eval cond))
      =^  res  this  (reem (eval cond))
      ?.  ?=(?($nil $false) res)
        (eval tru)
      ?~  args
        %-  rome
        [%nil this]
      (eval i.args)
    ::
    ?:  =(prim "fn*")
      =^  param  args  (take args)
      ~|  param
      ?>  ?=(?({$list *} {$vect *}) param)
      =^  close  args  (take args)
      =/  token  ctx
      %-  rome
      :_  this
      :-  %fun  :_  |
        ^-  mal-lambda
        ^?
        |=  {args/(list mal-type) this/_mal}
        ^-  mal-res
        ::~&  [%dig-for token %from ctx.this]
        ::~&  [%lamb-call args]
        :: XX add a GC that scans references? this create envs and never destroy
        =/  cont  ctx.this
        =/  new-env  (new:env level=0 (some token) p.param args)
        =^  level  envs.this  (new-ioref:envs.this new-env)
        =.  new-env  new-env(level level)
        =.  envs.this  (write-ioref:envs.this level new-env)
        ::
        ::~&  [%run-in ctx+level abet+abet.new-env]
        ::  XX bubble error from _(eval) on all of these =^  res things
        =^  res  this  (reem (eval:this(ctx level) close))
        (rome [res this(ctx cont)])
    ::
    ?:  =(prim "quote")
      %-  rome
      ?~  args
        [%nil this]
      ?:  =((lent args) 1)
        [i.args this]
      [[%list args] this]
    ::
    :: XX not TCO? is it?
    ?:  =(prim "quasiquote")
      =/  qot
        =/  is-pair
          |=  val/mal-type
          ?.  ?=({$list *} val)
            %.n
          ?~  p.val  ::  >called is-pair >actually tests if nonempty
            %.n
          %.y
        ::  ast is a %list
        =^  ast  args  (take args)
        |-
        ^-  mal-type
        ::  ~&  [%qq ast]
        ?.  (is-pair ast)
          ::  ~&  [%qq-list ast]
          [%list ~[[%symb "quote"] ast]]
        ?>  ?=({$list *} ast)
        =^  first  p.ast  (take p.ast)
        ?:  &(?=({$symb *} first) =("unquote" p.first))
          ::  ~&  [%qq-unquote ast]
          =^  second  p.ast  (take p.ast)
          second
        ::=^  first  ast  (take `(list mal-type)`ast)
        ?:  ?&
            (is-pair first)
            ?=({$list *} first)
            ?=(^ p.first)
            ?=({$symb *} i.p.first)
            =("splice-unquote" p.i.p.first)
            ==
          ::  ~&  [%qq-splice p.ast]
          =/  a  i.p.first
          =/  first  first(p t.p.first)
          =^  b  p.first  (take p.first)
          [%list ~[[%symb "concat"] b $(ast ast)]]
        ::  ~&  [%qq-cons first ast]
        [%list ~[[%symb "cons"] $(ast first) $(ast ast)]]
      ::  ~&  [%qq-res qot]
      (eval qot)
    ::
    ?:  =(prim "macroexpand")
      =^  ast  args  (take args)
      %-  rome
      (macroexpand ast (need (read-ioref:envs ctx)))
    ::
    (f)
  ::
  ++  eval-ast
    |=  ast/mal-type
    ^-  mal-res
    ::  ~&  [%eval -:(pr-str ast &)]
    ?-  ast
      {$symb *}  =/  key
                   |-
                   ~|  [%eval-in ctx+ctx key+p.ast]
                   ~|  [%alive-envs (turn (~(tap by refs.abet.envs)) |=({key/(ioref _env) val/_env} key))]
                   (get:(need (read-ioref:envs ctx)) envs p.ast)
                 ?~  key
                   (rome [%nil this])
                 (rome [(need key) this])
      ::
      {$list *}  =/  res/(list mal-type)  ~
                 |-
                 ?~  p.ast
                   (rome [[%list res] this])
                 =/  e  (eval:this i.p.ast)
                 ?:  ?=({$err *} e)
                   e
                 =^  r  this  p.e
                 $(this this, res (welp res ~[r]), p.ast t.p.ast)
      ::
      *          (rome [ast this])
    ==
  ::
  ++  eval
    |=  s/mal-type
    ^-  mal-res
    ?.  ?=({$list *} s)
      (eval-ast s)
    ::
    ?:  =((lent p.s) 0)
      (rome [s this])
    ::
    =^  s  this  (macroexpand s (need (read-ioref:envs ctx)))
    ?.  ?=({$list *} s)
      (eval-ast s)

    ::  continuation style so that special-apply can continue if not a form
    ::    i could just put the remainder at the end of the case check, but it
    ::    would be out of place
    =-  ?:  ?&
          !?=($~ p.s)

          =/  args  `(list mal-type)`p.s
          =^  sym  args  (take `(list mal-type)`args)
          ?=({$symb *} sym)
        ==
        (special-apply s continue)
      (continue)
    ::
    :: get new list
    ^-  continue/_|.(*mal-res)
    |.
    %+  riff  (eval-ast `mal-type`s)
    |=  {el/mal-type this/_mal}
    ?>  ?=({$list *} el)
    ?~  p.el
      [%err %empty-call]
    =/  func  `mal-type`i.p.el
    ?.  ?=({$fun *} func)
      [%err %eval-bad-func s]
    =/  fun/mal-lambda  p.func
    =/  param/(list mal-type)  t.p.el
    =/  c/{(list mal-type) _mal}  [param this]
    (fun c)
  ::
  ++  print
    |=  s/mal-type
    ^-  {tape _this}
    (pr-str s %.y)
  ++  pr-str
    |=  {s/mal-type readable/?}
    ^-  {tape _this}
    :_  this
    ?-  s
      $nil       "nil"
      ::
      $true      "true"
      ::
      $false     "false"
      ::
      {$list *}  :(weld "(" (roll (turn p.s |=(m/mal-type -:(print m))) |=({a/tape b/tape} ?~(b a :(weld b " " a)))) ")")
      ::
      {$vect *}  :(weld "[" (roll (turn p.s |=(m/mal-type -:(print m))) |=({a/tape b/tape} ?~(b a :(weld b " " a)))) "]")
      ::
      {$atom *}  =/  o/{? @}  (old:si p.s)
                 %+  weld  ?:(-.o "" "-")
                 =/  p  +.o
                 ::  tfw your test fails because numbers are german-style 1.010
                 (flop `tape`|-((weld <(mod p 10)> ?~((div p 10) "" $(p (div p 10))))))
      ::
      {$reff *}  (weld "$" <p.s>)
      ::
      {$symb *}  p.s
      ::
      {$str *}   ?.  readable
                   p.s
                 =/  build  ""
                 |-
                 ?~  p.s
                   :(weld "\"" build "\"")
                 ?:  =('"' i.p.s)
                   $(build (weld build "\\\""), p.s t.p.s)
                 ?:  =('\0a' i.p.s)
                   $(build (weld build "\\n"), p.s t.p.s)
                 ?:  =('\\' i.p.s)
                   $(build (weld build "\\\\"), p.s t.p.s)
                 $(build (weld build (trip i.p.s)), p.s t.p.s)
      ::
      {$fun *}  "###"
    ==
  ::
  ++  flatten
    |=  args/(list mal-type)
    ^-  (list mal-type)
    ?~  args
      ~
    =/  k  i.args
    ?:  ?=({$list *} k)
      (weld $(args p.k) $(args t.args))
    (welp ~[k] $(args t.args))
  ::
  ++  is-macro-call
    |=  {ast/mal-type env/_env}
    ?.  ?=({$list *} ast)
      %.n
    ?:  ?=($~ p.ast)
      %.n
    ?.  ?=({$symb *} i.p.ast)
      %.n
    =/  val  (get:env envs p.i.p.ast)
    ?~  val
      %.n
    ?.  ?=({$fun *} u.val)
      %.n
    =(mac.u.val %.y)
  ::
  ++  macroexpand
    |=  {ast/mal-type env/_env}
    ^-  {mal-type _this}
    |-
    ?:  (is-macro-call ast env)
      ::  ~&  [%macro-expand -:(pr-str ast &)]
      ?.  &(?=({$list *} ast) ?=(^ p.ast) ?=({$symb *} i.p.ast))
        ~|  %you-did-what-now
        !!
      =/  mac  (need (get:env envs p.i.p.ast))
      ?.  ?=({$fun *} mac)
        ~|  %macro-expand-not-macro
        !!
      =/  fun/mal-lambda  p.mac
      =^  res  this  (reem (fun [`(list mal-type)`t.p.ast this]))
      $(ast res, this this)
    [ast this]
  ::
  ++  reem
    |*  r/_(eval)
    ?:  ?=({$err *} r)
      ~|  [%reem r]
      !!
    +.r
  ::
  ++  rep
    |=  s/tape
    ^-  (result {tape _this} eval-err)
    =/  par/parse-res     (read-str:parser s)
    ?:  ?=({$err *} par)
      [%err %parse p.par]
    ::%+  riff  par         |=  ast/mal-type
    %+  riff  (eval p.par)  |=  res/{mal-type _mal}
    =^  rest  this  res
    =^  prit  this  (print rest)
    (rome [prit this])
  ::
  ++  safe-rep
    |=  s/tape
    ^-  (result {tape _this} ?(eval-err {$safe-fail (list tank)}))
    =/  safe
      ::  runtime panic = $err $safe-fail
      ::  runtime error = $err $eval-err
      ::  fine = $ok ...
      ::  mule shadoes the sky! use ++mock with |=({a/* b/*} ``.^(* b)) for praying
      (mule |.((rep s)))
    ?:  ?=($| -.safe)
      [%err [%safe-fail +.safe]]
    p.safe
  --
--
