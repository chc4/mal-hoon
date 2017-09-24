/-    mal
/+    help

::  !:
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
  {$symb p/tape}
  {$str p/tape}
  {$fun p/mal-lambda}
==
==
::
++  eval-err
    $@
      $?
      $empty-call
      $special-fail
      ==
    $%
      {$eval-bad-func mal-type}
    ==
++  table
  (map tape mal-type)
::
++  mal-lambda
  ::$-({(list mal-type) _mal} {mal-type _mal})
  _^?(|=({(list mal-type) _mal} *{mal-type _mal}))
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
      %+  cold  [%ok %nil]
        (jest 'nil')
      ::
      %+  cold  [%ok %true]
        (jest 'true')
      ::
      %+  cold  [%ok %false]
        (jest 'false')
      ::
      %+  cook  rome
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
    :-  "+"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      :_  this
      [%atom (sum:si a b)]
    ::
    :-  "-"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      :_  this
      [%atom (dif:si a b)]
    ::
    :-  "*"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      :_  this
      [%atom (pro:si a b)]
    ::
    :-  "/"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  a  args  (get-atom args)
      =^  b  args  (get-atom args)
      :_  this
      [%atom (fra:si a b)]
    ::
    :-  "prn"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      =^  f  args  (take args)
      =/  str  -:(pr-str:mal f %.y)
      ::  fucking side-effect-free code, do you speak it? let's cheat.
      ~>  %slog.[1 leaf+str]
      :_  this
      %nil
    ::
    :-  "list"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      :_  this
      [%list args]
    ::
    :-  "list?"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  f  args  (take args)
      :_  this
      ?:  ?=({$list *} f)  %true  %false
    ::
    :-  "empty?"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  f  args  (take args)
      :_  this
      ?:  ?=({$list *} f)
        ?:  =(~ p.f)  %true  %false
      %false
    ::
    :-  "count"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  f  args  (take args)
      :_  this
      ?:  ?=({$list *} f)
        [%atom (sun:si (lent p.f))]
      [%atom --0]
    ::
    :-  "="  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  one  args  (take args)
      =^  two  args  (take args)
      :_  this
      ?:  =(one two)
        %true
      %false
    ::
    :-  "<"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      :_  this
      ?:  (lth one two)
        %true
      %false
    ::
    :-  "<="  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      :_  this
      ?:  (lte one two)
        %true
      %false
    ::
    :-  ">"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      :_  this
      ?:  (gth one two)
        %true
      %false
    ::
    :-  ">="  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =^  one  args  (get-atom args)
      =^  two  args  (get-atom args)
      :_  this
      ?:  (gte one two)
        %true
      %false
    ::
    :-  "read-string"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      ~|  %need-str
      =^  str  args  (take args)
      ?>  ?=({$str *} str)
      (read:this p.str)
    ::
    :-  "slurp"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
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
      [[%str (trip `@t`(role cont))] this]
    ::
    :-  "eval"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      ~|  %need-str
      =^  ast  args  (take args)
      ::  mal's scoping is fucking stupid, i'm sorry.
      ::  (do ((fn* () (def! a 1))) a) -> nil
      ::  (do ((fn* () (eval '(def! a 1)))) a) -> 1
      ::  please, explain to me why the fuck this is supposed to work

      ::  we have to let eval modify the top-level env? i think?
      ::  XX creates an ioref of 0, assumes the top-level is placed first!
      =^  res  this  (reed (eval:this(ctx 0) ast))
      [res this]
    ::
    :-  "str"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      =/  build  ""
      |-
      ?~  args
        [[%str build] this]
      ?.  ?=({$str *} i.args)
        ~|  %str-not-str
        !!
      $(build (weld build p.i.args), args t.args)
    ::
    :-  "bound"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      =/  lvl  ?~  args  0  (abs:si -:(get-atom args))
      ^-  {mal-type _this}
      :_  this
      [%list (turn (~(tap in ~(key by data:(need (read-ioref:envs.this lvl))))) |=(s/tape [%str s]))]
    ::
    :-  "level"  :-  %fun  ^?
      |=  {args/(list mal-type) this/_mal}
      ^-  {mal-type _this}
      :_  this
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
    =/  r  (rep:machine "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))")
    +:(reed r)
  ::
  ++  read
    |=  s/tape
    ^-  {mal-type _this}
    :_  this
    (reed (read-str:parser s))
  ::
  ++  special-apply
    |=  s/mal-type
    ^-  {(unit mal-type) _this}
    ?.  ?=({$list *} s)
      [~ this]
    ?~  p.s
      [~ this]
    =/  args  `(list mal-type)`p.s
    =^  sym  args  (take `(list mal-type)`args)
    ?.  ?=({$symb *} sym)
      [~ this]
    =/  prim  p.sym
    ::
    ?:  =(prim "def!")
      ::  XX if overwriting a function, clear its env from the envs table
      =^  bind  args  (take args)
      =^  code  args  (take args)
      =^  res/mal-type  this  (reed (eval `mal-type`code))
      ?:  =(%nil res)
        ~|  %bad-bind
        [(some %nil) this]
      =/  key  ?>  ?=({$symb *} bind)
               p.bind
      ::  ~&  [%def-in ctx key]
      [(some res) (set ctx key res)]
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
      [(some res) this(ctx old-env)]
    ::
    ?:  =(prim "do")
      =/  args/(list mal-type)  t.p.s
      =/  f
        |=  {p/mal-type q/_mal}
        ^-  {mal-type _mal}
        (reed (eval:q p))
      ::  XX i thought it returned every elem, only actually need a reel
      =^  res  this  (spin args f `_mal`this)
      =+  (rep:this "(prn (bound) )")
      [(some (snag (dec (lent res)) res)) this]
    ::
    ?:  =(prim "if")
      =^  cond  args  (take args)
      =^  tru   args  (take args)
      ::=^  res   this  (reed (eval cond))
      =/  res   -:(reed (eval cond))
      ?.  ?=(?($nil $false) res)
        =^  tr  this  (reed (eval tru))
        [(some tr) this]
      ?~  args
        [(some %nil) this]
      =^  fal  this  (reed (eval i.args))
      [(some fal) this]
    ::
    ?:  =(prim "fn*")
      =^  param  args  (take args)
      ~|  param
      ?>  ?=(?({$list *} {$vect *}) param)
      =^  close  args  (take args)
      =/  token  ctx
      :_  this
      %-  some
      :-  %fun
        ^-  mal-lambda
        ^?
        |=  {args/(list mal-type) this/_mal}
        ^-  {mal-type _mal}
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
        =^  res  this  (reed (eval:this(ctx level) close))
        [res this(ctx cont)]
    ::
    ?:  =(prim "quote")
      ?~  args
        [(some %nil) this]
      ?:  =((lent args) 1)
        [(some i.args) this]
      [(some [%list args]) this]
    ::
    [~ this]
  ::
  ++  eval-ast
    |=  ast/mal-type
    ^-  {mal-type _this}
    ?-  ast
      {$symb *}  =/  key
                   |-
                   ~|  [%eval-in ctx+ctx key+p.ast]
                   ~|  [%alive-envs (turn (~(tap by refs.abet.envs)) |=({key/(ioref _env) val/_env} key))]
                   (get:(need (read-ioref:envs ctx)) envs p.ast)
                 ?~  key
                   [%nil this]
                 [(need key) this]
      ::
      {$list *}  =/  f
                   |=  {m/mal-type s/_mal}
                   ^-  {mal-type _mal}
                     =/  e  (eval:s m)
                     ~|  [%eval-ast-list m e]
                     (reed e)
                 =^  res  this  (spin `(list mal-type)`p.ast f `_mal`this)
                 [[%list res] this]
      ::
      *          [ast this]
    ==
  ::
  ++  eval
    |=  s/mal-type
    ^-  (result {mal-type _this} eval-err)
    ?.  ?=({$list *} s)
      %-  rome
      (eval-ast s)
    ::
    ?:  =((lent p.s) 0)
      (rome [s this])
    ::
    =/  spec  (special-apply s)
    ?^  -.spec
      =^  val  this  spec
      ^-  (result {mal-type _this} eval-err)
      %+  rath
        `(result mal-type eval-err)`(rung %special-fail val)
        `(result _this eval-err)`(rome this)
        ::(rome this)
    ::
    :: get new list
    =^  el/mal-type  this  (eval-ast `mal-type`s)
    ?>  ?=({$list *} el)
    ?~  p.el
      [%err %empty-call]
    =/  func  `mal-type`i.p.el
    ?.  ?=({$fun *} func)
      [%err %eval-bad-func func]
    =/  fun/mal-lambda  p.func
    =/  param/(list mal-type)  t.p.el
    =/  c/{(list mal-type) _mal}  [param this]
    (rome (fun c))
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
      {$symb *}  p.s
      ::
      {$str *}   ?.  readable
                   p.s
                 =/  build  ""
                 |-
                 ?~  p.s
                   :(weld "'" build "'")
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
  ++  rep
    |=  s/tape
    ^-  (result {tape _this} eval-err)
    =^  par   this  (read s)
    %+  riff  (eval par)  |=  res/{mal-type _mal}
    =^  rest   this  res
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
