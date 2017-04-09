/-    mal

!:
::  this is really weird and i don't get why it's not just =+  mal  again
=+  [. ^mal]
|%
::  list monad, not sure why this isn't standard when we have zing
++  zap
  |*  a/*
  ^-  (list _a)
  [a ~]
::
++  hint
  |*  {msg/cord han/mold}
  |=  fud/*  ^-  han
  ~|  hint+msg
  =+  gol=(han fud)
  ?>(=(gol fud) gol)
::
++  dy-cast
  |*  {typ/mold bun/vase}
  |=  a/vase  ^-  typ
  ~|  [p.bun p.a]
  ?>  (~(nest ut p.bun) & p.a)
  ;;(typ q.a)
::
++  shave
  |=  p/tape
  (slag 1 (scag (dec (lent p)) p))
::
++  parser
  |%
  ++  spaces
    %+  cold  ~
    %-  plus
    ;~  pose
      ace
      gap
      com
    ==
  ::
  ++  comment
    %+  cold  ~
    ;~  plug
      sem
      (most (jest '\0a') qit)
    ==
  ::
  ++  special
    %+  stag  %symb
    (cook trip (mask "[]\{}'`~^@"))
  ::
  ++  string
    %+  cook  (hint %string mal-type)
    %+  stag  %str
    %+  ifix  [doq doq]
    %-  star
    ;~  pose
      ;~(pfix bas ;~(pose bas doq bix:ab))
      ;~(less doq bas prn)
    ==
  ::
  ++  symbol
    %+  stag  %symb
    %+  cook  (hint %symbol tape)  ::  type hint for pretty-printing
    %-  plus
    ;~  pose
      (mask "!#$%&|*+-/:<=>?@^_~")
      low
      hig
      nud
    ==
  ::
  ++  atom
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
    %+  cook  (hint %token mal-type)
    ::  %+  stag  %list
    ::  %-  plus
    ;~  pose
      ::  ignore leading whitespace
      spaces
      ::  special token
      %+  stag  %symbol
      (cook trip (jest '~@'))
      ::  special characters
      special
      ::  re-use hoon's string parser for "abc"
      string
      ::  any amount of characters after ;
      comment
      ::  normal symbols
      symbol
    ==
  ++  read-str
    |=  s/tape
    ^-  (unit mal-type)
    %+  rust  s
    %+  cook
      |=  m/mal-type
      ?:  ?=({$list *} m)
        ?:  =((lent p.m) 1)
          (snag 0 p.m)
        m
      m
    read-form
  ::
  ++  flatten
    |=  out/(list mal-type)
      ?:  =((lent out) 1)
        (snag 0 out)
      [%list out]
  ::
  ++  read-list
    %+  cook  (hint %read-list mal-type)
    %+  ifix  [pel per]
    %+  cook  zing
    (more ace (knee *mal-type |.(read-form)))
  ::
  ++  read-atom
    %+  cook  (hint %read-atom mal-type)
    ;~  pose
      atom
      symbol
      token
    ==
  ::
  ++  read-form
    %+  cook  (hint %read-form mal-type)
    ::%+  cook  flatten
    %+  stag  %list
    %+  cook  zing
    %-  plus
    ;~  pose
      spaces
      ::
      (cook zap read-list)
      ::
      (cook zap read-atom)
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
++  env
  |_  {outer/(unit _env) data/table}
  ::
  ++  abet  +<       ::  sample
  ::
  ++  this  ..abet   ::  core
  ::
  ++  new
    |=  outer/(unit _env)
    ^-  _env
    ~(. env [outer *table])
  ::
  ++  set
    |=  {key/tape value/mal-type}
    ^-  _this
    this(data (~(put by data) [b=key c=value]))
  ::
  ++  find
    |=  key/tape
    ^-  (unit table)
    =/  res  (~(has by data) key)
    ?:  res
      (some data)
    ?~  =(outer ~)
      ~
    (find:(need outer) key)
  ::
  ++  get
    |=  key/tape
    ^-  (unit mal-type)
    =/  e  (find key)
    ?~  e
      ~
    =/  val  (~(get by (need e)) key)
    ?~  val
      ~|  %got-no-key
      ~
    (some (need val))
  --
  ::
++  make-env
    ^-  _env
    =/  close/_env  (new:env ~)
    =.  close  %+  set:close  "+"
      :-  %fun
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (sum:si a b)]
    =.  close  %+  set:close  "-"
      :-  %fun
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (dif:si a b)]
    ::
    =.  close  %+  set:close  "*"
      :-  %fun
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (pro:si a b)]
    ::
    =.  close  %+  set:close  "/"
      :-  %fun
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (fra:si a b)]
    close
    ::
::
++  mal              ::  this is long overdue;
  |_  ctx/_env       ::  hoon doesn't have an io monad, but state machines
  ++  abet  +<       ::  sample"
  ::
  ++  this  ..abet   ::  entire core
  ::
  ++  new
    ~(. mal make-env)
  ::
  ++  read
    |=  s/tape
    ^-  {mal-type _this}
    :_  this
    (need (read-str:parser s))
  ::
  ++  special-apply
    |=  s/mal-type
    ^-  {(unit mal-type) _this}
    ?.  ?=({$list *} s)
      [~ this]
    ?~  p.s
      [~ this]
    ?.  ?=({$symb *} i.p.s)
      [~ this]
    =/  prim  p.i.p.s
    ?:  =(prim "def!")
      =^  res/mal-type  this  (eval `mal-type`(snag 2 `(list mal-type)`p.s))
      ?:  =(%nil res)
        ~|  %bad-bind
        [(some %nil) this]
      =/  key  =+  a=(snag 1 `(list mal-type)`p.s)
               ?>  ?=({$symb *} a)
               p.a
      ::=.  ctx  (set:ctx `tape`key `mal-type`res)
      [(some res) this(ctx `_env`(set:ctx key res))]
    ::
    ?:  =(prim "let*")
      =/  args  `(list mal-type)`t.p.s
      =/  bindings  (snag 0 args)
      ?>  ?=({$list *} bindings)
      =/  bindings/(list mal-type)  p.bindings
      =/  old-env  ctx
      =.  ctx  (new:env (some ctx))
      =.  ctx
      |-
        ?:  =((lent bindings) 0)
          ctx
        =/  name/mal-type  (snag 0 bindings)
        ?>  ?=({$symb *} name)
        =^  bind  this  (eval (snag 1 bindings))
        ?:  =(%nil bind)
          ~&  %bad-let
          $(bindings (slag 2 bindings))
        ::~&  [%let-bind p.name bind]
        $(ctx (set:ctx p.name bind), bindings (slag 2 bindings))
      =/  close  (snag 1 args)
      =^  res  this  (eval close)
      [(some res) this(ctx old-env)]
    ::
    [~ this]
  ::
  ++  eval-ast
    |=  ast/mal-type
    ^-  {mal-type _this}  ::  no i/o monad, only forwards type inference
    ?-  ast
      {$symb *}  =/  key  (get:ctx p.ast)
                 ?~  key
                   [%nil this]
                 [(need key) this]
      ::
      {$list *}  =/  fire/(list mal-type)  p.ast
                 =/  acc/(list mal-type)  ~
                 |-
                 ?~  fire
                   [[%list acc] this]
                 =^  res/mal-type  this  (eval i.fire)
                 $(acc (weld acc (zap res)), fire t.fire)
      ::
      *          [ast this]
    ==
  ::
  ++  eval
    |=  s/mal-type
    ^-  {mal-type _this}
    ?.  ?=({$list *} s)
      (eval-ast s)
    ::
    ?:  =((lent p.s) 0)
      [s this]
    ::
    =/  spec  (special-apply s)
    ?^  -.spec
      =^  val  this  spec
      [(need val) this]
    ::
    :: get new list
    =^  el/mal-type  this  (eval-ast `mal-type`s)
    ?>  ?=({$list *} el)
    ?~  p.el
      ~|  %empty-call
      !!
    =/  func  `mal-type`i.p.el
    ?.  ?=({$fun *} func)
      [%nil this]
    =/  fun/mal-lambda  p.func
    =/  param/(list mal-type)  t.p.el
    [(fun param) this]
  ::
  ++  print
    |=  s/mal-type
    ^-  {tape _this}
    :_  this
    ?-  s
      $nil       "nil"
      ::
      {$list *}  :(weld "(" (roll (turn p.s |=(m/mal-type -:(print m))) |=({a/tape b/tape} ?~(b a :(weld b " " a)))) ")")
      ::
      {$atom *}  =/  o/{? @}  (old:si p.s)
                 %+  weld  ?:(-.o "" "-")
                 =/  p  +.o
                 ::  tfw your test fails because numbers are german-style 1.010
                 (flop `tape`|-(?~(p "" (weld <(mod p 10)> $(p (div p 10))))))
      ::
      {$symb *}  p.s
      ::
      {$str *}   p.s
      ::
      {$fun *}  "PRINT_FUN"
    ==
  ::
  ++  rep
    |=  s/tape
    ^-  {tape _this}
    =^  par   this  (read s)
    =^  res   this  (eval par)
    =^  prit  this  (print res)
    [prit this]
  ::
  ++  safe-rep
    |=  c/vase
    ^-  (unit vase)
    =/  safe
      (mule |.((slap c (ream '(rep s)'))))
    ?:  ?=($| -.safe)
      ~
    (some `vase`p.safe)
  --
--
