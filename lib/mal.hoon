/-    mal
/+    help

!:
::  this is really weird and i don't get why it's not just =+  mal  again
=+  [help . ^mal]
::
|%
++  parser
  |%
  ++  spaces
    %+  cook  rome
    %+  cold  ~
    %-  plus
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
          (rex "expected escape sequence" bix:ab)
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
    ::  %+  stag  %list
    ::  %-  plus
    ;~  rose
      ::  ignore leading whitespace
      spaces
      ::  special token
      %+  cook  rome
      %+  stag  %symbol
      (cook trip (jest '~@'))
      ::  special characters
      special
      ::  string parsing
      string
      ::  any amount of characters after ;
      comment
      ::  normal symbols
      symbol
    ==
  ++  read-str
    |=  s/tape
    ^-  parse-res
    %-  need
    %+  trust  s
    %+  cook  (hint %read-str parse-res)
    %+  cook
      %-  rift
      |=  m/mal-type
      ^-  mal-type
      ?:  ?=({$list *} m)
        ?:  =((lent p.m) 1)
          (snag 0 p.m)
        m
      m
    %+  cook  (hint %read-str-form parse-res)
    read-form
  ::
  ++  flatten
    |=  out/(list mal-type)
      ?:  =((lent out) 1)
        (snag 0 out)
      [%list out]
  ::
  ++  read-list
    %+  cook  (hint %read-list parse-res)
    ::  (list parse-res)
    %+  rfix  [pel (rex "expected )" per)]
    %+  cook  (rift zing)
    %+  cook  (hint %read-list-zing (result (list mal-type) tape))
    (rore ace (knee *parse-res |.(read-form)))
  ::
  ++  read-atom
    %+  cook  (hint %read-atom parse-res)
    ;~  rose
      atom
      symbol
      token
    ==
  ::
  ++  read-form
    %+  cook  (hint %read-form parse-res)
    %+  cook  (rift flatten)
    %+  cook  (rift zing)
    %+  cook  (hint %read-form-zing (result (list (list mal-type)) tape))
    %-  rlus
    ;~  rose
      spaces
      ::
      (cook (rift |=(p/mal-type (zang p))) read-list)
      ::
      (cook (rift |=(p/mal-type (zang p))) read-atom)
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
    (reed (read-str:parser s))
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
    ::
    ?:  =(prim "def!")
      =^  res/mal-type  this  (reed (eval `mal-type`(snag 2 `(list mal-type)`p.s)))
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
        =^  bind  this  (reed (eval (snag 1 bindings)))
        ?:  =(%nil bind)
          ~&  %bad-let
          $(bindings (slag 2 bindings))
        ::~&  [%let-bind p.name bind]
        $(ctx (set:ctx p.name bind), bindings (slag 2 bindings))
      =/  close  (snag 1 args)
      =^  res  this  (reed (eval close))
      [(some res) this(ctx old-env)]
    ::
    ?:  =(prim "do")
      =/  args/(list mal-type)  t.p.s
      =/  f
        |=  {p/mal-type q/_mal}
        ^-  {mal-type _mal}
        (reed (eval:q p))
      =^  res  this  (spin args f `_mal`this)
      [(some [%list res]) this]
    ::
    [~ this]
  ::
  ++  eval-ast
    |=  ast/mal-type
    ^-  {mal-type _this}
    ?-  ast
      {$symb *}  =/  key  (get:ctx p.ast)
                 ?~  key
                   [%nil this]
                 [(need key) this]
      ::
      {$list *}  =/  f
                   |=  {m/mal-type s/_mal}
                   ^-  {mal-type _mal}
                     (reed (eval:s m))
                 =/  res  (spin `(list mal-type)`p.ast f `_mal`this)
                 [[%list -.res] this]
      ::
      *          [ast this]
    ==
  ::

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
      %+  rath
        (rung %special-fail val)
        ::`(result _this eval-err)`(rome this)
        (rome this)
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
    (rome [(fun param) this])
  ::
  ++  print
    |=  s/mal-type
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
    |=  c/vase
    ^-  (unit vase)
    =/  safe
      (mule |.((slap c (ream '(rep s)'))))
    ?:  ?=($| -.safe)
      ~
    (some `vase`p.safe)
  --
--
