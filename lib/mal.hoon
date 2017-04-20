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
    ::  %+  stag  %list
    ::  %-  plus
    ;~  pfix
      ::  ignore leading whitespace
      spaces
    ;~  rose
      ::  string parsing
      string
      ::  any amount of characters after ;
      comment
      ::  normal symbols
      symbol
    ==
    ==
  ++  read-str
    |=  s/tape
    ^-  parse-res
    %-  need
    %+  trust  s
    %+  cook  (hint %read-str parse-res)
    read-form
  ::
  ++  read-list
    %+  cook  (rift |=(p/(list mal-type) [%list p]))
    %+  rfix  [pel (rex "expected )" per)]
    %+  cook  (hint %read-list-zing (result (list mal-type) tape))
    (rore ace (knee *parse-res |.(read-form)))
  ::
  ++  read-vector
    %+  cook  (rift |=(p/(list mal-type) [%vect p]))
    %+  rfix  [sel (rex "expected ]" ser)]
    %+  cook  (hint %read-vector-zing (result (list mal-type) tape))
    (rost ace (knee *parse-res |.(read-form)))
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
    ;~  prix
      spaces
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
++  env
  |_  {outer/(unit _env) data/table}
  ::
  ++  abet  +<       ::  sample
  ::
  ++  this  ..abet   ::  core
  ::
  ++  new
    |=  {outer/(unit _env) binds/(list mal-type) exprs/(list mal-type)}
    ^-  _env
    =/  blank  ~(. env [outer *table])
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
    =/  close/_env  (new:env ~ ~ ~)
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
      ^-  mal-lambda
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
    =/  args  `(list mal-type)`p.s
    =^  sym  args  (take `(list mal-type)`args)
    ?.  ?=({$symb *} sym)
      [~ this]
    =/  prim  p.sym
    ::
    ?:  =(prim "def!")
      =^  bind  args  (take args)
      =^  code  args  (take args)
      =^  res/mal-type  this  (reed (eval `mal-type`code))
      ?:  =(%nil res)
        ~|  %bad-bind
        [(some %nil) this]
      =/  key  ?>  ?=({$symb *} bind)
               p.bind
      ::=.  ctx  (set:ctx `tape`key `mal-type`res)
      [(some res) this(ctx `_env`(set:ctx key res))]
    ::
    ?:  =(prim "let*")
      =^  bindings  args  (take args)
      ?>  ?=({$list *} bindings)
      =/  bindings/(list mal-type)  p.bindings
      =/  old-env  ctx
      =.  ctx  (new:env (some ctx) ~ ~)
      =.  ctx
      |-
        ?:  =((lent bindings) 0)
          ctx
        =^  name/mal-type  bindings  (take bindings)
        ?>  ?=({$symb *} name)
        =^  entr  bindings  (take bindings)
        =^  bind  this  (reed (eval entr))
        ?:  =(%nil bind)
          ~&  %bad-let
          $(bindings bindings)
        ::~&  [%let-bind p.name bind]
        $(ctx (set:ctx p.name bind), bindings bindings)
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
      =^  res  this  (spin args f `_mal`this)
      [(some [%list res]) this]
    ::
    ?:  =(prim "fn*")
      =^  param  args  (take args)
      ~|  param
      ?>  ?=({$list *} param)
      =^  close  args  (take args)
      :_  this
      %-  some
      :-  %fun
        |=  arg/(list mal-type)
        ^-  mal-type
        =/  new-env  (new:env (some ctx) p.param arg)
        ::(reed (eval:~(. mal new-env) close))
        [%atom -0]

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
      {$vect *}  :(weld "[" (roll (turn p.s |=(m/mal-type -:(print m))) |=({a/tape b/tape} ?~(b a :(weld b " " a)))) "]")
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
