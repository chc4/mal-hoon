/-    sole
=+  [sole]

!:
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
--
::
|%
++  mal-type
$@
  $~
$%
  {$list p/(list mal-type)}
  {$atom p/@s}
  {$symb p/tape}
  {$str p/tape}
==
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
    %+  cook  |=  out/(list mal-type)
      ?:  =((lent out) 1)
        (snag 0 out)
      [%list out]
    read-form
  ::
  ++  read-list
    %+  cook  (hint %read-list mal-type)
    %+  ifix  [pel per]
    %+  stag  %list
    %+  cook  zing
    (more ace (knee *(list mal-type) |.(read-form)))
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
    %+  cook  (hint %read-form (list mal-type))
    ::%+  cook  |=  out/(list mal-type)
    ::  ?:  =((lent out) 1)
    ::    (snag 0 out)
    ::  [%list out]
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
  %-  malt  %-  limo
  :~
    :-  "+"
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (sum:si a b)]
    ::
    :-  "-"
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (dif:si a b)]
    ::
    :-  "*"
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (pro:si a b)]
    ::
    :-  "/"
      |=  arg/(list mal-type)
      ^-  mal-type
      =^  a  arg  (get-atom arg)
      =^  b  arg  (get-atom arg)
      [%atom (fra:si a b)]
    ::
  ==
::
++  eval-ast
  |=  {ast/mal-type env/_env}
  ^-  eval-result
  ?-  ast
    {$symb *}  =/  key  (~(get by env) p.ast)
               ?~  key
                 ~|  [%env-fetch p.ast]
                 !!
               [%fun (need key)]
    ::
    {$list *}  [%call (turn p.ast |=(m/mal-type (eval m env)))]
    ::
    *          [%only ast]
  ==
::
++  mal-lambda
  $-((list mal-type) mal-type)
::
++  eval-result
  $%
    {$fun p/mal-lambda}
    {$call p/(list eval-result)}
    {$only p/mal-type}
  ==
::
++  read
  |=  s/tape
  ^-  mal-type
  (need (read-str:parser s))
::
++  eval
  |=  {s/mal-type env/_env}
  ^-  eval-result
  ?.  ?=({$list *} s)
    (eval-ast s env)
  ::
  ?:  =((lent p.s) 0)
    [%only s]
  ::
  :: get new list
  =/  e/eval-result  (eval-ast s env)
  ?>  ?=({$call *} e)
  ?~  p.e
    ~|  %empty-call
    !!
  =/  func  `eval-result`i.p.e
  ?>  ?=({$fun *} func)
  =/  fun/mal-lambda  p.func
  =/  param/(list eval-result)  t.p.e
  =/  check  |-
    ^-  (list mal-type)
    ?~  param
      ~
    ?.  ?=({$only *} i.param)
      ~|  %not-only
      !!
    [p.i.param $(param t.param)]
  [%only (fun check)]
::
++  print
  |=  s/mal-type
  ^-  tape
  ?-  s
    $~         ""
    ::
    {$list *}  :(weld "(" (roll (turn p.s print) |=({a/tape b/tape} ?~(b a :(weld b " " a)))) ")")
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
  ==
::
++  rep
  |=  s/tape
  =/  repl-env  env
  =/  res  (eval (read s) repl-env)
  ?>  ?=({$only *} res)
  (print p.res)
::
++  safe-rep
  |=  s/tape
  ^-  (unit tape)
  =/  safe
    (mule |.((slap !>(=+(s=s .)) (ream '(rep s)'))))
  ?:  -.safe
    (some ;;(tape +>.safe))
  ~
--
