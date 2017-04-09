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
++  read
  |=  s/tape
  ^-  (unit mal-type)
  (read-str:parser s)
::
++  eval
  |=  s/(unit mal-type)
  ^-  mal-type
  (need s)
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
               (weld ?:(-.o "" "-") <+.o>)
    ::
    {$symb *}  p.s
    ::
    {$str *}   p.s
  ==
--
