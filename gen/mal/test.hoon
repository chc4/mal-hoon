/-    mal
/+    mal
!:

=>  |%
++  tests
$?
  $'step0_repl'
  $'step1_read_print'
  $'step2_eval'
  $'step3_env'
  $'step4_if_fn_do'
  $'step5_tco'
  $'step6_file'
  $'step7_quote'
  $'step8_macros'
  $'step9_try'
==
--

:-  %say
|=  $:
  {now/@da eny/@uvJ bek/beak}
  {arg/tests $~}
  {defer/_.n option/_.n $~}
==

=+  [help mal]

:-  %noun
=/  test/(list cord)
  .^
    (list cord)
  :-  %cx
  (tope bek (flop `path`/gen/mal/tests/(crip (weld (trip arg) ".mal"))/txt))
  ==

^-  (list tape)
=/  machine/(unit _mal:mal)  (some new:mal:mal)
|-
?~  test
  ~
?:  ?|
      &(!defer =(i.test ';>>> deferrable=True'))
      &(!option =(i.test ';>>> optional=True'))
    ==
  ::  just ignore the rest
  ~
?:  |(=(i.test '') =((cut 3 [0 1] i.test) ';'))
  ::  comment, ignore it
  $(test t.test)
=/  test/(list cord)  test
::  too lazy to build a proper door for this
=+  [code=*(list cord) trout=*(unit tape) rest=*(list cord)]
=+  |-
  ?~  test
    [code=code trout=trout rest=~ machine=machine]
  ?:  =((cut 3 [0 1] i.test) ';')
    [code=code trout=trout rest=[i.test t.test] machine=machine]
  ~|  running+(trip i.test)
  =/  salmon/(result {tape _mal:mal} *)  (safe-rep:(need machine) (trip i.test))
  %=  $
    code  [i.test code]
    trout  (ring (rind salmon |=({v/tape _mal:mal} v)))
    test  `(list cord)`t.test
    machine   ^-  (unit _mal:mal)
              %-  ring
              %+  rall
                ^-  (result _mal:mal *)  %+  rind  salmon  |=({t/tape v/_mal:mal} v)
              `(result _mal:mal *)`(rung "empty machine" machine)
  ==
=/  rest/(list cord)  rest
?~  rest
  ~|  'nothing to test against'
  !!
=/  next  i.rest
?.  =((cut 3 [0 1] next) ';')
  ~|  'next line wasnt result'^next
  !!
?:  =((cut 3 [0 2] next) '; ')
  :: XX LOLJK THIS IS FOR ALL OUTPUT WHAT THE FUCK
  ::  hoon literally can't do this, slog prints to console
  ::  compile-fail test
  ::?~  trout
  ::  ["[COMFAIL] PASS: {<code>}" $(test t.rest, machine machine)]
  ::["[COMFAIL] FAIL: {<code>}" $(test t.rest, machine machine)]
  ["[PRINTF] PASS: {<code>}" $(test t.rest, machine machine)]
::

?:  !=(~ trout)
  =/  out  (crip (need trout))
  =/  expect  (rsh 3 3 next)
 ?:  =(expect out)
    ["[RUNTIME] PASS: {<code>}" $(test t.rest, machine machine)]
  ["[RUNTIME] FAIL: {<code>}, EXPECTED {<`crip`expect>} GOT {<out>}" $(test t.rest, machine machine)]
["[RUNTIME] FAIL: {<code>}" $(test t.rest, machine machine)]

