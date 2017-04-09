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


:-  %noun
=/  test/(list cord)
  .^
    (list cord)
  :-  %cx
  (tope bek (flop `path`/gen/mal/tests/(crip (weld (trip arg) ".mal"))/txt))
  ==

^-  (list tape)
=/  machine/(unit vase)  (some !>(new:mal:mal))
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
=+  [code=*(list cord) trout=*(unit vase) rest=*(list cord)]
=+  |-
  ?~  test
    [code=code trout=trout rest=~ machine=machine]
  ?:  =((cut 3 [0 1] i.test) ';')
    [code=code trout=trout rest=[i.test t.test] machine=machine]
  ~|  running+(trip i.test)
  =/  salmon  (safe-rep:mal:mal (slop !>(s=(trip i.test)) (need machine)))
  %=  $
    code  [i.test code]
    trout  (bind salmon |=(v/vase (slot 2 v)))
    test  `(list cord)`t.test
    machine   ^-  (unit vase)
              %-  some
              ^-  vase
              %+  fall
                %+  bind  salmon  |=(v/vase `vase`(slot 3 v))
              (need machine)
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
  ::  compile-fail test
  ?~  trout
    ["[COMFAIL] PASS: {<code>}" $(test t.rest, machine machine)]
  ["[COMFAIL] FAIL: {<code>}" $(test t.rest, machine machine)]
::
=/  p/tank  (sell `vase`(need trout))
?>  ?=({$leaf *} p)
=/  out  (crip (shave:mal p.p))
=/  expect  (rsh 3 3 next)

?:  !=(~ trout)
 ?:  =(expect out)
    ["[RUNTIME] PASS: {<code>}" $(test t.rest, machine machine)]
  ["[RUNTIME] FAIL: {<code>}, EXPECTED {<`crip`expect>} GOT {<out>}" $(test t.rest, machine machine)]
["[RUNTIME] FAIL: {<code>}" $(test t.rest, machine machine)]

