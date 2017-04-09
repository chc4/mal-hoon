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
=+  [code=*(list cord) trout=*(unit vase) rest=*(list cord)]
=+  |-
  ?~  test
    [code=code trout=trout rest=~]
  ?:  =((cut 3 [0 1] i.test) ';')
    [code=code trout=trout rest=[i.test t.test]]
  ~|  running+(trip i.test)
  =/  salmon  (safe-rep:mal:mal (slop !>(s=(trip i.test)) (need machine)))
  $(code [i.test code], trout ((lift |=(v/vase (slot 2 v))) salmon), test `(list cord)`t.test, machine ((lift |=(v/vase (slot 3 v))) salmon))
=/  rest/(list cord)  rest
?~  rest
  ~|  'nothing to test against'
  !!
=/  next  i.rest
?.  =((cut 3 [0 1] next) ';')
  ~|  'next line wasnt result'^next
  !!
?:  ?|
      =((cut 3 [0 2] next) '; ')
    ?&
      !=(~ trout)
      .=
        `cord`(rsh 3 3 next)
        =/  p/tank  (sell `vase`(need trout))
        ?>  ?=({$leaf *} p)
        `cord`(crip (shave:mal p.p))
    ==
    ==
  ["PASS: {<code>}" $(test t.rest)]
["FAIL: {<code>}" $(test t.rest)]

