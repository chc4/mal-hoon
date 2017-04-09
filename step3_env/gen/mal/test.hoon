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
=/  comp/{code/tape rest/(list cord)}
  =/  acc/tape  ""
  |-
  ^-  {tape (list cord)}
  ?~  test
    [acc ~]
  ?:  =((cut 3 [0 1] i.test) ';')
    [acc [i.test t.test]]
  $(acc `tape`:(weld acc (trip i.test)), test `(list cord)`t.test)
~|  running+code.comp
=/  machine  new:mal:mal
=/  cont  (safe-rep:machine code.comp)
~&  [%running code.comp]
=/  adv  rest.comp
?~  adv
  ~|  'nothing to test against'
  !!
=/  next  i.adv
?.  =((cut 3 [0 1] next) ';')
  ~|  'next line wasnt result'^next
  !!
?:  ?|
      =((cut 3 [0 2] next) '; ')
      &(!=(~ cont) =((rsh 3 3 next) (crip -:(need cont))))
    ==
  ["PASS: {code.comp}" $(test t.adv)]
["FAIL: {code.comp}" $(test t.adv)]

