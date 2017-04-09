/+    mal

!:
:-  %say
|=  $:
  {now/@da eny/@uvJ bek/beak}
  {arg/tape $~}
  $~
==

:-  %noun
=/  test
  .^
    (list cord)
  :-  %cx
  (tope bek (flop `path`/gen/mal/tests/(crip (weld arg ".mal"))/txt))
  ==

^-  (list tape)
|-
?~  test
  ~
?:  =(i.test ';>>> deferrable=True')
  ::  just ignore the rest
  ~
?:  |(=(i.test '') =((cut 3 [0 1] i.test) ';'))
  ::  comment, ignore it
  $(test t.test)
=/  code/cord  i.test
~|  running+code
=/  cont  (read:mal (trip code))
=/  adv  t.test  ::  advance the list
?~  adv
  ~|  'nothing to test against'
  !!
=/  next  i.adv
?.  =((cut 3 [0 1] next) ';')
  ~|  'next line wasnt result'^next
  !!
?:  ?|
      =(~ cont)
      =((rsh 3 3 next) (crip (print:mal (eval:mal cont))))
      =((cut 3 [0 1] next) ';')  ::  this test harness is fucking stupid
    ==
  ["PASS" $(test t.adv)]
["FAIL: code" $(test t.adv)]

