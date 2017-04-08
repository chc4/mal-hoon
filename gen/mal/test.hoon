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

|-
?~  test
  ~
?:  |(=(i.test '') =((cut 3 [0 2] i.test) ';;'))
  ::  comment, ignore it
  $(test t.test)
=/  code  i.test
~&  running+code
=/  cont  (rep:mal (trip code))
=/  adv  t.test  ::  advance the list
?~  adv
  ~|  'nothing to test against'
  !!
=/  next  i.adv
?.  =((cut 3 [0 3] next) ';=>')
  ~|  'next line wasnt result'^next
  !!
?:  =((rsh 3 3 next) (crip cont))
  ["PASS" $(test t.adv)]
["FAIL: code" $(test t.adv)]

