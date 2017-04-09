/+  mal

:-  %say
|=  $:
  {now/@da eny/@uvJ bek/beak}
  {arg/tape $~}
  $~
==
:-  %noun

=/  res  (read:mal arg)
?~  res
  ~|  'failed to compile!'
  !!
(print:mal (eval:mal res))
