/-    sole
=+  [sole]

|%
++  read
  |=  s/tape
  s
::
++  eval
  |=  s/tape
  s
::
++  print
  |=  s/tape
  s
::
++  rep
  |=  s/tape
  (print (eval (read s)))
--
