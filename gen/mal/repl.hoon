/-    sole
/+    mal
=+    [. sole mal]

:-  %ask
|=  *
^-  (sole-result (cask tang))

=/  machine  new:mal:mal
|-
%+  sole-lo  [%.y %mal-repl "user> "]
%+  sole-go  (most (easy ~) qit)
|=  s/tape
::  vase of (result {tape _mal} ?(eval-err $safe-fail)) or something
=/  res  (safe-rep:machine s)
?:  ?=({$err *} res)
  %+  sole-yo
  ^-  tank
  ?:  ?=({$safe-fail *} p.res)
    [%rose ["" "" ""] +.p.res]
  >[%repl-none res]<
  ^$
=^  val  machine  (reed res)
%+  sole-yo
  ^-  tank
  :*  %rose
    ["" "" ""]
    :~
      leaf+s
      leaf+"\0a"
      =+  a=(sell !>(val))
      ?>  ?=({$leaf *} a)
      leaf+(shave p.a)
    ==
  ==
^$(machine machine)  ::  recurse
