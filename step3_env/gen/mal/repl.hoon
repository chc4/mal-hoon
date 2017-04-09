/-    sole
/+    mal
=+    [. sole mal]

:-  %ask
|=  *
^-  (sole-result (cask tang))

=/  machine  new:mal:mal
=/  cont/vase  !>(machine)
|-
%+  sole-lo  [%.y %show "user> "]
%+  sole-go  (most (easy ~) qit)
|=  s/tape
=/  res  (safe-rep:machine (slop !>(s=s) cont))
?~  res
  ^$
=.  cont  `vase`(slot 3 (need res))
%+  sole-yo
  ^-  tank
  :*  %rose
    ["" "" ""]
    :~
      leaf+s
      leaf+"\0a"
      =+  a=(sell (slot 2 (need res)))
      ?>  ?=({$leaf *} a)
      leaf+(slag 1 (scag (dec (lent p.a)) p.a))
    ==
  ==
^$(machine machine)  ::  recurse
