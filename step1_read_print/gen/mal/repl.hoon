/-    sole
/+    mal
=+  [sole]

:-  %ask
|=  *
^-  (sole-result (cask tang))

|-

%+  sole-lo  [%.y %show "user> "]
%+  sole-go  (most (easy ~) qit)
|=  s/tape
=/  res  (read:mal s)
?~  res
  ^$
%+  sole-yo  leaf+(print:mal (eval:mal res))
^$  ::  recurse
