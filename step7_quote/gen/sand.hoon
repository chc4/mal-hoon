|=  t/tape
=>
|%
++  ioref  ::  no PhantomData<T>, so hack it with $@
  |*  t/*
  $@  i/@  {$phantom t}
++  io
  |*  t/mold
  |_  {refs/(map ioref t) id/@}
  ++  abet  +<
  ++  this  .
  ++  new-ioref
    |=  val/t
    =/  i   id
    =.  id  +(id)
    [this i]
  ++  read-ioref
    |=  token/(ioref t)
    (~(get by refs) token)
  ::
  ++  set-ioref
    |*  {token/(ioref t) val/*}
    ?.  (~(has by refs) token)
      ~|  'bad token'
      !!
    =.  refs  (~(put by refs) token val)
    [val this]
==

=/  world  (io @)
=^  x/(ioref @)  world  (new-ioref:world 1)
=/  y  (read-ioref:world x)
y
