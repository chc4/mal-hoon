/-  help
=+  [. ^help]
!:
|%
++  zang                                                ::  list monad
  |*  a/*
  ^-  (list _a)
  [a ~]
::
++  hint                                                ::  hard with trace
  |*  {msg/cord han/mold}
  |=  fud/*  ^-  han
  ~|  [%hint msg fud]
  =+  gol=(han fud)
  ?>(=(gol fud) gol)
::
++  dy-cast
  |*  {typ/mold bun/vase}
  |=  a/vase  ^-  typ
  ~|  [p.bun p.a]
  ?>  (~(nest ut p.bun) & p.a)
  ;;(typ q.a)
::
++  shave
  |=  p/tape
  (slag 1 (scag (dec (lent p)) p))
::
++  spin                                                ::  stateful turn
  |*  {a/(list) b/_|=({* *} [** +<+]) c/*}
  ^-  {(list _-:(b)) _c}
  =/  acc/(list _-:(b))  ~
  |-
  ?~  a
    [acc c]
  =^  res/_-:(b)  c  (b i.a c)
  $(acc (welp acc ~[res]), a t.a, c c)
::
++  spun                                                ::  internal spin
  |*  {a/(list) b/_|=({* *} [** +<+])}
  =|  c/_+<+.b
  =+  [p q]=(spin a b c)
  p
::
++  ioref  ::  no PhantomData<T>, so hack it with $@
  |*  t/mold
  $@  i/@  {$phantom t}
++  io
  |*  t/mold
  ^?
  |_  {refs/(map (ioref t) t) id/@}
  +-  abet  +<
  +-  this  .
  +-  new-ioref
    |=  val/t
    ^-  {(ioref t) _this}
    =/  i   id
    =.  refs  (~(put by refs) i val)
    [i this(id +(id))]
  +-  read-ioref
    |=  token/(ioref t)
    (~(get by refs) token)
  ::
  +-  write-ioref
    |*  {token/(ioref t) val/*}
    ?.  (~(has by refs) token)
      ~|  'bad token'
      !!
    =.  refs  (~(put by refs) token val)
    this
  --
::
++  reed
  |*  r/(result)
  ?:  ?=({$ok *} r)
    +.r
  ?:  ?=({$err *} r)
    ~|  %reed
    !!
  ~|  %not-res-reed
  !!
::
++  rail
  |*  r/(result)
  ?:  ?=({$err *} r)
    +.r
  ?:  ?=({$ok *} r)
    ~|  %rail
    !!
  ~|  %not-res-rail
  !!
::
++  riff
  |*  {r/(result) f/$-(* (result))}
  ::^-  (result ?(_(reed r) _(reed $:f)) ?(_(rail r) _(rail $:f)))
  ?:  ?=({$ok *} r)
    (f p.r)
  r
::
++  rind                                                ::  argue
  |*  {a/(result) b/gate}
  ^-  (result _(b) _(rail a))
  ?:  ?=({$ok *} a)
    (rome (b (reed a)))
  a
::
++  rate                                                ::  choose
  |*  {a/(result) b/(result)}
  ?:  ?=({$err *} b)  a
  ?:  ?=({$err *} a)  b
  ?.(=(p.a p.b) ~|('rate' !!) a)
::
++  rung                                                ::  adapt unit
  |*  {err/* u/(unit)}
  ^-  (result _(need u) _err)
  ?~  u
    [%err err]
  [%ok u.u]
::
++  ring                                                ::  adapt result
  |*  a/(result)
  ^-  (unit _(reed a))
  ?:  ?=({$err *} a)
    ~
  (some (reed a))
::
++  rall
  |*  {a/(result) b/*}
  ?:  ?=({$err *} a)
    b
  a
::
++  rift                                                ::  curried lift
  |*  f/gate
  |*  r/(result)
  ?:  ?=({$err *} r)
    r
  (rome (f +.r))
::
++  roth
  |*  {p/(result) q/(result)}
  %+  riff  p  |*  p/*
  %+  riff  q  |*  q/*
  (rome (welp p q))
::
++  rath                                                ::  cons
  |*  {p/(result) q/(result)}
  %+  riff  p  |*  p/*
  %+  riff  q  |*  q/*
  (rome [p q])
::
++  rome
  |*  p/*
  [%ok p=p]
::
++  trust
  |*  {t/tape r/rule}
  ::^-  (unit (like (wonk *r)))
  =/  vex/(like _(wonk *r))  (r [[1 1] t])
  ?~  q.vex
    ~
  (some p.u.q.vex)
::
++  ruler  _|=(nail *(like (result)))                               ::  parsing rule
::
++  rex                                                 ::  parse else result
  |*  {else/result sef/rule}
  |=  tub/nail
  =+  vex=(sef tub)
  ?~  q.vex
    [p=p.tub q=[~ u=[p=[%err else] q=tub]]]
  [p=p.vex q=[~ u=[p=(rome p.u.q.vex) q=q.u.q.vex]]]
::
++  romp
  |*  raq/_|*({a/* b/*} [a b])                       ::  arbitrary compose
  |*  {vex/edge sab/rule}
  ~!  +<
  ?~  q.vex
    vex
  ?:  ?=({$err *} p.u.q.vex)
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?~  q.yit
    [p=yur q=q.yit]
  ?:  ?=({$err *} p.u.q.yit)
    [p=yur q=q.yit]
  [p=yur q=[~ u=[p=(raq p.u.q.vex p.u.q.yit) q=q.u.q.yit]]]
::
++  prix
  (romp |*({a/* b/*} b))
::
++  srix
  (romp |*({a/* b/*} a))
::
++  rug                                              ::  first then second
  |*  {vex/_*(like (result)) sab/ruler}
  ?~  q.vex
    vex
  ?:  ?=({$err *} p.u.q.vex)
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?~  q.yit
    [p=yur q=q.yit]
  ?:  ?=({$err *} p.u.q.yit)
    [p=yur q=q.yit]
  ~!  p.u.q.yit
  [p=yur q=[~ u=[p=(rome [(reed p.u.q.vex) (reed p.u.q.yit)]) q=q.u.q.yit]]]
::
++  rose                                               ::  first or second
  ::  if the rule is either none or an err that didn't parse anything
  |*  {vex/edge sab/rule}
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  ?:  ?=({$err *} p.u.q.vex)
    =+  roq=(sab)
    ?:  =(p.vex p.roq)
      [p=(last p.vex p.roq) q=q.roq]
    ?~  q.roq
      vex
    ?:  ?=({$err *} p.u.q.roq)
      [p=(last p.vex p.roq) q=q.roq]
    vex
    ::[p=(last p.vex p.roq) q=q.roq]
  vex
::
++  rage                                                ::  either
  |*  {p/(result) q/(result)}
  ?:  ?=({$err *} p)  p
  ?:  ?=({$err *} q)  q
  (rome [(reed p) (reed q)])
::
++  ress                                                ::  no first and second
  |*  {vex/edge sab/rule}
  ?~  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  ?:  ?=({$err *} p.u.q.vex)
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  (fail +<.sab)
::
++  rlus  |*(fel/rule ;~(rug fel (rtar fel)))
::
++  rtar                                                ::  0 or more times
  |*  fel/rule
  %^  rtis
    `(result (list _(reed (wonk *fel))) _(rail (wonk *fel)))`(rome ~)
    roth
  fel
::
++  rtis
  ::  try to parse with fel
  ::  if failure return rud
  ::  if err return the err
  ::  add new element to the top of rud
  ::  recurse and advance
  |*  {rud/* raq/_roth fel/ruler}
  |=  tub/nail
  ~!  (wonk *fel)
  =/  res  (result (list _(reed (wonk *fel))) _(rail (wonk *fel)))
  ^-  (like res)
  =+  vex=(fel tub)
  ?~  q.vex 
    [p.tub [~ rud tub]]
  ?:  =(p.vex p.tub)
    [p.tub [~ `res`rud tub]]
  ?:  ?=({$err *} p.u.q.vex)
    [p.vex [~ p.u.q.vex tub]]
  =/  con/res
    %+  raq
      `(result (list _(reed (wonk *fel))) _(rail (wonk *fel)))`rud
      `(result (list _(reed (wonk *fel))) _(rail (wonk *fel)))`(rome (limo ~[(reed p.u.q.vex)]))
  $(rud con, tub q.u.q.vex)
::
++  rore
  |*  {bus/rule fel/rule}
  ;~(rose (rost bus fel) (cook rome (easy ~)))
::
++  rost
  |*  {bus/rule fel/rule}
  ;~(rug fel (rtar ;~(prix bus fel)))
::
++  roar                                                ::  one or more
  ~|  %dont-use-me
  ?>  .n
  |*  {sep/rule tag/rule}
  ::^-  (like (result (list _(reed (wonk *tag))) _(rail wonk *tag)))
  ;~  pose
    (roast sep tag)
    ::
    %+  cook  rome
    (easy ~)
  ==
::
++  roast                                               ::  more than one
  |*  {sep/rule tag/rule}
  ~|  %dont-use-me
  ?>  .n
  =/  res  (result (list _(reed (wonk *tag))) _(rail (wonk *tag)))
  ::^-  (like res)
  ;~  pose
    %+  cook
      |=  {p/res q/res}
      (roth p q)
    ;~  plug
      %+  cook  (rift zang)
      tag
      ::
      |-
      ::%+  cook  |=  {p/(like _(wonk *tag)) q/(like _(wonk *tag))}  (rage p q)
      ;~  rug
        ;~  pfix
          sep
          ::
          tag
        ==
        ::
        ;~  pose
          (knee *res |.(^$))
          ::
          %+  cook  rome
          (easy ~)
        ==
      ==
    ==
    ::
    ;~  rose
      %+  cook  (rift zang)
      tag
      ::
      fail
    ==
  ==
::
++  rfix
  |*  {fel/{rule rule} hof/rule}
  ~!  +<
  ~!  +<:-.fel
  ~!  +<:+.fel
  ;~(prix -.fel ;~(srix hof +.fel))
::
++  rest
  ::  parse the next char
  ::  if it succeeds, try to parse the next one too
  |=  t/tape
  =/  str  t
  |-
  %+  cook
    |=  {r/(result {tape tape} tape)}
    ^-  (result tape tape)
    %+  riff  r  |=  {p/tape q/tape}
    (rome (welp p q))
  ;~  rug
    %+  sear
      |=  a/@t
      ^-  (unit (result tape tape))
      ~&  [%pars a str]
      ?:  =((lent str) 0)
        ~
      ?:  =((snag 0 str) a)
        `[%ok (zang (snag 0 str))]
      `[%err :(weld "parsing " t " got " (zang a))]
    next
    ::
    ;~  pose
      (knee *(result tape tape) |.(?~(str !! ^$(str t.str))))
      ::
      %+  cook
      |=  *
      ~&  str
      ?^  str
        ?:  =((lent str) 1)
          (rome ~)
        [%err :(weld "parsing " t " expected more")]
      [%err :(weld "parsing " t " expected more")]
      (easy ~)
    ==
  ==
::
++  rrst                                                ::  match a cord
  |=  daf/@t
  |=  tub/nail
  =+  fad=daf
  |-  ^-  (like @t)
  ?:  =(`@`0 daf)
    [p=p.tub q=[~ u=[p=fad q=tub]]]
  ?:  |(?=($~ q.tub) !=((end 3 1 daf) i.q.tub))
    (fail tub)
  $(p.tub (lust i.q.tub p.tub), q.tub t.q.tub, daf (rsh 3 1 daf))
--
