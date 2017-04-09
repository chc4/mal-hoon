|%
++  mal-type
$@
  $nil
$%
  {$list p/(list mal-type)}
  {$atom p/@s}
  {$symb p/tape}
  {$str p/tape}
  {$fun p/mal-lambda}
==
::
++  mal-lambda
  $-((list mal-type) mal-type)
::
++  table
  (map tape mal-type)
::
--
