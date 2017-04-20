/-  help
=>  help
|%
++  parse-res
    (result mal-type tape)
::
++  mal-type
$?
  $nil
  $true
  $false
$%
  {$list p/(list mal-type)}
  {$vect p/(list mal-type)}
  {$atom p/@s}
  {$symb p/tape}
  {$str p/tape}
  {$fun p/mal-lambda}
==
==
::
++  mal-lambda
  $-((list mal-type) mal-type)
::
++  eval-err
    $@
      $?
      $empty-call
      $special-fail
      ==
    $%
      {$eval-bad-func mal-type}
    ==
++  table
  (map tape mal-type)
::
--
