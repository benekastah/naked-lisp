
define: fact x result
  if: <= x 2
    result
    fact: sub1 x, * result x

'a
