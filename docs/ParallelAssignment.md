# Parallel Assignment Algorithm

* let $T$ be the set of all target registers
* let $M$ be the dependency map $\set{ r_i \gets r_j }$ where $r_i$ is in $T$. (read: $r_i$ gets its value from $r_j$).
* the term "unoccupied register" means a register that can safely be written to.

* for each empty transition $r_i \gets r_i$ in $M$:
  * delete $r_i \gets r_i$ from $M$
  * delete $r_i$ from $T$

* while $M$ is not empty:
  * while there are unoccupied registers $r_i$ in T:
    * find $r_i \gets r_j$ in $M$
    * EMIT move $r_j$ to $r_i$
    * delete $r_i \gets r_j$ from $M$
    * delete $r_i$ from $T$
    * if $r_x \gets r_j$ not in $M$:
      * mark $r_j$ as unoccupied
  * select an occupied register $r_i$ from $T$
  * EMIT move $r_i$ to $\mathrm{tmp}$
  * foreach $r_h \gets r_i$ in $M$:
  * replace all $r_h \gets r_i$ with $r_h \gets \mathrm{tmp}$ in $M$
  * mark $r_i$ unoccupied

## emitGoto-specific version

```text
Assume every source has already been reduced to a stable source:
    - a register ri
    - the scratch tmp
    - or a constant/immediate source, which never needs protection from clobbering

let T be the set of real destination registers { r0, r1, ..., r(n-1) }
let M be the copy set { rd <-- rs } where rd is in T
let J be the source used by the final jump
let uses(x) be the number of remaining consumers of source x

initialise uses(x) from M
if J is a register source:
    increment uses(J) by 1

the term "unoccupied register" means a register whose old value is no longer needed:
    uses(ri) == 0

for each identity copy { ri <-- ri } in M:
    delete { ri <-- ri } from M
    delete ri from T
    decrement uses(ri)

while M is not empty:
    while there exists an unoccupied register ri in T:
        find ri <-- rj in M
        EMIT move rj to ri
        delete ri <-- rj from M
        delete ri from T
        decrement uses(rj)

    if M is empty:
        break

    choose any occupied register ri from T
    EMIT move ri to tmp

    if J == ri:
        J = tmp

    foreach rh <-- ri in M:
        replace rh <-- ri with rh <-- tmp
        decrement uses(ri)

    uses(tmp) = number of rewritten copies rh <-- tmp
    mark ri unoccupied

EMIT jump to J

Notes:
    - the inner loop handles all acyclic copies
    - the tmp step breaks one cycle
    - one tmp is enough for register-to-register parallel copy
    - J participates in liveness even though it is not itself a real destination register
```
