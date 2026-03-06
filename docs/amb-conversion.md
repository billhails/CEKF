# Convert AMB to normal CPS after the CPS transform

As a prelude to generating C or LLVM, we need to transform AMB from a `fail` register to a plain
continuation.

The orginal inspiration for amb came from SICP, where it is implemented in this way as a second continuation passed as argument along with the first and passed as an extra argument to the first continuation when that is invoked. The only places that actually do anything with, or to, the failure continuation are `amb` itself, which creates a new failure continuation that will resume evaluating its second argument, and `back`, which invokes the failure continuation.

This should be much simpler than the CPS transform, we are simply threading the extra continuation through the existing control flow. However it makes sense both for proving and for documentation to implement the AMB transform as a prototype in `fn/rewrite` first, before attempting to implement in C.

While many transforms (beta reduction, eta reduction etc.) are somewhat interchangeable, the AMB transform
must occur after CPS and before closure conversion.

The $\mathcal{AMB}$ transform will affect the following nodes in minexp:

## Lambda

$$
\mathcal{AMB}\big\lgroup\mathtt{(\lambda(a\ k)\ b)}, \mathcal{f}\big\rgroup
\mapsto
\mathtt{(\lambda(a\ k\ \mathcal{f'})\ \mathcal{AMB}\big\lgroup \mathtt{b}, \mathcal{f'}\big\rgroup)}
$$

where $\mathcal{f}'$ is a fresh variable.

## Apply

$$
\mathcal{AMB}\big\lgroup\mathtt{(a\ b\ k)}, \mathcal{f}\big\rgroup
\mapsto
\mathtt{(a\ b\ k\ \mathcal{f})}
$$

## Amb

$$
\mathtt{(amb\ (k\ a)\ (k\ b))}
\mapsto
\mathtt{(k\ a\ (\lambda\ ()\ (k\ b\ \mathcal{f})))}
$$

or formally

$$
\mathcal{AMB}\big\lgroup\mathtt{(amb\ a\ b)}, \mathcal{f}\big\rgroup
\mapsto
\mathcal{AMB}\big\lgroup \mathtt{a}, (\lambda ()\ \mathcal{AMB}\big\lgroup \mathtt{b}, \mathcal{f}\big\rgroup)\big\rgroup
$$

## Back

$$
\mathcal{AMB}\big\lgroup\mathtt{(back)}, \mathcal{f}\big\rgroup
\mapsto
\mathtt{(\mathcal{f})}
$$

## Status

This is now implemented in `fn/rewrite/amb.fn` and
incorporated into `fn/rewrite/test_harness.fn` where
it *seems* to be working, but needs quite a bit of
testing.

However this most miraculous transformation just popped out, and had me scratching my head as to 1. how can it be correct and 2. since it is, how on earth did it work that out!

```scheme
; 20. Complex amb with computation
(+ (amb 1 2) (amb 3 4))
; ==> amb transform
(□ 4
  (λ ()
    (□ 5
      (λ ()
        (□ 5
          (λ ()
            (□ 6 Ω)))))))
```

(□ is the outer continuation and Ω is the outer failure continuation.)
