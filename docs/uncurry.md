# Uncurrying Scratchpad

```scheme
;          uncurried <==> curried
((λ (x y) body) arg) <==> ((λ (x) (λ (y) body)) arg)
```

$$
\lambda.x_1.x_2\;e = \lambda.x_1\;(\lambda.x_2\;e)
$$
