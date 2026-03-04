# Target C as a Compilation Step

As a stopgap before targetting LLVM, but also as a useful exercise that might inform
the LLVM work, I'd like to attempt to generate a standalone `main` function from
the MinExp (`minlam.yaml`) structures after CPS transformation and closure conversion.

This seems reasonable as CPS allows all function calls to be replaced with `goto`,
and the closure conversion means that all variable access is local.
