# prime-cert

[Pratt primality certificates](http://boole.stanford.edu/pub/SucCert.pdf)
in Haskell, implemented as an [LCF](https://en.wikipedia.org/wiki/Logic_for_Computable_Functions)-style proof system.

The result is that we have a type `Prime` which has constructors hidden at the
module level. The only way to obtain a value of type `Prime` outside of this
module is to provide a proof using the system presented in the paper.

![The rules of the proof system](rules.png)

For examples, see [`Examples.hs`](src/Examples.hs).

## References

- [0] &mdash; Vaughn Pratt, [*Every Prime has a Succint Certificate*](http://boole.stanford.edu/pub/SucCert.pdf)
- [1] &mdash; Wikipedia, [*Primality certificate*](https://en.wikipedia.org/wiki/Primality_certificate)
