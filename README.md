# Infinities.jl
A Julia package for representing infinity in all its forms

[![Build Status](https://github.com/JuliaMath/Infinities.jl/workflows/CI/badge.svg)](https://github.com/JuliaMath/Infinities.jl/actions)
[![codecov](https://codecov.io/gh/JuliaMath/Infinities.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JuliaMath/Infinities.jl)
[![version](https://juliahub.com/docs/General/Infinities/stable/version.svg)](https://juliahub.com/ui/Packages/General/Infinities)


This Julia package is used to represent infinities, including:

1. `InfiniteCardinal{k}`, for the cardinality of an infinite set, e.g., `ℵ₀` for the cardinality of the integers and `ℵ₁` for the first uncountable cardinal. The statement that `ℵ₁` is the cardinality of the reals is the continuum hypothesis, not an unconditional equality.
2.  `∞` to represent the positive real infinity.
3. `RealInfinity` to represent `±∞`.
4. `ComplexInfinity` to represent an oriented infinity  `exp(im*θ)∞`


Note that we subtype based on interfaces, rather than strict mathematical definitions. For example,  `ℵ₀ isa Integer` as `Integer` is often used to represent the size of a set or vector. Similarly, `∞ isa Real`.

## Extending `RealInfinity`

To add another representation of positive or negative infinity, subtype `RealInfinity`
and define `Base.signbit`: return `false` for positive infinity and `true` for negative infinity.

```julia
struct SignedInfinity <: RealInfinity
	negative::Bool
end
Base.signbit(inf::SignedInfinity) = inf.negative

SignedInfinity(true) == -∞            # true
SignedInfinity(true)^2 === +∞         # true
```

Read the sign from your representation in `signbit`. Do not define it as `x < 0`,
because the inherited comparisons call `signbit` and would cause infinite recursion.
You do not need to implement arithmetic, comparisons, floating-point conversion, or
hashing: your subtype inherits these operations from `RealInfinity`. Any additional
fields you store are ignored when comparing or hashing values. Calling `zero` on your
type or an instance returns `0.0`. Calling `one` or `oneunit` returns `1.0`.

A subtype may represent only one sign. Results need not retain its concrete type or
metadata: negation and powers may return `PositiveInfinity` or `NegativeInfinity`.
Constructors and representation-preserving conversions are the subtype's responsibility.
Specialize standard Base operations when representation preservation is needed.

## Static.jl integration

Loading [Static.jl](https://github.com/SciML/Static.jl) enables an optional package extension.
The values `∞`, `+∞`, `-∞`, `InfiniteCardinal{k}()`, and `NotANumber()` are already
fully determined by their types, so they need no separate static representation:

```julia
using Infinities, Static

static(∞) === ∞                       # true
is_static(typeof(-∞)) === True()      # true
known(typeof(ℵ₀)) === ℵ₀               # true
Static.lt(static(2), ∞) === True()     # true
static(2) + ℵ₀ === ℵ₀                 # true
```

Mixed arithmetic and comparisons use the same rules as the corresponding ordinary
numbers, including undefined results and NaN propagation. Base operations retain their
ordinary return types: `one(∞)` is `1` and `isinf(∞)` is `true`. Use `static` on a result
or Static.jl's comparison functions when a static result type is needed.

`ComplexInfinity` stores its direction as a value, not in its type. Its arithmetic
accepts static operands, but `is_static(ComplexInfinity)` is `False()` and
`static(im*∞)` is unsupported.

## ForwardDiff.jl integration

Loading [ForwardDiff.jl](https://github.com/JuliaDiff/ForwardDiff.jl) enables an optional
extension for mixed `+`, `-`, `*`, `/`, `mod`, `rem`, `min`, `max`, and comparisons
of dual numbers with `∞`, `+∞`, `-∞`, and `NotANumber()`. Primal values retain
Infinities' scalar results and exceptions, without converting infinities to floats.
Derivative rules use the same scalar arithmetic, so a zero tangent multiplied by an
infinity becomes `NotANumber()`. Bounded `mod` and `rem` preserve the dividend's
tangents, while an undefined remainder has undefined tangents.

```julia
using Infinities, ForwardDiff

ForwardDiff.derivative(input -> input * ∞, 2.0) # +∞
ForwardDiff.derivative(input -> input + ∞, 2.0) # 1.0
ForwardDiff.derivative(input -> input / ∞, 2.0) # 0.0
```

`Dual(∞)` preserves the infinity. Explicitly requesting a floating scalar type, such
as `Dual{Nothing, Float64}(∞)`, converts it. Mixed symbolic results can use `Real`
as the dual's scalar parameter, with a corresponding loss of type specialization.

This is not general support for arbitrary symbolic differentiation: ForwardDiff
operations such as unary negation can reject heterogeneous symbolic partials, and
powers remain subject to the existing scalar and ForwardDiff limitations. There is
no floating-point fallback. The extension does not define mixed dual-number operations
for `InfiniteCardinal` or `ComplexInfinity`.

## Similar packages

This package is meant to eventually replace [Infinity.jl](https://github.com/cjdoris/Infinity.jl) and the definitions of `∞` in [InfiniteArrays.jl](https://github.com/JuliaArrays/InfiniteArrays.jl). We do not yet support Infinity.jl's notions of `InfExtendedReal` but we hope to add this soon.
