# Infinities.jl
A Julia package for representing infinity in all its forms

[![Build Status](https://github.com/JuliaMath/Infinities.jl/workflows/CI/badge.svg)](https://github.com/JuliaMath/Infinities.jl/actions)


This Julia package is used to represent infinities, including:

1. `InfiniteCardinal{k}`, for the cardinality of an infinite set, e.g., `ℵ₀` for the cardinality of the integers and `ℵ₁` for the cardinality of the reals. 
2.  `∞` to represent the positive real infinity. 
3. `RealInfinity` to represent `±∞`.
4. `ComplexInfinity` to represent an oriented infinity  `exp(im*θ)∞`


Note that we subtype based on interfaces, rather than strict mathematical definitions. For example,  `ℵ₀ isa Integer` as `Integer` is often used to represent the size of a set or vector. Similarly, `∞ isa Real`.

## Similar packages

This package is meant to eventually replace [Infinity.jl](https://github.com/cjdoris/Infinity.jl) and the definitions of `∞` in [InfiniteArrays.jl](https://github.com/JuliaArrays/InfiniteArrays.jl). We do not yet support Infinity.jl's notions of `InfExtendedReal` but we hope to add this soon.