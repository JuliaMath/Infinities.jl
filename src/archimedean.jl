"""
    @archimedean T₁ T₂ ...
    @archimedean direction T₁ T₂ ...

Declare each listed type `T` to model positions on an Archimedean scale, so that its values lie
strictly between `-∞` and `+∞`.

Each type must define `Base.isless(x::T, y::T)` as a strict total order consistent with `isequal`.
The optional direction, `±∞`, `+∞` or `-∞`, names the unbounded ends of the scale. Without it,
both ends are unbounded. Declare magnitudes, which can also be scaled, with
`@archimedean_magnitude`.

The declaration defines `isless`, `+` and `-` between `T` and the real infinities `∞`, `+∞`
and `-∞`. `Base` builds `<`, `≤`, `>`, `≥`, `min`, `max` and sorting on `isless`.
On a scale unbounded in both directions, an infinity absorbs every value of `T`, so a sum or
difference is the infinity itself, negated when it is subtracted: `x + ∞ == ∞ ± x == ∞` and
`x - ∞ == -∞ ± x == -∞`.

# When to declare

Declare `T` if adding the same positive step to its values repeatedly gets past any value.
A magnitude is an amount with a natural zero, like a `Period`. For magnitudes, the condition is the
Archimedean property: for every `x > 0` and every `y`, some multiple `n*x` exceeds `y`.
A position is measured from an arbitrary origin, like a `Date`. Positions need not form a group
themselves, as adding two `Date`s has no meaning. It suffices that their differences are
magnitudes with the Archimedean property, as the differences of `Date`s are `Period`s. In
algebraic terms, the positions form a torsor of the group of their differences. In measurement
theory, magnitudes form a ratio scale and positions an interval scale. For a position, the `∞`
in `x + ∞` is an infinitely long step, and the `∞` in `∞ - x` is the end of the scale.

Hölder's theorem shows that, up to the choice of unit and origin, such a scale fits into the real
line, or into a half-line if it is bounded on one side. So `-∞` lies below and `+∞` above all of
its values in every unit.

# Scales bounded on one side

Some scales end at one of their own values on one side and go on without limit on the other,
like absolute temperature, which ends at absolute zero. The direction names the infinity on the
unbounded side. Comparisons stay the same, but `+` and `-` are defined only where the result is
that infinity. For `@archimedean +∞ T`, `x + ∞`, `∞ + x` and `∞ - x` give `∞`, and `x - (-∞)`
gives `+∞`. `x - ∞`, `x + (-∞)`, `-∞ + x` and `-∞ - x` throw a `MethodError`, because their
result `-∞` would lie below the end of the scale. `@archimedean -∞ T` mirrors this. A
subtype of `RealInfinity` acts as the built-in infinity of its sign.

# The model decides, not the representation

Declare a type for what it models, even where its representation falls short. The same reasoning
lets every `Real` compare with `∞`. `Int8` arithmetic wraps from `127` to `-128`, yet `Base`
orders `Int8` like the integers and treats the wrap as a limit of the 8-bit representation.
`Float64` models the real numbers, although the gaps between its values grow with their size.
Likewise, a `Year` models a number of years without upper limit, although its count stops at
`typemax(Int64)`.

Do not declare

- a cyclic type, where wrapping around is part of the meaning. A time of day (`Dates.Time`)
  returns to the same value after 24 hours, and a weekday after seven days.
- an order without a fixed step, such as strings in lexicographic order. Infinitely many
  strings lie between `"a"` and `"b"`, and appending the same characters again and again
  never gets from `"a"` past `"b"`.
- a partial order, in which some values are not comparable, such as sets ordered by inclusion.
- a type with infinite or unordered values of its own, such as `NaN`. The declaration would
  still order them below `∞`.
- a subtype of `Real`, which already compares with the infinities.

Call the macro at top level, either in the module that owns `T` or in a package extension that
adds support for `T`. Anywhere else, the new methods are type piracy.

# Examples

```julia
julia> struct Milepost
           miles::Rational{Int}
       end

julia> Base.isless(a::Milepost, b::Milepost) = isless(a.miles, b.miles)

julia> Infinities.@archimedean Milepost

julia> -∞ < Milepost(3//2) < ∞
true

julia> max(Milepost(3//2), ∞)
∞

julia> Milepost(3//2) - ∞
-∞
```
"""
macro archimedean(direction::Union{Symbol, Expr}, args::Union{Symbol, Expr}...)
    archimedean(false, direction, args...)
end

"""
    @archimedean_magnitude T₁ T₂ ...
    @archimedean_magnitude direction T₁ T₂ ...

Declare each listed type `T` to model magnitudes on an Archimedean scale, which can also be scaled
by the infinities.

A magnitude is an amount, such as a length or a duration. It has a natural zero, two magnitudes
add to another one, and a real factor scales it. The difference of two positions is a magnitude,
as the difference of two `Date`s is a `Period`.

The declaration defines everything `@archimedean` defines, with the same requirements and
direction. In addition, each type must define `Base.zero(::Type{T})`.

The declaration also defines `*`, `/`, `div`, `rem` and `mod` with the infinities as scalar
factors. Products follow the sign rules of `Real` numbers, with `NotANumber()` for a zero
magnitude. Dividing by an infinity gives `zero(T)` with remainder `x`. As for `Real` numbers,
`mod(x, y)` throws an `ArgumentError` if `x` and `y` have opposite signs, because the result would
be unbounded. On a scale bounded on one side, which ends at zero, only `∞` and `+∞` are factors,
because a negative one would leave the scale.

# Examples

```julia
julia> struct Meter
           value::Rational{Int}
       end

julia> Base.isless(a::Meter, b::Meter) = isless(a.value, b.value)

julia> Base.zero(::Type{Meter}) = Meter(0)

julia> Infinities.@archimedean_magnitude Meter

julia> Meter(3//2) - ∞
-∞

julia> Meter(-3//2) * ∞
-∞

julia> Meter(3//2) / ∞
Meter(0//1)
```
"""
macro archimedean_magnitude(direction::Union{Symbol, Expr}, args::Union{Symbol, Expr}...)
    archimedean(true, direction, args...)
end

"""
    archimedean(magnitude::Bool, direction, args...) -> Expr

Build the declarations of `@archimedean`, or of `@archimedean_magnitude` if `magnitude`, for the
macro arguments `direction` and `args...`.
"""
function archimedean(magnitude::Bool, direction::Union{Symbol, Expr}, args::Union{Symbol, Expr}...)
    direction === :∞ && throw(ArgumentError("the direction has to be `±∞`, `+∞` or `-∞`"))
    onesided = direction in (:(+∞), :(-∞))
    unbounded, negated_unbounded, scalars =
        direction == :(+∞) ? (PositiveRealInfinities, NegativeInfinity, PositiveRealInfinities) :
        direction == :(-∞) ? (NegativeInfinity, PositiveRealInfinities, PositiveRealInfinities) :
                             (AllRealInfinities, AllRealInfinities, AllRealInfinities)
    types = direction in (:(±∞), :(+∞), :(-∞)) ? args : (direction, args...)
    if isempty(types)
        name = magnitude ? "@archimedean_magnitude" : "@archimedean"
        throw(ArgumentError("`$name` needs at least one type after the direction"))
    end
    any(type -> type in (:(±∞), :(+∞), :(-∞), :∞), types) &&
        throw(ArgumentError("an infinity selector can appear only as the first argument"))
    definitions = map(types) do type
        T = esc(type)
        position = positioning(T, unbounded, negated_unbounded)
        definition = magnitude ? Expr(:block, position, scaling(T, scalars)) : position
        onesided ? Expr(:block, definition, redispatching(T, magnitude)) : definition
    end
    Expr(:block, definitions..., nothing)
end

"""
    positioning(T::Expr, unbounded::Type, negated_unbounded::Type) -> Expr

Build the expression defining comparison and arithmetic methods between the scale and
its infinities.

`unbounded` is the type accepted for infinity operands at unbounded ends of the scale.
`negated_unbounded` is the type accepted for subtrahends whose negatives are those ends.
"""
function positioning(T::Expr, unbounded::Type, negated_unbounded::Type)
    quote
        Base.isless(x::AllRealInfinities, ::$T) = signbit(x)
        Base.isless(x::$T, y::AllRealInfinities) = !isless(y, x)
        Base.:+(::$T, y::$unbounded) = y
        Base.:+(x::$unbounded, ::$T) = x
        Base.:-(x::$unbounded, ::$T) = x
        Base.:-(::$T, y::$negated_unbounded) = -y
    end
end

"""
    scaling(T::Expr, scalars::Type) -> Expr

Build the expression defining multiplication, division and remainders of the magnitudes with
the infinite scalars of type `scalars`.
"""
function scaling(T::Expr, scalars::Type)
    quote
        Base.:*(x::$T, y::$scalars) = _scale(x, y)
        Base.:*(x::$scalars, y::$T) = _scale(y, x)
        Base.:/(x::$T, ::$scalars) = zero(typeof(x))
        Base.div(x::$T, ::$scalars, ::RoundingMode) = zero(typeof(x))
        Base.rem(x::$T, ::$scalars) = x
        Base.mod(x::$T, y::$scalars) = _modulo(x, y)
    end
end

"""
    redispatching(T::Expr, magnitude::Bool) -> Expr

Build the expression defining the methods of a scale bounded on one side that pass an operand
of a user-defined subtype of `RealInfinity` on as the built-in infinity of the same sign, as the
sign of such a subtype need not be part of its type.
"""
function redispatching(T::Expr, magnitude::Bool)
    position = quote
        Base.:+(x::$T, y::RealInfinity) = _redispatch(+, x, y)
        Base.:+(x::RealInfinity, y::$T) = _redispatch(+, x, y)
        Base.:-(x::RealInfinity, y::$T) = _redispatch(-, x, y)
        Base.:-(x::$T, y::RealInfinity) = _redispatch(-, x, y)
    end
    magnitude || return position
    quote
        $position
        Base.:*(x::$T, y::RealInfinity) = _redispatch(*, x, y)
        Base.:*(x::RealInfinity, y::$T) = _redispatch(*, x, y)
        Base.:/(x::$T, y::RealInfinity) = _redispatch(/, x, y)
        Base.div(x::$T, y::RealInfinity, r::RoundingMode) = _redispatch(div, x, y, r)
        Base.rem(x::$T, y::RealInfinity) = _redispatch(rem, x, y)
        Base.mod(x::$T, y::RealInfinity) = _redispatch(mod, x, y)
    end
end

# Built-in infinities reach here only with the sign that leaves the scale.
function _redispatch(f, args...)
    canonical = map(a -> a isa RealInfinity ? RealInfinity(signbit(a)) : a, args)
    canonical === args && throw(MethodError(f, args))
    f(canonical...)
end

# As for a `Real` factor, a zero magnitude times an infinity is undefined.
function _scale(x, y::AllRealInfinities)
    z = zero(typeof(x))
    isless(z, x) ? RealInfinity(signbit(y)) : isless(x, z) ? RealInfinity(!signbit(y)) : NotANumber()
end

# A magnitude has no signed zero, so its zero is bounded for either sign of the divisor.
function _modulo(x, y::AllRealInfinities)
    z = zero(typeof(x))
    (signbit(y) ? isless(z, x) : isless(x, z)) && throw(ArgumentError("mod($x,$y) is unbounded"))
    x
end
