"""
    @archimedean T₁ T₂ ...
    @archimedean ±∞ T₁ T₂ ...
    @archimedean +∞ T₁ T₂ ...
    @archimedean -∞ T₁ T₂ ...

Declare each listed type `T` to model an Archimedean scale, so that its values lie
strictly between `-∞` and `+∞`.

The declaration defines `isless`, `+` and `-` between `T` and the real infinities `∞`, `+∞`
and `-∞`. `Base` builds `<`, `≤`, `>`, `≥`, `min`, `max` and sorting on `isless`.
On a scale unbounded in both directions, an infinity absorbs every value of `T`, so a sum or
difference is the infinity itself, negated when it is subtracted: `x + ∞ == ∞ ± x == ∞` and
`x - ∞ == -∞ ± x == -∞`.
For a position such as a `Date`, the `∞` in `x + ∞` is an infinitely long step, and the `∞`
in `∞ - x` is the end of the scale. Multiplication and division are left out, because a
position cannot be meaningfully scaled.

List types as separate macro arguments. The direction is optional. Without it, both ends are
unbounded.

Each type must define `Base.isless(x::T, y::T)` as a strict total order consistent with `isequal`.

# When to declare

Declare `T` if its values are magnitudes or positions on a scale that is unbounded in both
directions and on which adding the same positive step repeatedly gets past any value. A
magnitude has a natural zero, like a `Period`. A position is measured from an arbitrary origin,
like a `Date`, and the difference of two positions is a magnitude, as the difference of two
`Date`s is a `Period`. In measurement theory, a magnitude scale is a ratio scale, and a position
scale is an interval scale. For magnitudes, the condition on steps is the Archimedean property:
for every `x > 0` and every `y`, some multiple `n*x` exceeds `y`. For positions, it holds for
their differences. Hölder's theorem shows that, up to the choice of unit and origin, such a
scale fits into the real line. So `-∞` lies below and `+∞` above all of its values in every unit.

# Scales bounded on one side

Some scales end at one of their own values on one side and go on without limit on the other.
Absolute temperature, for example, ends at absolute zero. Hölder's theorem also covers such
scales and fits them into a half-line. Then only one infinity lies on the unbounded side, and
the optional direction names it. Comparisons are the same as on a scale unbounded in both
directions. For `@archimedean +∞ T`, `-∞ < x` still holds for every value `x`. `+` and `-` are
defined only where the result is `+∞`: `x + ∞`, `∞ + x` and `∞ - x` give `∞`, and `x - (-∞)`
gives `+∞`. `x - ∞`, `x + (-∞)`, `-∞ + x` and `-∞ - x` have no method, because their result
`-∞` would lie below the end of the scale. `@archimedean -∞ T` mirrors this.

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
julia> struct Meter
           value::Rational{Int}
       end

julia> Base.isless(a::Meter, b::Meter) = isless(a.value, b.value)

julia> Infinities.@archimedean Meter

julia> -∞ < Meter(3//2) < ∞
true

julia> max(Meter(3//2), ∞)
∞

julia> Meter(3//2) - ∞
-∞
```
"""
macro archimedean(direction::Union{Symbol, Expr}, args::Union{Symbol, Expr}...)
    direction === :∞ && throw(ArgumentError("the direction has to be `±∞`, `+∞` or `-∞`"))
    unbounded, negated_unbounded =
        direction == :(+∞) ? (PositiveRealInfinities, NegativeInfinity) :
        direction == :(-∞) ? (NegativeInfinity, PositiveRealInfinities) :
                             (AllRealInfinities, AllRealInfinities)
    types = direction in (:(±∞), :(+∞), :(-∞)) ? args : (direction, args...)
    isempty(types) && throw(ArgumentError("`@archimedean` needs at least one type after the direction"))
    any(type -> type in (:(±∞), :(+∞), :(-∞), :∞), types) &&
        throw(ArgumentError("an infinity selector can appear only as the first argument"))
    declarations = map(type -> archimedean(esc(type), unbounded, negated_unbounded), types)
    Expr(:block, declarations..., nothing)
end

"""
    archimedean(T::Expr, unbounded::Type, negated_unbounded::Type) -> Expr

Build the expression defining comparison and arithmetic methods between the scale and
its infinities.

`unbounded` is the type accepted for infinity operands at unbounded ends of the scale.
`negated_unbounded` is the type accepted for subtrahends whose negatives are those ends.
"""
function archimedean(T::Expr, unbounded::Type, negated_unbounded::Type)
    quote
        Base.isless(x::AllRealInfinities, ::$T) = signbit(x)
        Base.isless(x::$T, y::AllRealInfinities) = !isless(y, x)
        Base.:+(::$T, y::$unbounded) = y
        Base.:+(x::$unbounded, ::$T) = x
        Base.:-(x::$unbounded, ::$T) = x
        Base.:-(::$T, y::$negated_unbounded) = -y
    end
end
