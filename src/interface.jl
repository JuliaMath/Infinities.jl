const AllInfinities = Union{Infinity, RealInfinity, ComplexInfinity, InfiniteCardinal}
const AllRealInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}}
const IntegerInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}, InfiniteCardinal}
const ExtendedComplex{T} = Union{Complex{T}, ComplexInfinity{T}}

iszero(::AllInfinities) = false
isinf(::AllInfinities) = true
isfinite(::AllInfinities) = false

# `NotANumber` is no value at all, so it is neither finite nor infinite.
isnan(::NotANumber) = true
isinf(::NotANumber) = false
isfinite(::NotANumber) = false
iszero(::NotANumber) = false
isone(::NotANumber) = false
isinteger(::NotANumber) = false
signbit(::NotANumber) = false

# Undefined wins against every second argument, so it needs a method wherever `Base` or this
# package owns a slot of its own. A narrower slot simply beats a wider one. `Number` looks
# redundant against the types here, but without it a foreign `Number` reaches `Base`'s
# promoting fallback instead.
const NotANumberRivals = (Number, Real, AbstractFloat, AbstractIrrational, AllInfinities,
                          IntegerInfinities, RealInfinity,
                          InfiniteCardinal)
# A complex operand makes the undefined result complex, as it does over the floats.
const NotANumberComplexRivals = (Complex, Complex{Bool}, ComplexInfinity)

# `InfiniteCardinal` is absent because `Base` already answers `true` for it through `Integer`.
isinteger(::Union{Infinity, RealInfinity, ComplexInfinity}) = false
for f in (:round, :floor, :ceil, :trunc)
    @eval $f(x::Union{AllInfinities, NotANumber}; kwargs...) = x
end
round(x::Union{AllInfinities, NotANumber}, ::RoundingMode; kwargs...) = x

# `Infinity` is positive, so it has no common type with `NegativeInfinity` (as is already the case for `PositiveInfinity`).
promote_rule(::Type{Infinity}, ::Type{PositiveInfinity}) = PositiveInfinity
promote_rule(::Type{Infinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
promote_rule(::Type{<:RealInfinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
promote_rule(::Type{ComplexInfinity{T}}, ::Type{<:RealInfinity}) where T<:Integer = ComplexInfinity{T}
promote_rule(::Type{ComplexInfinity{T}}, ::Type{ComplexInfinity{S}}) where {T, S} = ComplexInfinity{promote_type(T, S)}

function tryparse(::Type{NegativeInfinity}, s::AbstractString)
    i = findfirst(!isspace, s)
    (isnothing(i) || s[i] != '-') && return nothing
    i = findnext(!isspace, s, nextind(s, i)) # A space can have multiple codeunits
    (isnothing(i) || s[i] != '∞') && return nothing
    return findnext(!isspace, s, nextind(s, i)) |> isnothing ? NegativeInfinity() : nothing
end

function tryparse(::Type{PositiveInfinity}, s::AbstractString)
    i = findfirst(!isspace, s)
    isnothing(i) && return nothing
    if s[i] == '+'
        i = findnext(!isspace, s, nextind(s, i)) # A space can have multiple codeunits
        isnothing(i) && return nothing
    end
    s[i] == '∞' || return nothing
    return findnext(!isspace, s, nextind(s, i)) |> isnothing ? PositiveInfinity() : nothing
end

function tryparse(::Type{RealInfinity}, s::AbstractString)
    negative = tryparse(NegativeInfinity, s)
    isnothing(negative) || return negative
    return tryparse(PositiveInfinity, s)
end