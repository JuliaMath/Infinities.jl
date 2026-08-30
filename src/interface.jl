const AllInfinities = Union{Infinity, RealInfinity, ComplexInfinity, InfiniteCardinal}
const AllRealInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}}
const IntegerInfinities = Union{Infinity, RealInfinity, ComplexInfinity{<:Integer}, InfiniteCardinal}
const ExtendedComplex{T} = Union{Complex{T}, ComplexInfinity{T}}

iszero(::AllInfinities) = false
isinf(::AllInfinities) = true
isfinite(::AllInfinities) = false

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