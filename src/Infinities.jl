module Infinities

import Base: angle, isone, iszero, isinf, isfinite, isnan, isreal, abs, one, oneunit, zero, isless, isequal, inv,
                +, -, *, /, ^, ==, <, ≤, >, ≥, fld, cld, div, mod, rem, divrem, min, max,
                sign, signbit, isapprox,
                string, show, promote_rule, convert, getindex, tryparse, conj,
                isinteger, round, floor, ceil, trunc, float,
                Bool, Integer

export ∞,  ℵ₀,  ℵ₁, RealInfinity, ComplexInfinity, InfiniteCardinal, NotANumber, PositiveInfinity, NegativeInfinity
# The following is commented out for now to avoid conflicts with Infinity.jl
# export Infinity

"""
    NotANumber()

Construct the undefined value, for example the result of `0 * ∞`.

Every float type has a `NaN` of its own. This one belongs to none of them.
"""
struct NotANumber <: Real end

(::Type{T})(::NotANumber) where {T<:AbstractFloat} = T(NaN)
float(::NotANumber) = NaN
Base.hash(::NotANumber, h::UInt)::UInt = hash(NaN, h)


"""
   Infinity()

represents the positive real infinite.
"""
struct Infinity <: Real end

const ∞ = Infinity()

show(io::IO, ::Infinity) = print(io, "∞")
string(::Infinity) = "∞"

_convert(::Type{Float64}, ::Infinity) = Inf64
_convert(::Type{Float32}, ::Infinity) = Inf32
_convert(::Type{Float16}, ::Infinity) = Inf16
_convert(::Type{T}, ::Infinity) where {T<:Real} = convert(T, Inf)::T
(::Type{T})(x::Infinity) where {T<:Real} = _convert(T, x)

sign(y::Infinity) = 1
angle(x::Infinity) = 0
signbit(::Infinity) = false

one(::Type{Infinity}) = 1
oneunit(::Type{Infinity}) = 1
oneunit(::Infinity) = 1
zero(::Infinity) = 0
zero(::Type{Infinity}) = 0

abstract type RealInfinity <: Real end
struct PositiveInfinity <: RealInfinity end
struct NegativeInfinity <: RealInfinity end

signbit(::PositiveInfinity) = false
signbit(::NegativeInfinity) = true
one(::RealInfinity) = 1.0

RealInfinity() = PositiveInfinity()
RealInfinity(::Infinity) = PositiveInfinity()
RealInfinity(x::RealInfinity) = x
RealInfinity(x::Bool) = ifelse(x, NegativeInfinity(), PositiveInfinity())
PositiveInfinity(::Infinity) = PositiveInfinity() # otherwise the generic `(::Type{T})(::Infinity) where T<:Real` would route through `Inf`

_convert(::Type{Float16}, x::RealInfinity) = sign(x)*Inf16
_convert(::Type{Float32}, x::RealInfinity) = sign(x)*Inf32
_convert(::Type{Float64}, x::RealInfinity) = sign(x)*Inf64
_convert(::Type{T}, x::RealInfinity) where {T<:Real} = sign(x)*convert(T, Inf)
(::Type{T})(x::RealInfinity) where {T<:Real} = _convert(T, x)

for Typ in (RealInfinity, Infinity)
    @eval Bool(x::$Typ) = throw(InexactError(:Bool, Bool, x)) # ambiguity fix
end

sign(y::RealInfinity) = 1-2signbit(y)
angle(x::RealInfinity) = π*signbit(x)

string(y::RealInfinity) = signbit(y) ? "-∞" : "+∞"
show(io::IO, y::RealInfinity) = print(io, string(y))

Base.to_index(i::RealInfinity) = convert(Integer, i)

one(::Type{RealInfinity}) = 1.0
oneunit(::Type{RealInfinity}) = 1.0
oneunit(::RealInfinity) = 1.0
zero(::RealInfinity) = 0.0
zero(::Type{RealInfinity}) = 0.0


#######
# ComplexInfinity
#######

"""
    ComplexInfinity(turns::UInt64)
    ComplexInfinity(; halfturns::Real = 0)

Construct an infinity in the complex plane, pointing in a direction held as a count of
`2^-64` turns.

The count wraps at a full turn, so the stored `UInt64` and the directions are bijective.
`0x0` points along the positive real axis. Values increase counterclockwise.
`0x8000000000000000` points along the negative real axis. Use `reinterpret(UInt64, x)` to
read out the exact value.

Multiplying by `∞` takes the direction from the other operand, which usually reads better
than naming an angle:

    im*∞            # cispi(0.5)∞
    (1+im)*∞        # cispi(0.25)∞
    exp(im*π/4)*∞   # the same direction again

Those forms and the `halfturns` keyword go through `angle`, so they round. It is exact on
the axes and at a quarter turn, but `exp(im*π/8)*∞` lands 256 counts past an eighth turn.
Provide the `UInt64` when you have an off-axis value where accuracy matters.
"""
struct ComplexInfinity <: Number
    turns::UInt64
    ComplexInfinity(turns::UInt64) = new(turns)
end

# A full turn fills the `UInt64` range, so a half turn is 2^63 units.
const _HALFTURN = UInt64(2)^63 # the negative real axis
# `_turns` and `_halfturns` are inverse: half turns in, count out, and back again.
# `mod` returns 2 itself for a tiny negative angle, since 2 + x rounds back to 2. A full
# turn is the direction zero. Testing `== 2` rather than `< 2` still lets `NaN` throw.
@inline _turns(halfturns::Real) = round(UInt64, (h = mod(halfturns, 2); h == 2 ? zero(h) : h) * 0x1p63)
# Scaling by 2^63 needs 63 bits beyond the numerator, so `Int128` has room for any `Int64`.
_turns(halfturns::Rational) = round(BigInt, mod(halfturns, 2) * big(2)^63) % UInt64
_turns(halfturns::Rational{<:Base.BitInteger64}) =
    round(Int128, mod(halfturns, 2) * Int128(2)^63) % UInt64
# `Base` puts an angle in `(-π, π]`, so past the half turn the count reads as negative.
@inline _halfturns(x::ComplexInfinity) =
    x.turns == _HALFTURN ? 1.0 : reinterpret(Int64, x.turns) / 0x1p63

ComplexInfinity(; halfturns::Real = 0) = ComplexInfinity(_turns(halfturns))
ComplexInfinity(::Infinity) = ComplexInfinity()
ComplexInfinity(x::RealInfinity) = ComplexInfinity(_directionof(x))
ComplexInfinity(x::ComplexInfinity) = x

signbit(y::ComplexInfinity) = y.turns == _HALFTURN
isreal(y::ComplexInfinity) = iszero(y.turns) || signbit(y)

# `Base` converts a `Complex` to a `Real` the same way, and throws the same error off the axis.
RealInfinity(x::ComplexInfinity) = isreal(x) ? RealInfinity(signbit(x)) :
                                   throw(InexactError(:RealInfinity, RealInfinity, x))

convert(::Type{ComplexInfinity}, ::Infinity) = ComplexInfinity()
convert(::Type{ComplexInfinity}, x::RealInfinity) = ComplexInfinity(x)


sign(y::ComplexInfinity) = cispi(_halfturns(y))
angle(x::ComplexInfinity) = _halfturns(x) * π
abs(::ComplexInfinity) = ∞
conj(y::ComplexInfinity) = ComplexInfinity(-y.turns)

# An exact zero has to stay finite, `Inf * 0` being a `NaN`.
@inline _ray(c) = iszero(c) ? c : copysign(Inf, c)
# `Complex` reaches only the eight rays of its two saturating parts, so the direction lands on the nearest of them.
function float(x::ComplexInfinity)
    s, c = sincospi(_halfturns(x))
    complex(_ray(c), _ray(s))
end

# The readable form names an angle, which recovers most counts but not all, so it is used
# only where reading it back gives the same direction.
function show(io::IO, x::ComplexInfinity)
    h = _halfturns(x)
    _directionof(cispi(h)) == x.turns ? print(io, "cispi($h)∞") :
                                    print(io, "ComplexInfinity(", repr(x.turns), ")")
end

one(::Type{ComplexInfinity}) = one(ComplexF64)
oneunit(::Type{ComplexInfinity}) = oneunit(ComplexF64)
oneunit(::ComplexInfinity) = oneunit(ComplexF64)
zero(::ComplexInfinity) = zero(ComplexF64)
zero(::Type{ComplexInfinity}) = zero(ComplexF64)


# `isequal` implies equal hashes, so the infinities have to hash like the float
# infinities they compare equal to. The interface requires implementing `hash(x, h::UInt)`.

Base.hash(::Infinity, h::UInt)::UInt = hash(Inf, h)
Base.hash(::PositiveInfinity, h::UInt)::UInt = hash(Inf, h)
Base.hash(::NegativeInfinity, h::UInt)::UInt = hash(-Inf, h)

# The two real directions have to hash like the real infinities they compare equal to.
function Base.hash(x::ComplexInfinity, h::UInt)::UInt
    iszero(x.turns) && return hash(Inf, h)
    x.turns == _HALFTURN && return hash(-Inf, h)
    hash(ComplexInfinity, hash(x.turns, h))
end


include("cardinality.jl")
include("interface.jl")
include("compare.jl")
include("algebra.jl")
include("ambiguities.jl")
end # module
