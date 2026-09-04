module Infinities

import Base: angle, isone, iszero, isinf, isfinite, abs, one, oneunit, zero, isless, inv,
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

represents something that is undefined, for example, `0 * ∞`.
"""
struct NotANumber <: Number end


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

# angle is π*a where a is (false==0) and (true==1)

"""
ComplexInfinity(signbit)

represents an infinity in the complex plane with the angle
specified by `π * signbit`. The use of the name `signbit` is
for consistency with `RealInfinity`.
"""
struct ComplexInfinity{T<:Real} <: Number
    signbit::T
end

ComplexInfinity{T}() where T = ComplexInfinity(zero(T))
ComplexInfinity() = ComplexInfinity{Bool}()
ComplexInfinity{T}(::Infinity) where T<:Real = ComplexInfinity{T}()
ComplexInfinity(::Infinity) = ComplexInfinity()
ComplexInfinity{T}(x::RealInfinity) where T<:Real = ComplexInfinity{T}(signbit(x))
ComplexInfinity(x::RealInfinity) = ComplexInfinity(signbit(x))
ComplexInfinity{T}(x::ComplexInfinity) where T<:Real = ComplexInfinity(T(x.signbit)) # ambiguity fix

signbit(y::ComplexInfinity) = mod(y.signbit, 2) == 1

convert(::Type{ComplexInfinity{T}}, ::Infinity) where T = ComplexInfinity{T}()
convert(::Type{ComplexInfinity}, ::Infinity) = ComplexInfinity()
convert(::Type{ComplexInfinity{T}}, x::RealInfinity) where T = ComplexInfinity{T}(x)
convert(::Type{ComplexInfinity}, x::RealInfinity) = ComplexInfinity(x)


sign(y::ComplexInfinity{<:Integer}) = mod(y.signbit, 2) == 0 ? 1 : -1
sign(y::ComplexInfinity) = cispi(y.signbit)
angle(x::ComplexInfinity) = π*x.signbit
abs(::ComplexInfinity) = ∞
conj(y::ComplexInfinity{<:Integer}) = y # an integer factor points along the real axis
conj(y::ComplexInfinity) = ComplexInfinity(mod(-y.signbit, 2))

# An exact zero has to stay finite, `Inf * 0` being a `NaN`.
@inline _ray(c) = iszero(c) ? c : copysign(Inf, c)
# `Complex` reaches only the eight rays of its two saturating parts, so the direction lands on the nearest of them.
function float(x::ComplexInfinity)
    s, c = sincospi(x.signbit)
    complex(_ray(c), _ray(s))
end

show(io::IO, x::ComplexInfinity) = print(io, "exp($(x.signbit)*im*π)∞")

one(::Type{<:ComplexInfinity}) = one(ComplexF64)
oneunit(::Type{<:ComplexInfinity}) = oneunit(ComplexF64)
oneunit(::ComplexInfinity) = oneunit(ComplexF64)
zero(::ComplexInfinity) = zero(ComplexF64)
zero(::Type{<:ComplexInfinity}) = zero(ComplexF64)


# `isequal` implies equal hashes, so the infinities have to hash like the float
# infinities they compare equal to. The interface requires implementing `hash(x, h::UInt)`.

Base.hash(::Infinity, h::UInt)::UInt = hash(Inf, h)
Base.hash(::PositiveInfinity, h::UInt)::UInt = hash(Inf, h)
Base.hash(::NegativeInfinity, h::UInt)::UInt = hash(-Inf, h)

# Equality of ComplexInfinity is equality of the angle, hence so is the hash.
function Base.hash(x::ComplexInfinity, h::UInt)::UInt
    θ = angle(x)
    θ == angle(PositiveInfinity()) && return hash(Inf, h)
    θ == angle(NegativeInfinity()) && return hash(-Inf, h)
    hash(ComplexInfinity, hash(θ, h))
end


include("cardinality.jl")
include("interface.jl")
include("compare.jl")
include("algebra.jl")
include("ambiguities.jl")
end # module
