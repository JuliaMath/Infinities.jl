module Infinities

import Base: angle, isone, iszero, isinf, isfinite, abs, one, oneunit, zero, isless, inv,
                +, -, *, ==, <, ≤, >, ≥, fld, cld, div, mod, min, max, sign, signbit,
                string, show, promote_rule, convert, getindex

export ∞,  ℵ₀,  ℵ₁, RealInfinity, ComplexInfinity, InfiniteCardinal, NotANumber
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

struct RealInfinity <: Real
    signbit::Bool
end

RealInfinity() = RealInfinity(false)
RealInfinity(::Infinity) = RealInfinity()
RealInfinity(x::RealInfinity) = x

_convert(::Type{Float16}, x::RealInfinity) = sign(x)*Inf16
_convert(::Type{Float32}, x::RealInfinity) = sign(x)*Inf32
_convert(::Type{Float64}, x::RealInfinity) = sign(x)*Inf64
_convert(::Type{T}, x::RealInfinity) where {T<:Real} = sign(x)*convert(T, Inf)
(::Type{T})(x::RealInfinity) where {T<:Real} = _convert(T, x)

for Typ in (RealInfinity, Infinity)
    @eval Bool(x::$Typ) = throw(InexactError(:Bool, Bool, x)) # ambiguity fix
end

signbit(y::RealInfinity) = y.signbit
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
ComplexInfinity{T}(x::ComplexInfinity) where T<:Real = ComplexInfinity(T(signbit(x))) # ambiguity fix

signbit(y::ComplexInfinity{Bool}) = y.signbit
signbit(y::ComplexInfinity{<:Integer}) = !(mod(y.signbit,2) == 0)
signbit(y::ComplexInfinity) = y.signbit

convert(::Type{ComplexInfinity{T}}, ::Infinity) where T = ComplexInfinity{T}()
convert(::Type{ComplexInfinity}, ::Infinity) = ComplexInfinity()
convert(::Type{ComplexInfinity{T}}, x::RealInfinity) where T = ComplexInfinity{T}(x)
convert(::Type{ComplexInfinity}, x::RealInfinity) = ComplexInfinity(x)


sign(y::ComplexInfinity{<:Integer}) = mod(y.signbit,2) == 0 ? 1 : -1
angle(x::ComplexInfinity) = π*x.signbit

show(io::IO, x::ComplexInfinity) = print(io, "exp($(x.signbit)*im*π)∞")

one(::Type{<:ComplexInfinity}) = one(ComplexF64)
oneunit(::Type{<:ComplexInfinity}) = oneunit(ComplexF64)
oneunit(::ComplexInfinity) = oneunit(ComplexF64)
zero(::ComplexInfinity) = zero(ComplexF64)
zero(::Type{<:ComplexInfinity}) = zero(ComplexF64)

Base.hash(::Infinity) = 0x020113134b21797f # made up


include("cardinality.jl")
include("interface.jl")
include("compare.jl")
include("algebra.jl")
include("ambiguities.jl")
end # module
