module Infinities

import Base: angle, isone, iszero, isinf, isfinite, abs, one, oneunit, zero, isless,
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

isinf(::Infinity) = true
isfinite(::Infinity) = false

+(::Infinity, ::Infinity) = ∞
+(::Number, y::Infinity) = ∞
+(::Infinity, ::Number) = ∞

+(::Integer, y::Infinity) = ∞
+(::Infinity, ::Integer) = ∞

# ⊻ is xor
*(::Infinity) = ∞
*(::Infinity, ::Infinity) = ∞



for OP in (:fld,:cld,:div)
  @eval begin
    $OP(::Infinity, ::Real) = ∞
    $OP(::Infinity, ::Infinity) = NotANumber()
  end
end

div(::T, ::Infinity) where T<:Real = zero(T)
fld(x::T, ::Infinity) where T<:Real = signbit(x) ? -one(T) : zero(T)
cld(x::T, ::Infinity) where T<:Real = signbit(x) ? zero(T) : one(T)

mod(::Infinity, ::Real) = NotANumber()
function mod(x::Real, ::Infinity)
    x ≥ 0 || throw(ArgumentError("mod(x,∞) is unbounded for x < 0"))
    x
end



struct RealInfinity <: Real
    signbit::Bool
end

RealInfinity() = RealInfinity(false)
RealInfinity(::Infinity) = RealInfinity()
RealInfinity(x::RealInfinity) = x

-(::Infinity) = RealInfinity(true)
+(::Infinity) = RealInfinity()

isinf(::RealInfinity) = true
isfinite(::RealInfinity) = false

promote_rule(::Type{Infinity}, ::Type{RealInfinity}) = RealInfinity
_convert(::Type{RealInfinity}, ::Infinity) = RealInfinity(false)

_convert(::Type{Float16}, x::RealInfinity) = sign(x)*Inf16
_convert(::Type{Float32}, x::RealInfinity) = sign(x)*Inf32
_convert(::Type{Float64}, x::RealInfinity) = sign(x)*Inf64
_convert(::Type{T}, x::RealInfinity) where {T<:Real} = sign(x)*convert(T, Inf)
(::Type{T})(x::RealInfinity) where {T<:Real} = _convert(T, x)

for Typ in (RealInfinity, Infinity)
    @eval Bool(x::$Typ) = throw(InexactError(:Bool, Bool, x))
end

signbit(y::RealInfinity) = y.signbit
sign(y::RealInfinity) = 1-2signbit(y)
angle(x::RealInfinity) = π*signbit(x)

mod(::RealInfinity, ::Real) = NotANumber()
function mod(x::Real, y::RealInfinity)
    signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
    x
end

string(y::RealInfinity) = signbit(y) ? "-∞" : "+∞"
show(io::IO, y::RealInfinity) = print(io, string(y))

==(x::RealInfinity, y::Infinity) = !x.signbit
==(y::Infinity, x::RealInfinity) = !x.signbit
==(x::RealInfinity, y::RealInfinity) = x.signbit == y.signbit


for Typ in (:Number, :Real, :Integer, :AbstractFloat)
    @eval begin
        +(::$Typ, y::RealInfinity) = y
        +(y::RealInfinity, ::$Typ) = y
        function *(a::$Typ, y::RealInfinity)
            iszero(a) && throw(ArgumentError("Cannot multiply $a * $y"))
            a > 0 ? y : (-y)
        end
    end
end

≤(::RealInfinity, ::Infinity) = true
≤(::Infinity, s::RealInfinity) = !signbit(s)

-(y::RealInfinity) = RealInfinity(!y.signbit)

function +(x::RealInfinity, y::RealInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
    x
end

+(x::RealInfinity, y::Infinity) = x+RealInfinity(y)
+(x::Infinity, y::RealInfinity) = RealInfinity(x)+y

# ⊻ is xor
*(a::RealInfinity, b::RealInfinity) = RealInfinity(signbit(a) ⊻ signbit(b))
*(a::Infinity, b::RealInfinity) = RealInfinity(a)*b
*(a::RealInfinity, b::Infinity) = a*RealInfinity(b)

*(a::Integer, y::Infinity) = a*RealInfinity(y)
*(y::Infinity, a::Integer) = RealInfinity(y)*a

*(a::Real, y::Infinity) = a*RealInfinity(y)
*(y::Infinity, a::Real) = RealInfinity(y)*a

*(y::RealInfinity, a::Real) = a*y
*(y::RealInfinity, a::Integer) = a*y

######
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

isinf(::ComplexInfinity) = true
isfinite(::ComplexInfinity) = false
signbit(y::ComplexInfinity{Bool}) = y.signbit
signbit(y::ComplexInfinity{<:Integer}) = !(mod(y.signbit,2) == 0)

promote_rule(::Type{Infinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
promote_rule(::Type{RealInfinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
convert(::Type{ComplexInfinity{T}}, ::Infinity) where T = ComplexInfinity{T}()
convert(::Type{ComplexInfinity}, ::Infinity) = ComplexInfinity()
convert(::Type{ComplexInfinity{T}}, x::RealInfinity) where T = ComplexInfinity{T}(x)
convert(::Type{ComplexInfinity}, x::RealInfinity) = ComplexInfinity(x)


sign(y::ComplexInfinity{<:Integer}) = mod(y.signbit,2) == 0 ? 1 : -1
angle(x::ComplexInfinity) = π*x.signbit
mod(::ComplexInfinity{<:Integer}, ::Integer) = NotANumber()


show(io::IO, x::ComplexInfinity) = print(io, "exp($(x.signbit)*im*π)∞")

-(y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? ComplexInfinity(one(B)) : ComplexInfinity(zero(B))

function +(x::ComplexInfinity, y::ComplexInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
    promote_type(typeof(x),typeof(y))(x.signbit)
end

+(x::ComplexInfinity, y::Infinity) = x+ComplexInfinity(y)
+(x::Infinity, y::ComplexInfinity) = ComplexInfinity(x)+y
+(x::ComplexInfinity, y::RealInfinity) = x+ComplexInfinity(y)
+(x::RealInfinity, y::ComplexInfinity) = ComplexInfinity(x)+y
+(::Number, y::ComplexInfinity) = y
+(y::ComplexInfinity, ::Number) = y

+(::Complex, ::Infinity) = ComplexInfinity()
+(::Infinity, ::Complex) = ComplexInfinity()
+(::Complex{Bool}, ::Infinity) = ComplexInfinity()
+(::Infinity, ::Complex{Bool}) = ComplexInfinity()

+(::Complex, y::RealInfinity) = ComplexInfinity(y)
+(y::RealInfinity, ::Complex) = ComplexInfinity(y)
+(::Complex{Bool}, y::RealInfinity) = ComplexInfinity(y)
+(y::RealInfinity, ::Complex{Bool}) = ComplexInfinity(y)


# ⊻ is xor
*(a::ComplexInfinity{Bool}, b::ComplexInfinity{Bool}) = ComplexInfinity(a.signbit ⊻ b.signbit)
*(a::ComplexInfinity, b::ComplexInfinity) = ComplexInfinity(a.signbit + b.signbit)
*(a::Infinity, b::ComplexInfinity) = ComplexInfinity(a)*b
*(a::ComplexInfinity, b::Infinity) = a*ComplexInfinity(b)
*(a::RealInfinity, b::ComplexInfinity) = ComplexInfinity(a)*b
*(a::ComplexInfinity, b::RealInfinity) = a*ComplexInfinity(b)

*(a::Real, y::ComplexInfinity) = a > 0 ? y : (-y)
*(y::ComplexInfinity, a::Real) = a*y

*(a::Number, y::ComplexInfinity) = ComplexInfinity(y.signbit+angle(a)/π)
*(y::ComplexInfinity, a::Number) = a*y

for Typ in (Complex, Complex{Bool})
    @eval begin
        *(a::$Typ, y::Infinity) = a*ComplexInfinity(y)
        *(y::Infinity, a::$Typ) = ComplexInfinity(y)*a
        *(a::$Typ,y::RealInfinity) = a*ComplexInfinity(y)
        *(y::RealInfinity, a::$Typ) = ComplexInfinity(y)*a
    end
end

for OP in (:fld,:cld,:div)
  @eval $OP(y::ComplexInfinity, a::Number) = y*(1/sign(a))
end

Base.hash(::Infinity) = 0x020113134b21797f # made up


include("cardinality.jl")
include("compare.jl")
include("algebra.jl")
include("ambiguities.jl")
end # module
