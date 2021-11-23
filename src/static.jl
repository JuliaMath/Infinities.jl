using Static

"""
StaticNotANumber()

represents something that is undefined, for example, `0 * ∞`.
"""
struct StaticNotANumber <: Number end


"""
   Infinity()

represents the positive real infinite. 
"""
struct StaticInfinity <: Real end

show(io::IO, ::StaticInfinity) = print(io, "static(∞)")
string(::StaticInfinity) = "static(∞)"

convert(::Type{Float64}, ::StaticInfinity) = static(Inf64)
convert(::Type{Float32}, ::StaticInfinity) = static(Inf32)
convert(::Type{Float16}, ::StaticInfinity) = static(Inf16)
Base.Float64(::StaticInfinity) = convert(Float64, static(∞))
Base.Float32(::StaticInfinity) = convert(Float32, static(∞))
Base.Float16(::StaticInfinity) = convert(Float16, static(∞))

# Static.jl doesn't yet support this
# Base.BigFloat(::StaticInfinity) = BigFloat(Inf)
# 
# julia> static(BigFloat(Inf))
# ERROR: There is no static alternative for type BigFloat.
# Stacktrace:
#  [1] error(s::String)
#    @ Base ./error.jl:33
#  [2] _no_static_type(x::Any)
#    @ Static ~/.julia/packages/Static/2w1BY/src/Static.jl:85
#  [3] static(x::BigFloat)
#    @ Static ~/.julia/packages/Static/2w1BY/src/Static.jl:70
#  [4] top-level scope
#    @ REPL[23]:1

convert(::Type{AF}, ::StaticInfinity) where AF<:AbstractFloat = convert(AF, static(Inf))


sign(y::StaticInfinity) = static(1)
angle(x::StaticInfinity) = static(0)

one(::Type{Infinity}) = static(1)
oneunit(::Type{Infinity}) = static(1)
oneunit(::StaticInfinity) = static(1)
zero(::StaticInfinity) = static(0)

isinf(::StaticInfinity) = static(true)
isfinite(::StaticInfinity) = static(false)

==(x::StaticInfinity, y::StaticInfinity) = static(true)
==(x::StaticInfinity, y::Number) = isinf(y) && angle(y) == angle(x)
==(y::Number, x::StaticInfinity) = x == y

isless(x::StaticInfinity, y::StaticInfinity) = static(false)
isless(x::Real, y::StaticInfinity) = isfinite(x) || signbit(x)
isless(x::AbstractFloat, y::StaticInfinity) = isless(x, convert(typeof(x), y))
isless(x::StaticInfinity, y::AbstractFloat) = static(false)
isless(x::StaticInfinity, y::Real) = static(false)

≤(::StaticInfinity, ::StaticInfinity) = static(true)
<(::StaticInfinity, ::StaticInfinity) = static(false)
≥(::StaticInfinity, ::StaticInfinity) = static(true)
>(::StaticInfinity, ::StaticInfinity) = static(false)

<(x::Real, ::StaticInfinity) = isfinite(x) || signbit(x)
≤(::Real, ::StaticInfinity) = static(true)
<(::StaticInfinity, ::Real) = static(false)
≤(::StaticInfinity, y::Real) = isinf(y) && !signbit(y)

>(::Real, ::StaticInfinity) = static(false)
≥(x::Real, ::StaticInfinity) = isinf(x) && !signbit(x)
>(::StaticInfinity, y::Real) = isfinite(y) || signbit(y)
≥(::StaticInfinity, y::Real) = static(true)


min(::StaticInfinity, ::StaticInfinity) = static(∞)
max(::StaticInfinity, ::StaticInfinity) = static(∞)
min(x::Real, ::StaticInfinity) = x
max(::Real, ::StaticInfinity) = static(∞)
min(::StaticInfinity, x::Real) = x
max(::StaticInfinity, ::Real) = static(∞)

+(::StaticInfinity) = static(∞)
+(::StaticInfinity, ::StaticInfinity) = static(∞)
+(::Number, y::StaticInfinity) = static(∞)
+(::StaticInfinity, ::Number) = static(∞)
-(::StaticInfinity, ::Number) = static(∞)

+(::Integer, y::StaticInfinity) = static(∞)
+(::StaticInfinity, ::Integer) = static(∞)
-(::StaticInfinity, ::Integer) = static(∞)

-(::StaticInfinity, ::StaticInfinity) = StaticNotANumber()

# ⊻ is xor
*(::StaticInfinity) = static(∞)
*(::StaticInfinity, ::StaticInfinity) = static(∞)



for OP in (:fld,:cld,:div)
  @eval begin
    $OP(::StaticInfinity, ::Real) = static(∞)
    $OP(::StaticInfinity, ::StaticInfinity) = StaticNotANumber()
  end
end

div(::T, ::StaticInfinity) where T<:Real = static(zero(T))
fld(x::T, ::StaticInfinity) where T<:Real = signbit(x) ? -one(T) : zero(T)
cld(x::T, ::StaticInfinity) where T<:Real = signbit(x) ? zero(T) : one(T)

mod(::StaticInfinity, ::StaticInfinity) = StaticNotANumber()
mod(::StaticInfinity, ::Real) = StaticNotANumber()
function mod(x::Real, ::StaticInfinity) 
    x ≥ 0 || throw(ArgumentError("mod(x,∞) is unbounded for x < 0"))
    x
end



struct StaticRealInfinity{Signbit} <: Real end

StaticRealInfinity() = StaticRealInfinity{False}()
StaticRealInfinity(::StaticInfinity) = StaticRealInfinity()
StaticRealInfinity(x::StaticRealInfinity) = x

-(::StaticInfinity) = StaticRealInfinity{True}()
-(x::Number, ::StaticInfinity) = x + static(-∞)
-(x::Integer, ::StaticInfinity) = x + static(-∞)
-(x::Complex, ::StaticInfinity) = x + static(-∞)
-(x::Complex{Bool}, ::StaticInfinity) = x + static(-∞)


isinf(::StaticRealInfinity) = static(true)
isfinite(::StaticRealInfinity) = static(false)

promote_rule(::Type{Infinity}, ::Type{StaticRealInfinity}) = StaticRealInfinity
convert(::Type{StaticRealInfinity}, ::StaticInfinity) = StaticRealInfinity{False}()

convert(::Type{Float64}, x::StaticRealInfinity) = static(sign(x))*Inf64
convert(::Type{Float32}, x::StaticRealInfinity) = static(sign(x))*Inf32
convert(::Type{Float16}, x::StaticRealInfinity) = static(sign(x))*Inf16
Base.Float64(x::StaticRealInfinity) = convert(Float64, x)
Base.Float32(x::StaticRealInfinity) = convert(Float32, x)
Base.Float16(x::StaticRealInfinity) = convert(Float16, x)
Base.BigFloat(x::StaticRealInfinity) = sign(x)*BigFloat(Inf)
convert(::Type{AF}, x::StaticRealInfinity) where AF<:AbstractFloat = sign(x)*convert(AF, Inf)


# SignBit is a type parameter that's either True or False
# Note that 
#     True() == static(true)
#     False() == static(false)
signbit(y::StaticRealInfinity{Signbit}) where {Signbit} = Signbit()

sign(y::StaticRealInfinity) = static(1) - static(2) * signbit(y)
angle(x::StaticRealInfinity) = π*signbit(x)
mod(::StaticRealInfinity, ::StaticRealInfinity) = StaticNotANumber()
mod(::StaticRealInfinity, ::Real) = StaticNotANumber()
function mod(x::Real, y::StaticRealInfinity) 
    signbit(x) == signbit(y) || throw(ArgumentError("mod($x,$y) is unbounded"))
    x
end

string(y::StaticRealInfinity) = signbit(y) ? "-∞" : "+∞"
show(io::IO, y::StaticRealInfinity) = print(io, string(y))

==(x::StaticRealInfinity, y::StaticInfinity) = !signbit(x)
==(y::StaticInfinity, x::StaticRealInfinity) = !signbit(x)
==(x::StaticRealInfinity, y::StaticRealInfinity) = signbit(x) == signbit(y)

==(x::StaticRealInfinity, y::Number) = isinf(y) && signbit(y) == signbit(x)
==(y::Number, x::StaticRealInfinity) = x == y

isless(x::StaticRealInfinity, y::StaticRealInfinity) = signbit(x) && !signbit(y)
for Typ in (:Number, :Real, :Integer, :AbstractFloat)
    @eval begin
        isless(x::StaticRealInfinity, y::$Typ) = signbit(x) && y ≠ -∞
        isless(x::$Typ, y::StaticRealInfinity) = !signbit(y) && x ≠ ∞
        +(::$Typ, y::StaticRealInfinity) = y
        +(y::StaticRealInfinity, ::$Typ) = y
        -(y::StaticRealInfinity, ::$Typ) = y
        -(::$Typ, y::StaticRealInfinity) = -y
        function *(a::$Typ, y::StaticRealInfinity) 
            iszero(a) && throw(ArgumentError("Cannot multiply $a * $y"))
            a > 0 ? y : (-y)
        end
    end
end

≤(::StaticRealInfinity, ::StaticInfinity) = static(true)
≤(::StaticInfinity, s::StaticRealInfinity) = !signbit(s)
<(s::StaticRealInfinity, ::StaticInfinity) = signbit(s)
<(::StaticInfinity, ::StaticRealInfinity) = static(false)
≥(s::StaticRealInfinity, ::StaticInfinity) = !signbit(s)
≥(::StaticInfinity, ::StaticRealInfinity) = static(true)
>(::StaticRealInfinity, ::StaticInfinity) = static(false)
>(::StaticInfinity, s::StaticRealInfinity) = signbit(s)




function -(::StaticInfinity, y::StaticRealInfinity) 
    signbit(y) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    ∞
end

function -(x::StaticRealInfinity, ::StaticInfinity) 
    signbit(x) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    x
end

function -(x::StaticRealInfinity, y::StaticRealInfinity) 
    signbit(x) == !signbit(y) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    x
end

-(y::StaticRealInfinity) = StaticRealInfinity(!signbit(y))

function +(x::StaticRealInfinity, y::StaticRealInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
    x
end

+(x::StaticRealInfinity, y::StaticInfinity) = x+StaticRealInfinity(y)
+(x::StaticInfinity, y::StaticRealInfinity) = StaticRealInfinity(x)+y

# ⊻ is xor
*(a::StaticRealInfinity, b::StaticRealInfinity) = StaticRealInfinity(signbit(a) ⊻ signbit(b))
*(a::StaticInfinity, b::StaticRealInfinity) = StaticRealInfinity(a)*b
*(a::StaticRealInfinity, b::StaticInfinity) = a*StaticRealInfinity(b)

*(a::Integer, y::StaticInfinity) = a*StaticRealInfinity(y)
*(y::StaticInfinity, a::Integer) = StaticRealInfinity(y)*a

*(a::Real, y::StaticInfinity) = a*StaticRealInfinity(y)
*(y::StaticInfinity, a::Real) = StaticRealInfinity(y)*a

*(y::StaticRealInfinity, a::Real) = a*y
*(y::StaticRealInfinity, a::Integer) = a*y

<(x::StaticRealInfinity, y::StaticRealInfinity) = signbit(x) & !signbit(y)
≤(x::StaticRealInfinity, y::StaticRealInfinity) = signbit(x) | !signbit(y)

for OP in (:<,:≤)
    @eval begin
        $OP(x::Real, y::StaticRealInfinity) = !signbit(y)
        $OP(y::StaticRealInfinity, x::Real) = signbit(y)
    end
end


min(x::StaticRealInfinity, y::StaticRealInfinity) = StaticRealInfinity(signbit(x) | signbit(y))
max(x::StaticRealInfinity, y::StaticRealInfinity) = StaticRealInfinity(signbit(x) & signbit(y))
min(x::Real, y::StaticRealInfinity) = signbit(y) ? y : x
max(x::Real, y::StaticRealInfinity) = signbit(y) ? x : y
min(x::StaticRealInfinity, y::Real) = signbit(x) ? x : y
max(x::StaticRealInfinity, y::Real) = signbit(x) ? y : x
min(x::StaticRealInfinity, ::StaticInfinity) = x
max(::StaticRealInfinity, ::StaticInfinity) = static(∞)
min(::StaticInfinity, x::StaticRealInfinity) = x
max(::StaticInfinity, x::StaticRealInfinity) = static(∞)



######
# StaticComplexInfinity
#######

# angle is π*a where a is (false==0) and (true==1)

"""
StaticComplexInfinity(signbit)

represents a statically-known infinity in the complex plane with the angle 
specified by `π * signbit`. The use of the name `signbit` is for consistency
with `StaticRealInfinity`. 
"""
struct StaticComplexInfinity{T<:Real} <: Number
    signbit::T
end

StaticComplexInfinity{T}() where T = StaticComplexInfinity(zero(T))
StaticComplexInfinity() = StaticComplexInfinity{Bool}()
StaticComplexInfinity{T}(::StaticInfinity) where T<:Real = StaticComplexInfinity{T}()
StaticComplexInfinity(::StaticInfinity) = StaticComplexInfinity()
StaticComplexInfinity{T}(x::StaticRealInfinity) where T<:Real = StaticComplexInfinity{T}(signbit(x))
StaticComplexInfinity(x::StaticRealInfinity) = StaticComplexInfinity(signbit(x))



isinf(::StaticComplexInfinity) = static(true)
isfinite(::StaticComplexInfinity) = static(false)


promote_rule(::Type{Infinity}, ::Type{StaticComplexInfinity{T}}) where T = StaticComplexInfinity{T}
promote_rule(::Type{StaticRealInfinity}, ::Type{StaticComplexInfinity{T}}) where T = StaticComplexInfinity{T}
convert(::Type{StaticComplexInfinity{T}}, ::StaticInfinity) where T = StaticComplexInfinity{T}()
convert(::Type{StaticComplexInfinity}, ::StaticInfinity) = StaticComplexInfinity()
convert(::Type{StaticComplexInfinity{T}}, x::StaticRealInfinity) where T = StaticComplexInfinity{T}(x)
convert(::Type{StaticComplexInfinity}, x::StaticRealInfinity) = StaticComplexInfinity(x)


sign(y::StaticComplexInfinity{<:Integer}) = mod(signbit(y),2) == 0 ? 1 : -1
angle(x::StaticComplexInfinity) = π*signbit(x)
mod(::StaticComplexInfinity{<:Integer}, ::Integer) = StaticNotANumber()


show(io::IO, x::StaticComplexInfinity) = print(io, "exp($(signbit(x))*im*π)∞")

==(x::StaticComplexInfinity, y::StaticInfinity) = signbit(x) == 0
==(y::StaticInfinity, x::StaticComplexInfinity) = signbit(x) == 0
==(x::StaticComplexInfinity, y::StaticRealInfinity) = signbit(x) == signbit(y)
==(y::StaticRealInfinity, x::StaticComplexInfinity) = signbit(x) == signbit(y)
==(x::StaticComplexInfinity, y::StaticComplexInfinity) = signbit(x) == signbit(y)

==(x::StaticComplexInfinity, y::Number) = isinf(y) && angle(y) == angle(x)
==(y::Number, x::StaticComplexInfinity) = x == y

isless(x::StaticComplexInfinity{Bool}, y::StaticComplexInfinity{Bool}) = signbit(x) && !signbit(y)
isless(x::Number, y::StaticComplexInfinity{Bool}) = !signbit(y) && x ≠ ∞
isless(x::StaticComplexInfinity{Bool}, y::Number) = signbit(x) && y ≠ -∞

-(y::StaticComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? StaticComplexInfinity(one(B)) : StaticComplexInfinity(zero(B))

function +(x::StaticComplexInfinity, y::StaticComplexInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
    promote_type(typeof(x),typeof(y))(signbit(x))
end

+(x::StaticComplexInfinity, y::StaticInfinity) = x+StaticComplexInfinity(y)
+(x::StaticInfinity, y::StaticComplexInfinity) = StaticComplexInfinity(x)+y
+(x::StaticComplexInfinity, y::StaticRealInfinity) = x+StaticComplexInfinity(y)
+(x::StaticRealInfinity, y::StaticComplexInfinity) = StaticComplexInfinity(x)+y
+(::Number, y::StaticComplexInfinity) = y
+(y::StaticComplexInfinity, ::Number) = y
-(y::StaticComplexInfinity, ::Number) = y
-(::Number, y::StaticComplexInfinity) = -y

+(::Complex, ::StaticInfinity) = StaticComplexInfinity()
+(::StaticInfinity, ::Complex) = StaticComplexInfinity()
-(::StaticInfinity, ::Complex) = StaticComplexInfinity()
+(::Complex{Bool}, ::StaticInfinity) = StaticComplexInfinity()
+(::StaticInfinity, ::Complex{Bool}) = StaticComplexInfinity()
-(::StaticInfinity, ::Complex{Bool}) = StaticComplexInfinity()

+(::Complex, y::StaticRealInfinity) = StaticComplexInfinity(y)
+(y::StaticRealInfinity, ::Complex) = StaticComplexInfinity(y)
-(y::StaticRealInfinity, ::Complex) = StaticComplexInfinity(y)
+(::Complex{Bool}, y::StaticRealInfinity) = StaticComplexInfinity(y)
+(y::StaticRealInfinity, ::Complex{Bool}) = StaticComplexInfinity(y)
-(y::StaticRealInfinity, ::Complex{Bool}) = StaticComplexInfinity(y)


# ⊻ is xor
*(a::StaticComplexInfinity{Bool}, b::StaticComplexInfinity{Bool}) = StaticComplexInfinity(a.signbit ⊻ b.signbit)
*(a::StaticComplexInfinity, b::StaticComplexInfinity) = StaticComplexInfinity(a.signbit + b.signbit)
*(a::StaticInfinity, b::StaticComplexInfinity) = StaticComplexInfinity(a)*b
*(a::StaticComplexInfinity, b::StaticInfinity) = a*StaticComplexInfinity(b)
*(a::StaticRealInfinity, b::StaticComplexInfinity) = StaticComplexInfinity(a)*b
*(a::StaticComplexInfinity, b::StaticRealInfinity) = a*StaticComplexInfinity(b)

*(a::Real, y::StaticComplexInfinity) = a > static(0) ? y : (-y)
*(y::StaticComplexInfinity, a::Real) = a*y

*(a::Number, y::StaticComplexInfinity) = StaticComplexInfinity(signbit(y)+angle(a)/π)
*(y::StaticComplexInfinity, a::Number) = a*y

*(a::Complex, y::StaticInfinity) = a*StaticComplexInfinity(y)
*(y::StaticInfinity, a::Complex) = StaticComplexInfinity(y)*a

*(a::Complex,y::StaticRealInfinity) = a*StaticComplexInfinity(y)
*(y::StaticRealInfinity, a::Complex) = StaticComplexInfinity(y)*a

for OP in (:fld,:cld,:div)
  @eval $OP(y::StaticComplexInfinity, a::Number) = y*(1/sign(a))
end

min(x::StaticComplexInfinity{B}, y::StaticComplexInfinity{B}) where B<:Integer = sign(x) == -1 ? x : y
max(x::StaticComplexInfinity{B}, ::StaticComplexInfinity{B}) where B<:Integer = sign(x) == 1 ? x : y
min(x::Real, y::StaticComplexInfinity{B}) where B<:Integer = sign(y) == static(1) ? x : y
min(x::StaticComplexInfinity{B}, y::Real) where B<:Integer = min(y,x)
max(x::Real, y::StaticComplexInfinity{B}) where B<:Integer = sign(y) == static(1) ? y : x
max(x::StaticComplexInfinity{B}, y::Real) where B<:Integer = max(y,x)

for OP in (:<,:≤)
    @eval begin
        $OP(x::Real, y::StaticComplexInfinity{B}) where B<:Integer = sign(y) == static( 1)
        $OP(y::StaticComplexInfinity{B}, x::Real) where B<:Integer = sign(y) == static(-1)
    end
end

for OP in (:>, :≥)
    @eval begin
        $OP(x::Real, y::StaticComplexInfinity{B}) where B<:Integer = sign(y) == static(-1)
        $OP(y::StaticComplexInfinity{B}, x::Real) where B<:Integer = sign(y) == static(1)
    end
end

Base.hash(::StaticInfinity) = 0x107ff1530f9b7a1e # made up


