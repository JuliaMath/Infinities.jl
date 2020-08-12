module Infinities

import Base: angle, isone, iszero, isinf, isfinite, abs, one, zero, isless, 
                +, -, *, ==, <, ≤, >, ≥, fld, cld, div, mod, min, max, sign, signbit, 
                string, show, promote_rule, convert

export ∞, Infinity, RealInfinity, ComplexInfinity, NotANumber

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

convert(::Type{Float64}, ::Infinity) = Inf64
convert(::Type{Float32}, ::Infinity) = Inf32
convert(::Type{Float16}, ::Infinity) = Inf16
convert(::Type{AF}, ::Infinity) where AF<:AbstractFloat = convert(AF, Inf)


sign(y::Infinity) = 1
angle(x::Infinity) = 0

==(x::Infinity, y::Infinity) = true

one(::Type{Infinity}) = 1
zero(::Infinity) = 0

isinf(::Infinity) = true
isfinite(::Infinity) = false

==(x::Infinity, y::Number) = isinf(y) && angle(y) == angle(x)
==(y::Number, x::Infinity) = x == y



isless(x::Infinity, y::Infinity) = false
isless(x::Real, y::Infinity) = isfinite(x) || sign(y) == -1
isless(x::AbstractFloat, y::Infinity) = isless(x, convert(typeof(x), y))
isless(x::Infinity, y::AbstractFloat) = false
isless(x::Infinity, y::Real) = false

+(::Infinity, ::Infinity) = ∞
+(::Number, y::Infinity) = ∞
+(::Infinity, ::Number) = ∞
-(::Infinity, ::Number) = ∞
-(x::Number, ::Infinity) = x + (-∞)

+(::Integer, y::Infinity) = ∞
+(::Infinity, ::Integer) = ∞
-(::Infinity, ::Integer) = ∞
-(x::Integer, ::Infinity) = x + (-∞)
+(::Complex, y::Infinity) = ∞
+(::Infinity, ::Complex) = ∞
-(::Infinity, ::Complex) = ∞
-(x::Complex, ::Infinity) = x + (-∞)

-(::Infinity, ::Infinity) = NotANumber()

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

mod(::Infinity, ::Infinity) = NotANumber()
mod(::Infinity, ::Real) = NotANumber()
function mod(x::Real, ::Infinity) 
    x ≥ 0 || throw(ArgumentError("mod(x,∞) is unbounded for x < 0"))
    x
end

min(::Infinity, ::Infinity) = ∞
max(::Infinity, ::Infinity) = ∞
min(x::Real, ::Infinity) = x
max(::Real, ::Infinity) = ∞
min(::Infinity, x::Real) = x
max(::Infinity, ::Real) = ∞

≤(::Infinity, ::Infinity) = true
<(::Infinity, ::Infinity) = false
≥(::Infinity, ::Infinity) = true
>(::Infinity, ::Infinity) = false

for OP in (:<, :≤)
    @eval begin
        $OP(::Real, ::Infinity) = true
        $OP(::Infinity, ::Real) = false
    end
end

for OP in (:>, :≥)
    @eval begin
        $OP(::Real, ::Infinity) = false
        $OP(::Infinity, ::Real) = true
    end
end


struct RealInfinity <: Real
    signbit::Bool
end

RealInfinity() = RealInfinity(false)
RealInfinity(::Infinity) = RealInfinity()
RealInfinity(x::RealInfinity) = x

-(::Infinity) = RealInfinity(true)
+(::Infinity) = ∞

isinf(::RealInfinity) = true
isfinite(::RealInfinity) = false

promote_rule(::Type{Infinity}, ::Type{RealInfinity}) = RealInfinity
convert(::Type{RealInfinity}, ::Infinity) = RealInfinity(false)

signbit(y::RealInfinity) = y.signbit
sign(y::RealInfinity) = 1-2signbit(y)
angle(x::RealInfinity) = π*signbit(x)
mod(::RealInfinity, ::RealInfinity) = NotANumber()
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

==(x::RealInfinity, y::Number) = isinf(y) && signbit(y) == signbit(x)
==(y::Number, x::RealInfinity) = x == y

isless(x::RealInfinity, y::RealInfinity) = signbit(x) && !signbit(y)
for Typ in (:Number, :Real, :Integer, :AbstractFloat)
    @eval begin
        isless(x::RealInfinity, y::$Typ) = signbit(x) && y ≠ -∞
        isless(x::$Typ, y::RealInfinity) = !signbit(y) && x ≠ ∞
        +(::$Typ, y::RealInfinity) = y
        +(y::RealInfinity, ::$Typ) = y
        -(y::RealInfinity, ::$Typ) = y
        -(::$Typ, y::RealInfinity) = -y
        function *(a::$Typ, y::RealInfinity) 
            iszero(a) && throw(ArgumentError("Cannot multiply $a * $y"))
            a > 0 ? y : (-y)
        end
    end
end

≤(::RealInfinity, ::Infinity) = true
≤(::Infinity, s::RealInfinity) = !signbit(s)
<(s::RealInfinity, ::Infinity) = signbit(s)
<(::Infinity, ::RealInfinity) = false
≥(s::RealInfinity, ::Infinity) = !signbit(s)
≥(::Infinity, ::RealInfinity) = true
>(::RealInfinity, ::Infinity) = false
>(::Infinity, s::RealInfinity) = signbit(s)




function -(::Infinity, y::RealInfinity) 
    signbit(y) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    ∞
end

function -(x::RealInfinity, ::Infinity) 
    signbit(x) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    x
end

function -(x::RealInfinity, y::RealInfinity) 
    signbit(x) == !signbit(y) || throw(ArgumentError("Cannot subtract ∞ from ∞"))
    x
end

-(y::RealInfinity) = RealInfinity(!y.signbit)

function +(x::RealInfinity, y::RealInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
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

<(x::RealInfinity, y::RealInfinity) = signbit(x) & !signbit(y)
≤(x::RealInfinity, y::RealInfinity) = signbit(x) | !signbit(y)

for OP in (:<,:≤)
    @eval begin
        $OP(x::Real, y::RealInfinity) = !signbit(y)
        $OP(y::RealInfinity, x::Real) = signbit(y)
    end
end


min(x::RealInfinity, y::RealInfinity) = RealInfinity(x.signbit | y.signbit)
max(x::RealInfinity, y::RealInfinity) = RealInfinity(x.signbit & y.signbit)
min(x::Real, y::RealInfinity) = y.signbit ? x : y
max(x::Real, y::RealInfinity) = y.signbit ? y : x
min(x::RealInfinity, y::Real) = x.signbit ? x : y
max(x::RealInfinity, y::Real) = x.signbit ? y : x
min(x::RealInfinity, ::Infinity) = x
max(::RealInfinity, ::Infinity) = ∞
min(::Infinity, x::RealInfinity) = x
max(::Infinity, x::RealInfinity) = ∞



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


promote_rule(::Type{Infinity}, ::Type{ComplexInfinity{T}}) where T = ComplexInfinity{T}
convert(::Type{ComplexInfinity{T}}, ::Infinity) where T = ComplexInfinity{T}()
convert(::Type{ComplexInfinity}, ::Infinity) = ComplexInfinity()
convert(::Type{ComplexInfinity{T}}, x::RealInfinity) where T = ComplexInfinity{T}(x)
convert(::Type{ComplexInfinity}, x::RealInfinity) = ComplexInfinity(x)


sign(y::ComplexInfinity{<:Integer}) = mod(y.signbit,2) == 0 ? 1 : -1
angle(x::ComplexInfinity) = π*x.signbit
mod(::ComplexInfinity{<:Integer}, ::Integer) = NotANumber()


show(io::IO, x::ComplexInfinity) = print(io, "$(exp(im*π*x.signbit))∞")

==(x::ComplexInfinity, y::Infinity) = x.signbit == 0
==(y::Infinity, x::ComplexInfinity) = x.signbit == 0
==(x::ComplexInfinity, y::RealInfinity) = x.signbit == signbit(y)
==(y::RealInfinity, x::ComplexInfinity) = x.signbit == signbit(y)
==(x::ComplexInfinity, y::ComplexInfinity) = x.signbit == y.signbit

==(x::ComplexInfinity, y::Number) = isinf(y) && angle(y) == angle(x)
==(y::Number, x::ComplexInfinity) = x == y

isless(x::ComplexInfinity{Bool}, y::ComplexInfinity{Bool}) = x.signbit && !y.signbit
isless(x::Number, y::ComplexInfinity{Bool}) = !y.signbit && x ≠ ∞
isless(x::ComplexInfinity{Bool}, y::Number) = x.signbit && y ≠ -∞

-(y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? ComplexInfinity(one(B)) : ComplexInfinity(zero(B))

function +(x::ComplexInfinity, y::ComplexInfinity)
    x == y || throw(ArgumentError("Angles must be the same to add ∞"))
    promote_type(typeof(x),typeof(y))(x.signbit)
end

+(x::ComplexInfinity, y::Infinity) = x+ComplexInfinity(y)
+(x::Infinity, y::ComplexInfinity) = ComplexInfinity(x)+y
+(x::ComplexInfinity, y::RealInfinity) = x+ComplexInfinity(y)
+(x::RealInfinity, y::ComplexInfinity) = ComplexInfinity(x)+y
+(::Number, y::ComplexInfinity) = y
+(y::ComplexInfinity, ::Number) = y
-(y::ComplexInfinity, ::Number) = y
-(::Number, y::ComplexInfinity) = -y


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

*(a::Number, y::Infinity) = a*ComplexInfinity(y)
*(y::Infinity, a::Number) = ComplexInfinity(y)*a
*(y::RealInfinity, a::Number) = ComplexInfinity(y)*a

*(a::Complex, y::Infinity) = a*ComplexInfinity(y)
*(y::Infinity, a::Complex) = ComplexInfinity(y)*a

*(a::Complex,y::RealInfinity) = a*ComplexInfinity(y)
*(y::RealInfinity, a::Complex) = ComplexInfinity(y)*a

for OP in (:fld,:cld,:div)
  @eval $OP(y::ComplexInfinity, a::Number) = y*(1/sign(a))
end

min(x::ComplexInfinity{B}, y::ComplexInfinity{B}) where B<:Integer = sign(x) == -1 ? x : y
max(x::ComplexInfinity{B}, ::ComplexInfinity{B}) where B<:Integer = sign(x) == 1 ? x : y
min(x::Real, y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? x : y
min(x::ComplexInfinity{B}, y::Real) where B<:Integer = min(y,x)
max(x::Real, y::ComplexInfinity{B}) where B<:Integer = sign(y) == 1 ? y : x
max(x::ComplexInfinity{B}, y::Real) where B<:Integer = max(y,x)

for OP in (:<,:≤)
    @eval begin
        $OP(x::Real, y::ComplexInfinity{B}) where B<:Integer = sign(y) ==  1
        $OP(y::ComplexInfinity{B}, x::Real) where B<:Integer = sign(y) == -1
    end
end

for OP in (:>, :≥)
    @eval begin
        $OP(x::Real, y::ComplexInfinity{B}) where B<:Integer = sign(y) == -1
        $OP(y::ComplexInfinity{B}, x::Real) where B<:Integer = sign(y) == 1
    end
end

##
# Checked
##

Base.Checked.checked_sub(::Integer, ::Infinity) = -∞
Base.Checked.checked_sub(::Infinity, ::Integer) = ∞
Base.Checked.checked_add(::Integer, ::Infinity) = ∞
Base.Checked.checked_add(::Infinity, ::Integer) = ∞

Base.Checked.checked_sub(::Integer, x::RealInfinity) = -x
Base.Checked.checked_sub(x::RealInfinity, ::Integer) = x
Base.Checked.checked_add(::Integer, x::RealInfinity) = x
Base.Checked.checked_add(x::RealInfinity, ::Integer) = x


Base.to_index(::Infinity) = ∞

###
# Cardinalities
###
"""
InfiniteCardinal{K}()

represents the k-th cardinal number. 
Note that `InfiniteCardinal <: Integer` to support
being treated as a length in array machinier.
"""
struct InfiniteCardinal{N} <: Integer end

isone(::InfiniteCardinal) = false
iszero(::InfiniteCardinal) = false

==(::InfiniteCardinal, ::Int) = false
==(::Int, ::InfiniteCardinal) = false

*(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
*(::InfiniteCardinal{N}, ::Infinity) where N = InfiniteCardinal{N}()
*(::Infinity, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
function *(a::Integer, b::InfiniteCardinal)
    a > 0 || throw(ArgumentError("$a is negative"))
    b
end

*(a::InfiniteCardinal, b::Integer) = b*a


abs(a::InfiniteCardinal) = a
zero(::InfiniteCardinal) = 0

for OP in (:<, :≤)
    @eval begin
        $OP(::Real, ::InfiniteCardinal) = true
        $OP(::InfiniteCardinal, ::Real) = false
    end
end

for OP in (:>, :≥)
    @eval begin
        $OP(::Real, ::InfiniteCardinal) = false
        $OP(::InfiniteCardinal, ::Real) = true
    end
end

const ℵ₀ = InfiniteCardinal{0}()
const ℵ₁ = InfiniteCardinal{1}()

string(::InfiniteCardinal{0}) = "ℵ₀"
string(::InfiniteCardinal{1}) = "ℵ₁"

show(io::IO, F::InfiniteCardinal{0}) where N =
    print(io, "ℵ₀")
show(io::IO, F::InfiniteCardinal{1}) where N =
    print(io, "ℵ₁")

end # module
