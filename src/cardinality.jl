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

const ℵ₀ = InfiniteCardinal{0}()
const ℵ₁ = InfiniteCardinal{1}()

string(::InfiniteCardinal{0}) = "ℵ₀"
string(::InfiniteCardinal{1}) = "ℵ₁"

show(io::IO, F::InfiniteCardinal{0}) where N = print(io, "ℵ₀")
show(io::IO, F::InfiniteCardinal{1}) where N = print(io, "ℵ₁")


isone(::InfiniteCardinal) = false
iszero(::InfiniteCardinal) = false

sign(y::InfiniteCardinal) = 1
angle(x::InfiniteCardinal) = 0
abs(a::InfiniteCardinal) = a
zero(::InfiniteCardinal) = 0
one(::InfiniteCardinal) = 1

isinf(::InfiniteCardinal) = true
isfinite(::InfiniteCardinal) = false

Integer(::Infinity) = InfiniteCardinal{0}()

==(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = true
==(::InfiniteCardinal, ::InfiniteCardinal)= false
==(::InfiniteCardinal, ::Int) = false
==(::Int, ::InfiniteCardinal) = false
==(x::InfiniteCardinal, ::Infinity) = x == ℵ₀
==(::Infinity, y::InfiniteCardinal) = ℵ₀ == y
==(::InfiniteCardinal{0}, y::RealInfinity) = ∞ == y
==(x::RealInfinity, ::InfiniteCardinal{0}) = x == ∞
==(::InfiniteCardinal, y::Real) = ∞ == y
==(x::Real, ::InfiniteCardinal) = x == ∞

isless(x::InfiniteCardinal, y::InfiniteCardinal) = false
isless(x::Real, y::InfiniteCardinal) = isfinite(x) || sign(y) == -1
isless(x::AbstractFloat, y::InfiniteCardinal) = isless(x, convert(typeof(x), y))
isless(x::InfiniteCardinal, y::AbstractFloat) = false
isless(x::InfiniteCardinal, y::Real) = false

<(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false
≤(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = true
>(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false
≥(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = true

for OP in (:<, :≤)
    @eval begin
        $OP(::Real, ::InfiniteCardinal) = true
        $OP(::InfiniteCardinal, ::Real) = false
        $OP(x::RealInfinity, ::InfiniteCardinal) = $OP(x, ∞)
        $OP(::InfiniteCardinal, y::RealInfinity) = $OP(∞, y)
    end
end

for OP in (:>, :≥)
    @eval begin
        $OP(::Real, ::InfiniteCardinal) = false
        $OP(::InfiniteCardinal, ::Real) = true
        $OP(x::RealInfinity, ::InfiniteCardinal) = $OP(x, ∞)
        $OP(::InfiniteCardinal, y::RealInfinity) = $OP(∞, y)
    end
end

min(::InfiniteCardinal, ::InfiniteCardinal) = ∞
max(::InfiniteCardinal, ::InfiniteCardinal) = ∞
min(x::Real, ::InfiniteCardinal) = x
max(::Real, ::InfiniteCardinal) = ∞
min(::InfiniteCardinal, x::Real) = x
max(::InfiniteCardinal, ::Real) = ∞

min(x::RealInfinity, ::InfiniteCardinal) = min(x, ∞)
max(x::RealInfinity, ::InfiniteCardinal) = max(x, ∞)
min(::InfiniteCardinal, y::RealInfinity) = min(∞, x)
max(::InfiniteCardinal, y::RealInfinity) = max(∞, y)


min(::Infinity, ::InfiniteCardinal) = ∞
min(::InfiniteCardinal, ::Infinity) = ∞
max(::Infinity, ::InfiniteCardinal) = ∞
max(::InfiniteCardinal, ::Infinity) = ∞

*(x::InfiniteCardinal) = x
*(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
*(::InfiniteCardinal{N}, ::Infinity) where N = InfiniteCardinal{N}()
*(::Infinity, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
function *(a::Integer, b::InfiniteCardinal)
    a > 0 || throw(ArgumentError("$a is negative"))
    b
end

*(a::Number, b::InfiniteCardinal) = a * ∞

*(a::InfiniteCardinal, b::Integer) = b*a
*(a::InfiniteCardinal, b::Number) = b*a

+(x::InfiniteCardinal) = x
+(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
+(::Number, y::InfiniteCardinal) = y
+(x::InfiniteCardinal, ::Number) = x
-(x::InfiniteCardinal, ::Number) = x

+(::Integer, y::InfiniteCardinal) = y
+(x::InfiniteCardinal, ::Integer) = x
-(x::InfiniteCardinal, ::Integer) = x

-(::InfiniteCardinal, ::InfiniteCardinal) = NotANumber()

-(::InfiniteCardinal) = -∞
-(x::Number, ::InfiniteCardinal) = x - ∞
-(x::Integer, ::InfiniteCardinal) = x - ∞
-(x::Complex, ::InfiniteCardinal) = x - ∞
-(x::Complex{Bool}, ::InfiniteCardinal) = x - ∞



for OP in (:fld,:cld,:div)
  @eval begin
    $OP(x::InfiniteCardinal, ::Real) = x
    $OP(::InfiniteCardinal, ::InfiniteCardinal) = NotANumber()
  end
end

div(::T, ::InfiniteCardinal) where T<:Real = zero(T)
fld(x::T, ::InfiniteCardinal) where T<:Real = signbit(x) ? -one(T) : zero(T)
cld(x::T, ::InfiniteCardinal) where T<:Real = signbit(x) ? zero(T) : one(T)

mod(::InfiniteCardinal, ::InfiniteCardinal) = NotANumber()
mod(::InfiniteCardinal, ::Real) = NotANumber()
function mod(x::Real, ::InfiniteCardinal) 
    x ≥ 0 || throw(ArgumentError("mod(x,∞) is unbounded for x < 0"))
    x
end


Base.to_index(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(dims::Tuple{Vararg{Union{Infinity, Integer, AbstractUnitRange}}}) = map(Base.to_shape, dims)


##
# Checked
##

Base.Checked.checked_sub(::Integer, ::InfiniteCardinal{0}) = -∞
Base.Checked.checked_sub(::InfiniteCardinal{0}, ::Integer) = ℵ₀
Base.Checked.checked_add(::Integer, ::InfiniteCardinal{0}) = ℵ₀
Base.Checked.checked_add(::InfiniteCardinal{0}, ::Integer) = ℵ₀

Base.Checked.checked_mul(x::Integer, ::InfiniteCardinal{0}) = sign(x)*∞
Base.Checked.checked_mul(::InfiniteCardinal{0}, x::Integer) = sign(x)*∞


##
# hash
##

Base.hash(::InfiniteCardinal{0}) = 0x775431eef01bca90 # made up
Base.hash(::InfiniteCardinal{1}) = 0x55437c69b794f8ce # made up