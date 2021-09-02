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

signbit(::InfiniteCardinal) = false
sign(::InfiniteCardinal) = 1
angle(::InfiniteCardinal) = 0
abs(a::InfiniteCardinal) = a
zero(::InfiniteCardinal) = 0
one(::Type{<:InfiniteCardinal}) = 1
oneunit(::Type{<:InfiniteCardinal}) = 1

isinf(::InfiniteCardinal) = true
isfinite(::InfiniteCardinal) = false

Integer(::Infinity) = InfiniteCardinal{0}()
function Integer(x::RealInfinity)
    signbit(x) && throw(InexactError(:Integer, Integer, x))
    ℵ₀
end
function Integer(x::ComplexInfinity)
    iszero(angle(x)) || throw(InexactError(:Integer, Integer, x))
    ℵ₀
end

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

@generated isless(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(isless(N, M)))
isless(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = false
isless(x::Real, ::InfiniteCardinal{0}) = isfinite(x)
isless(x::Real, ::InfiniteCardinal) = true
isless(x::AbstractFloat, ::InfiniteCardinal{0}) = isfinite(x)
isless(x::AbstractFloat, ::InfiniteCardinal) = true
isless(::InfiniteCardinal, y::Real) = false
isless(x::InfiniteCardinal, y::AbstractFloat) = false

@generated <(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(N < M))
@generated ≤(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(N ≤ M))
@generated >(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(N > M))
@generated ≥(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :($(N ≥ M))

≤(::InfiniteCardinal{0}, ::InfiniteCardinal) = true
>(::InfiniteCardinal{0}, ::InfiniteCardinal) = false
≥(::InfiniteCardinal, ::InfiniteCardinal{0}) = true
<(::InfiniteCardinal, ::InfiniteCardinal{0}) = false


<(x::Real, ::InfiniteCardinal{0}) = x < ∞
<(x::Real, ::InfiniteCardinal) = true
≤(x::Real, ::InfiniteCardinal) = true
<(::InfiniteCardinal, x::Real) = false
≤(::InfiniteCardinal{0}, x::Real) = ∞ ≤ x
≤(::InfiniteCardinal, x::Real) = false
>(::InfiniteCardinal{0}, y::Real) = ∞ > y
>(::InfiniteCardinal, ::Real) = true
≥(::InfiniteCardinal, ::Real) = true
>(::Real, ::InfiniteCardinal) = false
≥(x::Real, ::InfiniteCardinal{0}) = x ≥ ∞
≥(x::Real, ::InfiniteCardinal) = false


<(::Infinity, ::InfiniteCardinal{0}) = false
<(::Infinity, ::InfiniteCardinal) = true
≤(::Infinity, ::InfiniteCardinal) = true
<(::InfiniteCardinal, ::Infinity) = false
≤(::InfiniteCardinal{0}, ::Infinity) = true
≤(::InfiniteCardinal, ::Infinity) = false
>(::InfiniteCardinal{0}, ::Infinity) = false
>(::InfiniteCardinal, ::Infinity) = true
≥(::InfiniteCardinal, ::Infinity) = true
>(::Infinity, ::InfiniteCardinal) = false
≥(::Infinity, ::InfiniteCardinal{0}) = true
≥(::Infinity, ::InfiniteCardinal) = false


<(x::RealInfinity, ::InfiniteCardinal{0}) = x < ∞
<(x::RealInfinity, ::InfiniteCardinal) = true
≤(x::RealInfinity, ::InfiniteCardinal) = true
>(::InfiniteCardinal{0}, y::RealInfinity) = ∞ > y
>(::InfiniteCardinal, ::RealInfinity) = true
≥(::InfiniteCardinal, ::RealInfinity) = true


@generated min(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :(InfiniteCardinal{$(min(N,M))}())
@generated max(::InfiniteCardinal{N}, ::InfiniteCardinal{M}) where {N,M} = :(InfiniteCardinal{$(max(N,M))}())
min(x::Real, ::InfiniteCardinal) = x
max(::Real, ℵ::InfiniteCardinal) = ℵ
min(::InfiniteCardinal, x::Real) = x
max(ℵ::InfiniteCardinal, ::Real) = ℵ

min(x::RealInfinity, ::InfiniteCardinal) = min(x, ∞)
max(::RealInfinity, ℵ::InfiniteCardinal) = ℵ
min(::InfiniteCardinal, y::RealInfinity) = min(∞, y)
max(ℵ::InfiniteCardinal, ::RealInfinity) = ℵ

min(::Infinity, ::InfiniteCardinal) = ∞
min(::InfiniteCardinal, ::Infinity) = ∞
max(::Infinity, ℵ::InfiniteCardinal) = ℵ
max(ℵ::InfiniteCardinal, ::Infinity) = ℵ

*(x::InfiniteCardinal) = x
*(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
*(::InfiniteCardinal{N}, ::Infinity) where N = InfiniteCardinal{N}()
*(::Infinity, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
function *(a::Integer, b::InfiniteCardinal)
    a > 0 || throw(ArgumentError("$a is non-positive"))
    b
end

*(a::Number, b::InfiniteCardinal) = a * ∞

*(a::InfiniteCardinal, b::Integer) = b*a
*(a::InfiniteCardinal, b::Number) = b*a

+(x::InfiniteCardinal) = x
+(x::InfiniteCardinal, y::InfiniteCardinal) = max(x,y)

+(::Integer, y::InfiniteCardinal) = y
+(x::InfiniteCardinal, ::Integer) = x
-(x::InfiniteCardinal, ::Integer) = x

-(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = NotANumber()

-(::InfiniteCardinal) = -∞
-(x::Integer, ::InfiniteCardinal) = x - ∞

for op in (:+, :-)
    @eval begin
        $op(x::Number, ::InfiniteCardinal) = $op(x, ∞)
        $op(::InfiniteCardinal, x::Number) = $op(∞, x)
    end
end

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


# avoid stack overflow
getindex(A::Array, i1::InfiniteCardinal{0}, I::Integer...) = throw(BoundsError(A, i1, I...))

Base._unsafe_getindex(::IndexStyle, A::AbstractArray, I::InfiniteCardinal{0}) = error("Overload getindex(::$(typeof(A)), ::InfiniteCardinal{0})")

# Avoid too-strict restrictions in SubArray
function getindex(V::SubArray{T,N}, i1::InfiniteCardinal{0}, I::Integer...) where {T,N}
  @boundscheck checkbounds(V, i1, I...)
  @inbounds r = V.parent[Base.reindex(V.indices, tuple(i1, I...))...]
  r
end
