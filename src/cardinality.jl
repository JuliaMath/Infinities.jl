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

show(io::IO, F::InfiniteCardinal{0}) = print(io, "ℵ₀")
show(io::IO, F::InfiniteCardinal{1}) = print(io, "ℵ₁")


isone(::InfiniteCardinal) = false
iszero(::InfiniteCardinal) = false

signbit(::InfiniteCardinal) = false
sign(::InfiniteCardinal) = 1
angle(::InfiniteCardinal) = 0
abs(a::InfiniteCardinal) = a
zero(::InfiniteCardinal) = 0
one(::Type{<:InfiniteCardinal}) = 1
oneunit(::Type{<:InfiniteCardinal}) = 1
oneunit(::InfiniteCardinal) = 1

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

*(x::InfiniteCardinal) = x
*(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
*(x::InfiniteCardinal, y::InfiniteCardinal) = max(x,y)
*(::InfiniteCardinal{N}, ::Infinity) where N = InfiniteCardinal{N}()
*(::Infinity, ::InfiniteCardinal{N}) where N = InfiniteCardinal{N}()
function *(a::Integer, b::InfiniteCardinal)
    a > 0 || throw(ArgumentError("$a is non-positive"))
    b
end

for Typ in (Number, Complex, ComplexInfinity, Complex{Bool}, RealInfinity, Rational)
    @eval begin
        *(a::$Typ, b::InfiniteCardinal) = a * ∞
        *(a::InfiniteCardinal, b::$Typ) = b*a
    end
end

*(a::InfiniteCardinal, b::Integer) = b*a

+(x::InfiniteCardinal) = x

-(x::InfiniteCardinal, ::Integer) = x

-(::InfiniteCardinal{N}, ::InfiniteCardinal{N}) where N = NotANumber()

-(::InfiniteCardinal) = -∞
-(x::Integer, ::InfiniteCardinal) = x - ∞

for OP in (:fld,:cld,:div)
    for Typ in (Real, Rational)
        @eval begin
            $OP(x::InfiniteCardinal, ::$Typ) = x
        end
    end
    @eval begin
        $OP(::InfiniteCardinal, ::InfiniteCardinal) = NotANumber()
        $OP(::Infinity, ::InfiniteCardinal) = NotANumber()
        $OP(::InfiniteCardinal, ::Infinity) = NotANumber()
    end
end
for Typ in (Real, Rational)
    @eval begin
        div(::T, ::InfiniteCardinal) where T <: $Typ = zero(T)
        fld(x::T, ::InfiniteCardinal) where T <: $Typ = signbit(x) ? -one(T) : zero(T)
        cld(x::T, ::InfiniteCardinal) where T <: $Typ = signbit(x) ? zero(T) : one(T)
    end
end

Base.to_index(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(dims::Tuple{Vararg{Union{Infinity, Integer, AbstractUnitRange}}}) = map(Base.to_shape, dims)


##
# Checked
##

Base.Checked.checked_sub(::Integer, ::InfiniteCardinal{0}) = -∞
Base.Checked.checked_sub(::InfiniteCardinal{0}, ::Integer) = ℵ₀
Base.Checked.checked_sub(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = NotANumber()
Base.Checked.checked_add(::Integer, ::InfiniteCardinal{0}) = ℵ₀
Base.Checked.checked_add(::InfiniteCardinal{0}, ::Integer) = ℵ₀
Base.Checked.checked_add(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = ℵ₀

Base.Checked.checked_mul(x::Integer, ::InfiniteCardinal{0}) = sign(x)*∞
Base.Checked.checked_mul(::InfiniteCardinal{0}, x::Integer) = sign(x)*∞
Base.Checked.checked_mul(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = ℵ₀


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
