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

signbit(::InfiniteCardinal) = false
sign(::InfiniteCardinal) = 1
angle(::InfiniteCardinal) = 0
abs(a::InfiniteCardinal) = a
zero(::InfiniteCardinal) = 0
zero(::Type{<:InfiniteCardinal}) = 0
one(::Type{<:InfiniteCardinal}) = 1
oneunit(::Type{<:InfiniteCardinal}) = 1
oneunit(::InfiniteCardinal) = 1

Integer(::Infinity) = InfiniteCardinal{0}()
function Integer(x::RealInfinity)
    signbit(x) && throw(InexactError(:Integer, Integer, x))
    ℵ₀
end
function Integer(x::ComplexInfinity)
    iszero(angle(x)) || throw(InexactError(:Integer, Integer, x))
    ℵ₀
end

Base.to_index(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(::Union{Infinity,InfiniteCardinal{0}}) = ℵ₀
Base.to_shape(dims::Tuple{Vararg{Union{Infinity, Integer, AbstractUnitRange}}}) = map(Base.to_shape, dims)


##
# Checked
##

Base.Checked.checked_sub(::Integer, ::InfiniteCardinal{0}) = -∞
Base.Checked.checked_sub(::InfiniteCardinal{0}, ::Integer) = ℵ₀
Base.Checked.checked_sub(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = NotANumber() # ambiguity fix
Base.Checked.checked_add(::Integer, ::InfiniteCardinal{0}) = ℵ₀
Base.Checked.checked_add(::InfiniteCardinal{0}, ::Integer) = ℵ₀
Base.Checked.checked_add(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = ℵ₀ # ambiguity fix

Base.Checked.checked_mul(x::Integer, ::InfiniteCardinal{0}) = sign(x)*∞
Base.Checked.checked_mul(::InfiniteCardinal{0}, x::Integer) = sign(x)*∞
Base.Checked.checked_mul(::InfiniteCardinal{0}, ::InfiniteCardinal{0}) = ℵ₀ # ambiguity fix


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
