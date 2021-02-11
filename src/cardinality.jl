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
one(::InfiniteCardinal) = 1

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

show(io::IO, F::InfiniteCardinal{0}) where N = print(io, "ℵ₀")
show(io::IO, F::InfiniteCardinal{1}) where N = print(io, "ℵ₁")
