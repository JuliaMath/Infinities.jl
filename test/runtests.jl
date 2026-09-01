using Infinities, Base64, Test
import Infinities: Infinity, AllInfinities

using Aqua, JET

"An `AbstractString` indexed by character position, so that byte arithmetic on indices is invalid."
struct CharString <: AbstractString
    chars::Vector{Char}
end
CharString(s::AbstractString) = CharString(collect(s))
Base.ncodeunits(s::CharString) = length(s.chars)
Base.codeunit(::CharString) = Char
Base.codeunit(s::CharString, i::Integer) = s.chars[i]
Base.isvalid(s::CharString, i::Integer) = 1 ≤ i ≤ ncodeunits(s)
Base.iterate(s::CharString, i::Integer=1) = i ≤ length(s.chars) ? (s.chars[i], i + 1) : nothing

@testset "∞" begin
    @testset "∞" begin
        @test ∞ ≠ 1
        @test 1 ≠ ∞
        @test ∞ == ∞
        @test ∞ == Inf
        @test Inf == ∞

        @testset "inequalities" begin
            @test isless(1, ∞)
            @test !isless(Inf, ∞)
            @test !isless(∞, Inf)
            @test !isless(∞, 1)
            @test !isless(∞, ∞)
            @test !(∞ < ∞)
            @test ∞ ≤ ∞
            @test !(∞ > ∞)
            @test ∞ ≥ ∞

            @test 5 < ∞ && 5 ≤ ∞
            @test !(∞ < 5) && !(∞ ≤ 5)
            @test ∞ > 5 && ∞ ≥ 5
            @test !(5 > ∞) && !(5 ≥ ∞)
        end

        @test ∞ + ∞ ≡ ∞
        @test ∞ + 1 ≡ 1 + ∞ ≡ ∞ + 1.0 ≡ 1.0 + ∞ ≡ ∞
        @test ∞ - 1 ≡ ∞ - 1.0 ≡ ∞
        @test *(∞) ≡ ∞
        @test ∞*∞ ≡ ∞
        @test_throws ArgumentError ∞ - ∞

        @test one(∞) ≡ one(Infinity) ≡ oneunit(∞) ≡ oneunit(Infinity) ≡ 1
        @test zero(∞) ≡ 0

        @test !isone(∞)
        @test !iszero(∞)
        @test !signbit(∞)

        @test sign(∞) ≡ 1
        @test angle(∞) ≡ 0

        @test string(∞) == stringmime("text/plain", ∞) == "∞"

        @test isinf(∞)
        @test !isfinite(∞)

        @test Base.to_index(∞) ≡ ℵ₀

        @testset "min/max" begin
            @test max(1,∞) ≡ max(∞,1) ≡ ∞
            @test min(1,∞) ≡ min(∞,1) ≡ 1
            @test maximum([1,∞]) ≡ ∞
            @test minimum([1,∞]) ≡ 1

            @test min(∞, ∞) == ∞
            @test max(∞, ∞) == ∞
            @test min(3,∞) == 3
            @test max(3,∞) == ∞
        end

        @testset "div/fld/cld" begin
            @test div(∞, 2) ≡ ∞
            @test fld(∞, 2) ≡ ∞
            @test cld(∞, 2) ≡ ∞
            @test div(2, ∞) ≡ 0
            @test fld(2, ∞) ≡ 0
            @test cld(2, ∞) ≡ 1
            @test div(-2, ∞) ≡ 0
            @test fld(-2, ∞) ≡ -1
            @test cld(-2, ∞) ≡ 0
            @test mod(2,∞) ≡ 2
            @test div(∞,∞) isa NotANumber
            @test fld(∞,∞) isa NotANumber
            @test cld(∞,∞) isa NotANumber
            @test mod(∞,∞) isa NotANumber
            @test mod(∞,2) isa NotANumber
            @test_throws ArgumentError mod(-2,∞)
        end

        @testset "convert" begin
            @test convert(Float64, ∞) ≡ Float64(∞) ≡ Inf
            @test convert(Float32, ∞) ≡ Float32(∞) ≡ Inf32
            @test convert(Float16, ∞) ≡ Float16(∞) ≡ Inf16
            @test convert(BigFloat, ∞)::BigFloat == BigFloat(∞)::BigFloat == BigFloat(Inf)
            @test convert(RealInfinity, ∞) isa RealInfinity
            @test convert(RealInfinity, ∞) == Inf
        end
    end

    @testset "RealInfinity" begin
        @test RealInfinity(∞) ≡ convert(RealInfinity, ∞) ≡ RealInfinity() ≡
                RealInfinity(false) ≡ RealInfinity(RealInfinity())

        @test promote_type(Infinity, PositiveInfinity) == PositiveInfinity
        @test promote(∞, RealInfinity()) ≡ (RealInfinity(),RealInfinity())
        # ∞ and -∞ have no common concrete type, just like +∞ and -∞
        @test_throws ErrorException promote(∞, -∞)
        @test_throws ErrorException promote(+∞, -∞)

        @test -∞ ≡ RealInfinity(true)
        @test +∞ ≡ RealInfinity()

        @test sign(-∞) == -1
        @test angle(-∞) ≈ π

        @test ∞ == +∞ == RealInfinity(∞)
        @test RealInfinity() == ∞
        @test ∞ ≠ -∞
        @test 1 - ∞ ≡ 1.0 - ∞ ≡ -∞
        @test 1 - (-∞) ≡ 1.0 - (-∞) ≡ RealInfinity()
        @test (-∞) - 5 ≡ -∞

        @test (-∞)*(-∞) ≡ ∞*RealInfinity(∞) ≡ RealInfinity(∞)*∞

        @test !isless(RealInfinity(), RealInfinity())
        @test isless(-∞, RealInfinity())
        @test  isless(-∞, 1)
        @test !isless(-∞, -Inf)
        @test !isless(-Inf, -∞)
        @test !isless(1, -∞)

        @test -∞ ≤ ∞
        @test RealInfinity() ≤ ∞
        @test ∞ ≤ RealInfinity()
        @test -∞ ≤ -∞
        @test !(∞ ≤ -∞)
        @test -∞ < ∞
        @test !(-∞ < -∞)
        @test !(RealInfinity() < ∞) && !(∞ < RealInfinity())
        @test RealInfinity() ≥ ∞ && ∞ ≥ RealInfinity()
        @test !(-∞ > ∞)
        @test ∞ > -∞
        @test !(5 < -∞)
        @test -∞ < 5

        @test !(RealInfinity(false) < RealInfinity(false))
        @test RealInfinity(false) ≤ RealInfinity(false)
        @test RealInfinity(true) < RealInfinity(false)
        @test RealInfinity(true) ≤ RealInfinity(false)
        @test !(RealInfinity(false) < RealInfinity(true))
        @test !(RealInfinity(false) ≤ RealInfinity(true))
        @test !(RealInfinity(true) < RealInfinity(true))
        @test RealInfinity(true) ≤ RealInfinity(true)

        @test RealInfinity(true) + RealInfinity(true) == RealInfinity(true)
        @test RealInfinity(false) + RealInfinity(false) == RealInfinity(false)
        @test RealInfinity(true)+1 == RealInfinity(true)
        @test RealInfinity(false)+1 == RealInfinity(false)

        @test string(-∞) == "-∞"

        @test (-∞) + (-∞) ≡ -∞
        @test (1∞) + (1∞) ≡ 1∞
        @test ∞ + (1∞) ≡ (1∞) + ∞ ≡ 1∞

        @test_throws ArgumentError ∞ + (-∞)
        @test_throws ArgumentError (1∞) + (-∞)
        @test_throws ArgumentError (-∞) + ∞

        @test ∞ - (-∞) ≡ +∞
        @test (-∞) - ∞ ≡ -∞
        @test (1∞) - (-∞) ≡ 1∞
        @test (-∞) - (1∞) ≡ -∞

        @test_throws ArgumentError ∞ - (1∞)
        @test_throws ArgumentError (1∞) - ∞
        @test_throws ArgumentError (1∞) - (1∞)
        @test_throws ArgumentError (-∞) - (-∞)
        @test_throws ArgumentError 0*∞
        @test_throws ArgumentError 0*(-∞)
        @test_throws ArgumentError Inf - RealInfinity()
        @test_throws ArgumentError RealInfinity() - Inf

        @test (-∞)*2 ≡ 2*(-∞) ≡ -2 * ∞ ≡ ∞ * (-2) ≡ (-2) * RealInfinity() ≡ -∞
        @test (-∞)*2.3 ≡ 2.3*(-∞) ≡ -2.3 * ∞ ≡ ∞ * (-2.3) ≡ (-2.3) * RealInfinity() ≡ -∞

        @testset "power" begin
            # zero
            @test (+∞)^0.0 ≡ (-∞)^0.0 ≡ 1.0

            # positive even/odd/fraction
            @test (+∞)^2.0 ≡ (-∞)^2.0 ≡ +∞
            @test (+∞)^1.0 ≡ +∞
            @test (-∞)^1.0 ≡ -∞
            @test (+∞)^0.5 ≡ +∞
            @test_throws DomainError (-∞)^0.5

            # negative even/odd/fraction
            @test (+∞)^(-2.0) ≡ (-∞)^(-2.0) ≡ 0.0
            @test (+∞)^(-1.0) ≡ 0.0
            @test (-∞)^(-1.0) ≡ -0.0
            @test (+∞)^(-0.5) ≡ 0.0
            @test_throws DomainError (-∞)^(-0.5)

            # irrational
            @test (+∞)^π ≡ +∞
            @test_throws DomainError (-∞)^π
        end

        @test isinf(-∞)
        @test !isfinite(-∞)

        @test [∞, -∞] isa Vector{Real}
        @test [+∞, -∞] isa Vector{RealInfinity}

        @test mod(-∞, 5) isa NotANumber
        @test mod(-∞, -∞) isa NotANumber
        @test mod(5, RealInfinity()) == 5
        @test_throws ArgumentError mod(5,-∞)

        @testset "min/max" begin
            @test min(-∞, ∞) ≡ min(∞, -∞) ≡ min(-∞, RealInfinity()) ≡ -∞
            @test max(-∞, RealInfinity()) ≡ RealInfinity()
            @test max(∞, -∞) ≡ max(-∞,∞) ≡ ∞
            @test min(5, RealInfinity()) ≡ min(RealInfinity(), 5) ≡ 5
            @test min(5, -∞) ≡ min(-∞, 5) ≡ -∞
            @test max(5, RealInfinity()) ≡ max(RealInfinity(), 5) ≡ RealInfinity()
            @test max(5, -∞) ≡ max(-∞, 5) ≡ 5
        end

        @testset "convert" begin
            @test convert(Float64, -∞) ≡ Float64(-∞) ≡ -Inf
            @test convert(Float32, -∞) ≡ Float32(-∞) ≡ -Inf32
            @test convert(Float16, -∞) ≡ Float16(-∞) ≡ -Inf16
            @test convert(BigFloat, -∞)::BigFloat == BigFloat(-∞)::BigFloat == -BigFloat(Inf)
        end

        @test Base.to_index(RealInfinity()) ≡ ℵ₀
    end

    @testset "ComplexInfinity" begin
        @test ComplexInfinity(∞) ≡ convert(ComplexInfinity, ∞) ≡ ComplexInfinity() ≡
            ComplexInfinity(false) ≡ ComplexInfinity{Bool}(∞) ≡ ComplexInfinity{Bool}(RealInfinity()) ≡ ComplexInfinity{Bool}(ComplexInfinity())

        @test convert(ComplexInfinity{Bool}, ∞) ≡ convert(ComplexInfinity, ∞) ≡ ComplexInfinity()
        @test convert(ComplexInfinity{Bool}, -∞) ≡ convert(ComplexInfinity, -∞) ≡ -ComplexInfinity()

        @test isinf(ComplexInfinity())
        @test !isfinite(ComplexInfinity())

        @test promote(∞, RealInfinity(), ComplexInfinity()) ≡ ntuple(_ -> ComplexInfinity(), 3)
        @test promote_type(Infinity, ComplexInfinity{Bool}) == promote_type(RealInfinity, ComplexInfinity{Bool}) == ComplexInfinity{Bool}


        @test ComplexInfinity(∞) == ∞
        @test ∞ == ComplexInfinity(∞)
        @test ComplexInfinity(∞) == RealInfinity()
        @test RealInfinity() == ComplexInfinity(∞)
        @test ComplexInfinity(-∞) == -∞
        @test  -∞ == ComplexInfinity(-∞)
        @test ∞ + im ≡ im + ∞ ≡ ∞ + 1.0im ≡ 1.0im + ∞ ≡ ∞ - im ≡ ∞ - 1.0im ≡ ComplexInfinity()
        @test RealInfinity() + im ≡ im + RealInfinity() ≡ RealInfinity() + 1.0im ≡ 1.0im + RealInfinity() ≡ RealInfinity() - im ≡ RealInfinity() - 1.0im ≡ ComplexInfinity()

        @test im - ∞ ≡ 1.0im - ∞ ≡ -ComplexInfinity()
        @test im - ComplexInfinity() ≡ 1.0im - ComplexInfinity() ≡ -ComplexInfinity()
        @test ComplexInfinity() - im ≡ ComplexInfinity() - 1.0im ≡ ComplexInfinity()

        @test ComplexInfinity() + ∞ ≡ ComplexInfinity() + RealInfinity() ≡
                ∞ + ComplexInfinity() ≡ RealInfinity() + ComplexInfinity() ≡ ComplexInfinity()
        @test ComplexInfinity(true) + ComplexInfinity(true) == ComplexInfinity(true)
        @test ComplexInfinity(false) + ComplexInfinity(false) == ComplexInfinity(false)
        @test ComplexInfinity(true)+1 == ComplexInfinity(true)
        @test ComplexInfinity(false)+1 == ComplexInfinity(false)

        @test ∞ * ComplexInfinity() ≡ RealInfinity() * ComplexInfinity() ≡
             ComplexInfinity() * ∞ ≡ ComplexInfinity() * RealInfinity() ≡ ComplexInfinity()

        @test  2.0im*∞ ≡ ∞*2.0im ≡ 2.0im * RealInfinity() ≡ RealInfinity() * 2.0im ≡ ComplexInfinity(1/2)
        @test 2ComplexInfinity() ≡ ComplexInfinity()*2 ≡ ComplexInfinity()

        @test exp(im*π/4)*∞ == Inf+im*Inf
        @test exp(im*π/4)+∞ == ∞
        @test Inf + im + ∞ ≡ ComplexInfinity()

        @test Inf == ComplexInfinity()
        @test ComplexInfinity() == Inf

        @test isless(-ComplexInfinity(), ComplexInfinity())
        @test isless(5, ComplexInfinity())
        @test !isless(ComplexInfinity(), 5)

        @test 5 < ComplexInfinity() && 5 ≤ ComplexInfinity()
        @test !(ComplexInfinity() < 5) && !(ComplexInfinity() ≤ 5)
        @test 5 > -ComplexInfinity() && 5 ≥ -ComplexInfinity()
        @test ComplexInfinity() > 5 && ComplexInfinity() ≥  5

        @test 1 + ComplexInfinity() ≡ 1.0 + ComplexInfinity() ≡ ComplexInfinity() + 1 ≡ ComplexInfinity() + 1.0 ≡ ComplexInfinity()
        @test 5 * ComplexInfinity() ≡ ComplexInfinity()
        @test (-5) * ComplexInfinity() ≡ -ComplexInfinity()

        @test ComplexInfinity(0.25) * ComplexInfinity(0.5) ≡ ComplexInfinity(0.75)
        @test ComplexInfinity(0.0) + ComplexInfinity() ≡ ComplexInfinity() + ComplexInfinity(0.0) ≡ ComplexInfinity(0.0)

        @test mod(ComplexInfinity(), 5) ≡ NotANumber()

        @test stringmime("text/plain", ComplexInfinity()) == "exp(false*im*π)∞"

        @testset "min/max" begin
            @test min(ComplexInfinity(), -ComplexInfinity()) ≡ -ComplexInfinity()
            @test max(ComplexInfinity(), -ComplexInfinity()) ≡ ComplexInfinity()
            @test min(ComplexInfinity(), 5) ≡ min(5,ComplexInfinity())  ≡ 5
            @test max(ComplexInfinity(), 5) ≡ max(5,ComplexInfinity())  ≡ ComplexInfinity()
        end

        @testset "fld/cld/div" begin
            @test div(ComplexInfinity(), 5) ≡ fld(ComplexInfinity(), 5) ≡ ComplexInfinity()
            @test div(-ComplexInfinity(),2) ≡ -ComplexInfinity()
        end

        @test signbit(ComplexInfinity(3))
        @test !signbit(ComplexInfinity(100))
    end

    @testset "Set" begin
        s = Set([∞,1])
        @test 1 in s
        @test ∞ in s
        @test 2 ∉ s
    end

    @testset "hash" begin
        infinities = (∞, +∞, -∞, Inf, -Inf, Inf32, -Inf32, Inf16, -Inf16, big(Inf), -big(Inf),
                      InfiniteCardinal{0}(), ComplexInfinity(false),
                      ComplexInfinity(true), ComplexInfinity(0.1))

        # isequal must imply equal hashes
        for a in infinities, b in infinities
            isequal(a, b) && @test hash(a) == hash(b)
        end

        @test hash(+∞) ≠ hash(-∞)
        @test hash(ℵ₀) ≠ hash(ℵ₁)

        for x in (infinities..., ℵ₁)
            @test hash(x, UInt(1)) isa UInt
            @test hash((x,)) isa UInt
        end
    end

    @testset "Base.literal_pow" begin
        @test Base.literal_pow(^, ℵ₀, Val(0)) ≡ ℵ₀^0 ≡ 1
        @test Base.literal_pow(^, ℵ₀, Val(1)) ≡ ℵ₀^1 ≡ ℵ₀
        @test Base.literal_pow(^, ℵ₀, Val(-1)) ≡ ℵ₀^(-1) ≡ 0
        @test Base.literal_pow(^, ℵ₀, Val(2)) ≡ ℵ₀^2 ≡ ℵ₀
        @test Base.literal_pow(^, ℵ₀, Val(-2)) ≡ ℵ₀^(-2) ≡ 0

        @test Base.literal_pow(^, ∞, Val(0)) ≡ ∞^0 ≡ 1
        @test Base.literal_pow(^, ∞, Val(1)) ≡ ∞^1 ≡ ∞
        @test Base.literal_pow(^, ∞, Val(-1)) ≡ ∞^(-1) ≡ 0
        @test Base.literal_pow(^, ∞, Val(2)) ≡ ∞^2 ≡ ∞
        @test Base.literal_pow(^, ∞, Val(-2)) ≡ ∞^(-2) ≡ 0

        @test Base.literal_pow(^, +∞, Val(0)) ≡ (+∞)^0 ≡ 1.0
        @test Base.literal_pow(^, +∞, Val(1)) ≡ (+∞)^1 ≡ +∞
        @test Base.literal_pow(^, +∞, Val(-1)) ≡ (+∞)^(-1) ≡ 0.0
        @test Base.literal_pow(^, +∞, Val(2)) ≡ (+∞)^2 ≡ +∞
        @test Base.literal_pow(^, +∞, Val(-2)) ≡ (+∞)^(-2) ≡ 0.0

        @test Base.literal_pow(^, -∞, Val(0)) ≡ (-∞)^0 ≡ 1.0
        @test Base.literal_pow(^, -∞, Val(1)) ≡ (-∞)^1 ≡ -∞
        @test Base.literal_pow(^, -∞, Val(-1)) ≡ (-∞)^(-1) ≡ (VERSION < v"1.12-" ?  0.0 : -0.0)
        @test Base.literal_pow(^, -∞, Val(2)) ≡ (-∞)^2 ≡ +∞
        @test Base.literal_pow(^, -∞, Val(-2)) ≡ (-∞)^(-2) ≡ 0.0

        @test Base.literal_pow(^, ComplexInfinity(0.1), Val(0)) ≡ ComplexInfinity(0.1)^0 ≡ 1.0+0.0im
        @test Base.literal_pow(^, ComplexInfinity(0.1), Val(1)) ≡ (ComplexInfinity(0.1))^1 ≡ ComplexInfinity(0.1)
        @test Base.literal_pow(^, ComplexInfinity(0.1), Val(-1)) ≡ (ComplexInfinity(0.1))^(-1) ≡ 0.0+0.0im
    end

    @testset "one/zero/oneunit" begin
        @test one(ℵ₀) ≡ one(∞)≡ one(ℵ₀) ≡ oneunit(∞) ≡ one(Infinity) ≡ one(InfiniteCardinal{0}) ≡ oneunit(Infinity) ≡ oneunit(InfiniteCardinal{0})  ≡ 1
        @test one(-∞) ≡ oneunit(-∞) ≡ one(RealInfinity) ≡ oneunit(RealInfinity) ≡ 1.0
        @test one(exp(0.1im)∞) ≡ oneunit(exp(0.1im)∞) ≡ one(ComplexInfinity) ≡ oneunit(ComplexInfinity) ≡ 1.0+0.0im

        @test zero(ℵ₀) ≡ zero(∞) ≡ zero(Infinity) ≡ zero(InfiniteCardinal{0}) ≡ 0
        @test zero(-∞) ≡ zero(RealInfinity) ≡ 0.0
        @test zero(exp(0.1im)∞) ≡ zero(ComplexInfinity) ≡ 0.0+0.0im
    end

    @testset "float precisions" begin
        for T in (Float16, Float32, Float64, BigFloat)
            for inf in (∞, +∞, ComplexInfinity(), ℵ₀)
                @test T(Inf) == inf == T(Inf)
                @test T(-Inf) ≠ inf
            end
            for inf in (-∞, -ComplexInfinity())
                @test T(-Inf) == inf == T(-Inf)
                @test T(Inf) ≠ inf
            end
        end
    end

    @testset "isinf(x, y)" begin
        # ℵ₁ points in the same direction as ∞, even though `ℵ₁ == ∞` is false
        positive = (∞, +∞, ℵ₀, ℵ₁, ComplexInfinity(), Inf, Inf32, Inf16, big(Inf))
        negative = (-∞, -ComplexInfinity(), -Inf, -Inf32, -Inf16, -big(Inf))
        imaginary = (ComplexInfinity(0.5), complex(0.0, Inf))
        others = (0, 1.5, -2, -1.5, 0.0, -0.0, NaN, NaN32, prevfloat(Inf), nextfloat(-Inf),
                  nextfloat(0.0), prevfloat(-0.0), "∞", "-∞")

        for xs in (positive, negative, imaginary, others), ys in (positive, negative, imaginary)
            for x in xs, y in ys
                y isa AllInfinities || continue # only our own infinities are admissible as a reference
                @test isinf(x, y) == (xs === ys)
                # `==` asks the narrower question, and `ℵ₁` is the whole of the difference
                if x !== ℵ₁ && y !== ℵ₁
                    @test isinf(x, y) == (x == y)
                end
            end
        end
        @test isinf(ℵ₁, ∞) && isinf(∞, ℵ₁) && isinf(Inf, ℵ₁) && isinf(ℵ₁, ℵ₀)
        @test ℵ₁ ≠ ∞ && ∞ ≠ ℵ₁ && Inf ≠ ℵ₁ && ℵ₁ ≠ ℵ₀
    end

    @testset "NaN" begin
        for nan in (NaN, NaN32, NaN16, big(NaN)), inf in (∞, +∞, -∞, ℵ₀)
            # a numeric comparison is false in every direction
            for op in (<, ≤, >, ≥, ==)
                @test !op(nan, inf) && !op(inf, nan)
            end

            # the sort order puts `NaN` after every value, an infinity included
            @test isless(inf, nan) && !isless(nan, inf)
            @test (isless(nan, inf), isless(inf, nan), isequal(nan, inf)) |> count == 1

            # `max` and `min` propagate `NaN`, as they do over the floats alone
            @test isnan(max(nan, inf)) && isnan(max(inf, nan))
            @test isnan(min(nan, inf)) && isnan(min(inf, nan))
        end
        sorted = sort([∞, NaN, 1.0, -∞])
        @test sorted[1] === -∞ && sorted[2] === 1.0 && sorted[3] === ∞ && isnan(sorted[4])
    end

    @testset "ordinary values" begin
        for inf in (∞, +∞, ℵ₀)
            @test 1.0 < inf && !(inf < 1.0) && 1.0 ≤ inf && inf ≥ 1.0
            @test isless(1.0, inf) && !isless(inf, 1.0)
            @test max(1.0, inf) === max(inf, 1.0) === inf
            @test min(1.0, inf) === min(inf, 1.0) === 1.0
        end
        @test -∞ < 1 < ∞ && -∞ ≤ -∞ && ∞ ≤ ∞
        @test !(Inf < ∞) && !(∞ < Inf) && Inf ≤ ∞
        @test max(-∞, ∞) === ∞ && min(-∞, ∞) === -∞
    end

    @testset "parsing" begin
        @test tryparse(NegativeInfinity, "-∞") == NegativeInfinity()
        @test tryparse(NegativeInfinity, " - ∞ ") == NegativeInfinity()
        @test tryparse(NegativeInfinity, "∞ ") === nothing
        @test tryparse(NegativeInfinity, "3-∞") === nothing
        @test tryparse(NegativeInfinity, "-+∞") === nothing
        @test tryparse(NegativeInfinity, "-∞2") === nothing
        @test tryparse(NegativeInfinity, "") === nothing
        @test tryparse(NegativeInfinity, "  ") === nothing
        @test tryparse(NegativeInfinity, "- ") === nothing

        @test tryparse(PositiveInfinity, "+∞") == PositiveInfinity()
        @test tryparse(PositiveInfinity, " + ∞ ") == PositiveInfinity()
        @test tryparse(PositiveInfinity, "∞") == PositiveInfinity()
        @test tryparse(PositiveInfinity, " ∞ ") == PositiveInfinity()
        @test tryparse(PositiveInfinity, "-∞") === nothing
        @test tryparse(PositiveInfinity, "+-∞") === nothing
        @test tryparse(PositiveInfinity, "--∞") === nothing
        @test tryparse(PositiveInfinity, "-∞∞") === nothing
        @test tryparse(PositiveInfinity, "") === nothing
        @test tryparse(PositiveInfinity, "  ") === nothing
        @test tryparse(PositiveInfinity, "+ ") === nothing

        @test tryparse(RealInfinity, "-∞") == NegativeInfinity()
        @test tryparse(RealInfinity, " - ∞ ") == NegativeInfinity()
        @test tryparse(RealInfinity, "+∞") == PositiveInfinity()
        @test tryparse(RealInfinity, " ∞ ") == PositiveInfinity()
        @test tryparse(RealInfinity, "") === nothing
        @test tryparse(RealInfinity, "  ") === nothing
        @test tryparse(RealInfinity, "-∞2") === nothing
        @test tryparse(RealInfinity, "3") === nothing

        @testset "strings whose indices are not byte offsets" begin
            @test tryparse(NegativeInfinity, CharString("-∞")) == NegativeInfinity()
            @test tryparse(NegativeInfinity, CharString(" - ∞ ")) == NegativeInfinity()
            @test tryparse(NegativeInfinity, CharString("-∞2")) === nothing
            @test tryparse(PositiveInfinity, CharString("+∞")) == PositiveInfinity()
            @test tryparse(PositiveInfinity, CharString(" ∞ ")) == PositiveInfinity()
            @test tryparse(PositiveInfinity, CharString("∞∞")) === nothing
            @test tryparse(RealInfinity, CharString("-∞")) == NegativeInfinity()
            @test tryparse(RealInfinity, CharString("∞")) == PositiveInfinity()
            @test tryparse(RealInfinity, CharString("")) === nothing
        end
    end
end



include("test_cardinality.jl")
include("test_ambiguity.jl")

@testset "Project quality" begin
    Aqua.test_all(Infinities)
    test_package(Infinities)
end
