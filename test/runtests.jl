using Infinities, Base64, Test
import Infinities: Infinity, AllInfinities, _isinf

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
        @test ∞ - ∞ ≡ NotANumber()

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

        @test ∞ + (-∞) ≡ (1∞) + (-∞) ≡ (-∞) + ∞ ≡ NotANumber()

        @test ∞ - (-∞) ≡ +∞
        @test (-∞) - ∞ ≡ -∞
        @test (1∞) - (-∞) ≡ 1∞
        @test (-∞) - (1∞) ≡ -∞

        # summing opposite directions is undefined, as it is over the floats
        @test ∞ - (1∞) ≡ (1∞) - ∞ ≡ (1∞) - (1∞) ≡ (-∞) - (-∞) ≡ NotANumber()
        @test Inf - RealInfinity() ≡ RealInfinity() - Inf ≡ NotANumber()
        @test 0*∞ ≡ 0*(-∞) ≡ NotANumber()

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
        # every spelling of the positive real axis is the same value, a cardinal included
        @test ComplexInfinity(∞) ≡ convert(ComplexInfinity, ∞) ≡ ComplexInfinity() ≡
            ComplexInfinity(0x0000000000000000) ≡ ComplexInfinity(RealInfinity()) ≡
            ComplexInfinity(ComplexInfinity()) ≡ ComplexInfinity(ℵ₀)

        @test convert(ComplexInfinity, -∞) ≡ -ComplexInfinity()
        # one direction is one value, however it is spelled
        @test ComplexInfinity(halfturns = -0.5) ≡ ComplexInfinity(halfturns = 1.5) ≡ -im*∞
        @test ComplexInfinity(halfturns = 1) ≡ ComplexInfinity(halfturns = 3) ≡ ComplexInfinity(-∞)
        @test isreal(ComplexInfinity()) && isreal(-ComplexInfinity()) && !isreal((1+im)*∞)
        # a rational direction converts without a float step
        @test reinterpret(UInt64, ComplexInfinity(halfturns = 2//3)) ≡ 0x5555555555555555
        # a numerator too wide for `Int128` takes the `BigInt` route to the same count
        @test ComplexInfinity(halfturns = big(1)//3) ≡ ComplexInfinity(halfturns = 1//3)
        @test ComplexInfinity(0x4000000000000000) ≡ ComplexInfinity(halfturns = 1//2) ≡ im*∞
        # `mod` rounds a hair below the axis up to a full turn, which has no count of its own
        @test ComplexInfinity(halfturns = -1e-300) ≡ ComplexInfinity(halfturns = 2.0) ≡ ComplexInfinity()
        @test complex(1.0, -1e-17)*∞ ≡ ComplexInfinity()
        # no count names a direction that is not one, so the conversion has to refuse
        for h in (NaN, Inf, -Inf)
            @test_throws InexactError ComplexInfinity(halfturns = h)
        end
        # the count runs forwards, `angle` reports it on `Base`'s branch of `(-π, π]`
        for h in (0.0, 0.25, 0.5, 1.0, -0.25, -0.5, -0.75)
            @test angle(ComplexInfinity(halfturns = h)) ≡ h*π
            @test complex(cospi(h), sinpi(h))*∞ == ComplexInfinity(halfturns = h)
        end
        # off the axes the angle has to be rounded to reach a count
        @test reinterpret(UInt64, exp(im*π/8)*∞) - reinterpret(UInt64, ComplexInfinity(halfturns = 1//8)) ≡
            0x0000000000000100
        @test angle(exp(im*0.3)*∞) ≈ angle(∞*exp(im*0.3)) ≈ 0.3
        # the count is finer than an angle in a `Float64`, so equality has to read the count
        @test ComplexInfinity(0x7fffffffffffffff) ≠ -ComplexInfinity()
        @test im*∞ * ComplexInfinity(0x0000000000000001) ≠ im*∞

        @test isinf(ComplexInfinity())
        @test !isfinite(ComplexInfinity())

        @test promote(∞, RealInfinity(), ComplexInfinity()) ≡ ntuple(_ -> ComplexInfinity(), 3)
        @test promote_type(Infinity, ComplexInfinity) == promote_type(RealInfinity, ComplexInfinity) == ComplexInfinity


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
        @test ComplexInfinity(-∞) + ComplexInfinity(-∞) == ComplexInfinity(-∞)
        @test ComplexInfinity() + ComplexInfinity() == ComplexInfinity()
        @test ComplexInfinity(-∞)+1 == ComplexInfinity(-∞)
        @test ComplexInfinity()+1 == ComplexInfinity()

        # An infinite summand reaches `_infadd` through `toinf`, which has to give half turns
        @test complex(Inf, 0.0) + ∞ ≡ ComplexInfinity()
        @test complex(-Inf, 0.0) + (-∞) ≡ -ComplexInfinity()
        @test complex(0.0, Inf) + im*∞ ≡ im*∞
        @test complex(0.0, -Inf) + (-im*∞) ≡ -im*∞
        @test complex(0.0, Inf) + ∞ ≡ complex(NotANumber(), NotANumber())
        # two infinite parts are the only way an infinite `Complex` points off the axes
        for (z, inf) in ((complex(Inf, Inf), (1+im)*∞), (complex(-Inf, Inf), (-1+im)*∞),
                         (complex(-Inf, -Inf), (-1-im)*∞), (complex(Inf, -Inf), (1-im)*∞))
            @test z + inf ≡ inf
        end

        @test ∞ * ComplexInfinity() ≡ RealInfinity() * ComplexInfinity() ≡
             ComplexInfinity() * ∞ ≡ ComplexInfinity() * RealInfinity() ≡ ComplexInfinity()

        @test  2.0im*∞ ≡ ∞*2.0im ≡ 2.0im * RealInfinity() ≡ RealInfinity() * 2.0im ≡ im*∞
        @test 2ComplexInfinity() ≡ ComplexInfinity()*2 ≡ ComplexInfinity()
        # a factor gives the direction it actually has, so rescaling moves it once it rounds
        @test 4*(0.3+0.1im)*∞ ≡ (0.3+0.1im)*∞
        @test 3*(0.3+0.1im)*∞ ≢ (0.3+0.1im)*∞

        @test exp(im*π/4)*∞ == Inf+im*Inf
        @test exp(im*π/4)+∞ == ∞
        @test Inf + im + ∞ ≡ ComplexInfinity()

        @test Inf == ComplexInfinity()
        @test ComplexInfinity() == Inf

        # the complex plane carries no order, so these are undefined as they are for `Complex`
        for op in (isless, <, ≤, >, ≥, min, max), y in (5, ComplexInfinity(), (1+im)*∞)
            @test_throws MethodError op(ComplexInfinity(), y)
            @test_throws MethodError op(y, ComplexInfinity())
        end
        # a direction on the axis converts, as `Real(::Complex)` does
        @test RealInfinity(ComplexInfinity()) ≡ +∞
        @test RealInfinity(-ComplexInfinity()) ≡ -∞
        @test_throws InexactError RealInfinity((1+im)*∞)
        @test 5 < RealInfinity(ComplexInfinity())

        @test 1 + ComplexInfinity() ≡ 1.0 + ComplexInfinity() ≡ ComplexInfinity() + 1 ≡ ComplexInfinity() + 1.0 ≡ ComplexInfinity()
        @test 5 * ComplexInfinity() ≡ ComplexInfinity()
        @test (-5) * ComplexInfinity() ≡ -ComplexInfinity()

        @test (1+im)*∞ * (im*∞) ≡ (-1+im)*∞
        @test (2.0+0.0im)*∞ + ComplexInfinity() ≡ ComplexInfinity() + (2.0+0.0im)*∞ ≡ ComplexInfinity()

        @test stringmime("text/plain", ComplexInfinity()) == "cispi(0.0)∞"
        # a count an angle cannot name is shown as itself, so every form reads back
        @test sprint(show, ComplexInfinity(0x5555555555555555)) == "ComplexInfinity(0x5555555555555555)"
        for u in (0x0000000000000000, 0x4000000000000000, 0x5555555555555555, 0xdeadbeefdeadbeef)
            @test Core.eval(@__MODULE__, Meta.parse(sprint(show, ComplexInfinity(u)))) ≡ ComplexInfinity(u)
        end

        @testset "integer operations" begin
            # an integer operation needs a real, and `Base` defines none of these for a `Complex`
            for op in (div, fld, cld, mod, rem), x in (ComplexInfinity(), (1+im)*∞)
                @test_throws MethodError op(x, 5)
                @test_throws MethodError op(5, x)
            end
            @test div(RealInfinity(ComplexInfinity()), 5) ≡ +∞
        end

        @test signbit(ComplexInfinity(halfturns = 3))
        @test !signbit(ComplexInfinity(halfturns = 100))
        # `signbit` returns a `Bool` for every angle, as it does over the reals
        @test signbit(ComplexInfinity(-∞)) === signbit(-ComplexInfinity()) === true
        @test signbit(im*∞) === signbit(ComplexInfinity()) === false

        @testset "abs/sign/conj/-" begin
            @test -(im*∞) ≡ -im*∞
            @test -(-(im*∞)) ≡ im*∞
            @test -ComplexInfinity() ≡ ComplexInfinity(-∞)
            @test abs(ComplexInfinity()) ≡ abs(im*∞) ≡ ∞
            @test sign(im*∞) ≡ complex(0.0, 1.0)
            @test sign(ComplexInfinity()) ≡ complex(1.0, 0.0)
            @test sign(ComplexInfinity(-∞)) ≡ complex(-1.0, 0.0)
            # conjugation negates the direction, so on the real axis it changes nothing
            @test conj((1+im)*∞) ≡ (1-im)*∞
            @test conj(conj((1+im)*∞)) ≡ (1+im)*∞
            @test conj(ComplexInfinity(-∞)) ≡ ComplexInfinity(-∞)
        end

        @testset "float" begin
            @test float(ComplexInfinity()) ≡ float((2.0+0.0im)*∞) ≡ complex(Inf, 0.0)
            @test float(im*∞) ≡ complex(0.0, Inf)
            @test float(ComplexInfinity(-∞)) ≡ complex(-Inf, 0.0)
            @test float(-im*∞) ≡ float(ComplexInfinity(halfturns = 3/2)) ≡ complex(0.0, -Inf)
            # `Complex` points along eight rays only, so every other angle collapses onto the nearest
            @test float((1+im)*∞) ≡ float(ComplexInfinity(0x1000000000000000)) ≡ complex(Inf, Inf)
        end
    end

    @testset "Set" begin
        s = Set([∞,1])
        @test 1 in s
        @test ∞ in s
        @test 2 ∉ s
    end

    @testset "hash" begin
        infinities = (∞, +∞, -∞, Inf, -Inf, Inf32, -Inf32, Inf16, -Inf16, big(Inf), -big(Inf),
                      InfiniteCardinal{0}(), ComplexInfinity(),
                      ComplexInfinity(-∞), ComplexInfinity(0x1000000000000000),
                      # counts that an angle in a `Float64` cannot tell apart
                      ComplexInfinity(0x7fffffffffffffff),
                      im*∞ * ComplexInfinity(0x0000000000000001))

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

        @test Base.literal_pow(^, ComplexInfinity(0x1000000000000000), Val(0)) ≡ ComplexInfinity(0x1000000000000000)^0 ≡ 1.0+0.0im
        @test Base.literal_pow(^, ComplexInfinity(0x1000000000000000), Val(1)) ≡ (ComplexInfinity(0x1000000000000000))^1 ≡ ComplexInfinity(0x1000000000000000)
        @test Base.literal_pow(^, ComplexInfinity(0x1000000000000000), Val(-1)) ≡ (ComplexInfinity(0x1000000000000000))^(-1) ≡ 0.0+0.0im
    end

    @testset "one/zero/oneunit" begin
        @test one(ℵ₀) ≡ one(∞)≡ one(ℵ₀) ≡ oneunit(∞) ≡ one(Infinity) ≡ one(InfiniteCardinal{0}) ≡ oneunit(Infinity) ≡ oneunit(InfiniteCardinal{0})  ≡ 1
        @test one(-∞) ≡ oneunit(-∞) ≡ one(RealInfinity) ≡ oneunit(RealInfinity) ≡ 1.0
        @test one(exp(0.1im)∞) ≡ oneunit(exp(0.1im)∞) ≡ one(ComplexInfinity) ≡ oneunit(ComplexInfinity) ≡ 1.0+0.0im

        @test zero(ℵ₀) ≡ zero(∞) ≡ zero(Infinity) ≡ zero(InfiniteCardinal{0}) ≡ 0
        @test zero(-∞) ≡ zero(RealInfinity) ≡ 0.0
        @test zero(exp(0.1im)∞) ≡ zero(ComplexInfinity) ≡ 0.0+0.0im
    end

    @testset "isinteger/round" begin
        infinities = (∞, +∞, -∞, ℵ₀, ComplexInfinity(), (1+im)*∞)
        @test !isinteger(∞) && !isinteger(+∞) && !isinteger(-∞)
        @test !isinteger(ComplexInfinity()) && !isinteger((1+im)*∞)
        @test isinteger(ℵ₀) # an `InfiniteCardinal` is an `Integer`
        @test ∞ ∉ 1:5 # `in` asks a range for `isinteger` before comparing
        for f in (round, floor, ceil, trunc), x in infinities
            @test f(x) ≡ f(x; digits=2) ≡ x
        end
        for r in (RoundNearest, RoundUp, RoundDown, RoundToZero), x in infinities
            @test round(x, r) ≡ round(x, r; digits=2) ≡ x
        end
    end

    @testset "division" begin
        @test ∞ / 2 ≡ 2 \ ∞ ≡ +∞
        @test (-∞) / 2 ≡ ∞ / -2 ≡ -∞
        # a zero divisor keeps the direction, and its own sign is the one that counts
        @test ∞ / 0 ≡ ∞ / 0.0 ≡ (-∞) / (-0.0) ≡ +∞
        @test (-∞) / 0 ≡ (-∞) / 0.0 ≡ ∞ / (-0.0) ≡ -∞
        @test im*∞ / 2 ≡ im*∞
        # dividing by a complex turns the direction by its angle
        @test (+∞) / (1+im) ≡ (1-im)*∞
        @test 2 / -∞ ≡ -0.0
        @test 2 / ∞ == ∞ \ 2 == 2 / ℵ₀ == 0 # the type follows `inv`, which returns an `Int` for `∞`
        # `∞` is positive, so the quotient keeps the dividend exact; a signed infinity
        # needs a float to carry `-0.0`
        @test (2//3) / ∞ ≡ (2//3) / ℵ₀ ≡ 0//1
        @test (2//3) / (+∞) ≡ 0.0
        @test ∞ / ∞ isa NotANumber
        # a complex operand on either side makes the undefined quotient complex
        @test ComplexInfinity() / ∞ ≡ ∞ / ComplexInfinity() ≡
              ComplexInfinity() / ComplexInfinity() ≡ complex(NotANumber(), NotANumber())
        @test isnan(NaN / ∞) && isnan(∞ / NaN)
    end

    @testset "rem/divrem" begin
        @test 3 % ∞ ≡ rem(3, -∞) ≡ 3 % ℵ₀ ≡ 3
        @test -3 % ∞ ≡ -3 # `rem` keeps the sign of the dividend, where `mod(-3, ∞)` is unbounded
        @test rem(∞, 3) isa NotANumber && rem(∞, ∞) isa NotANumber
        @test isnan(rem(NaN, ∞))
        @test divrem(3, ∞) ≡ (0, 3) && divrem(-3, ∞) ≡ (0, -3)
        # `Base` has an `Integer`-only `divrem` that avoids `rem`, and `ℵ₀` is an `Integer`
        @test divrem(3, ℵ₀) ≡ (0, 3)
        @test divrem(ℵ₀, 3) ≡ (ℵ₀, NotANumber())
        @test divrem(ℵ₀, ℵ₀) ≡ (NotANumber(), NotANumber())
        # `Rational` and `BigInt` bring their own `Base` methods, which need methods of their own
        @test rem(1//2, ∞) ≡ rem(1//2, ℵ₀) ≡ 1//2
        @test rem(ℵ₀, 1//2) isa NotANumber
        @test divrem(big(3), ∞) == divrem(big(3), ℵ₀) == (0, 3)
    end

    @testset "isapprox" begin
        @test ∞ ≈ Inf && -∞ ≈ -Inf32 && ℵ₀ ≈ ∞ && ∞ ≈ ∞
        @test !(∞ ≈ 1e300) && !(∞ ≈ -∞)
        @test Inf ≈ ∞ && -Inf32 ≈ -∞ && !(1e300 ≈ ∞) # the infinity may stand on either side
        # an `InfiniteCardinal` is an `Integer`, for which `Base` has its own `isapprox`
        @test ℵ₀ ≈ ℵ₀ && !(ℵ₀ ≈ ℵ₁)
        @test !(ℵ₀ ≈ 3) && !(3 ≈ ℵ₀)
        @test isapprox(∞, Inf; atol=1) # the keywords are accepted, but nothing is near an infinity
        @test !isapprox(∞, 1; atol=∞)

        @testset "infinite tolerance" begin
            values = (0, 1, -2, 1.5, 0.0, NaN, Inf, -Inf)
            for x in values, y in values, (inf, flt) in ((∞, Inf), (+∞, Inf), (-∞, -Inf), (ℵ₀, Inf))
                @test isapprox(x, y; atol=inf) == isapprox(x, y; atol=flt)
                # Skipped over a `Base` bug: its `Integer` method evaluates `rtol * 0` for two zeros, so `isapprox(0, 0; rtol=Inf)` is `false`.
                x isa Integer && y isa Integer && iszero(x) && iszero(y) && continue
                @test isapprox(x, y; rtol=inf) == isapprox(x, y; rtol=flt)
            end
        end
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
            @test T(2) + ∞ ≡ ∞ + T(2) ≡ ∞
            @test T(2) * +∞ ≡ (+∞)^T(2) ≡ +∞
        end
    end

    @testset "_isinf(x, y)" begin
        # ℵ₁ points in the same direction as ∞, even though `ℵ₁ == ∞` is false
        positive = (∞, +∞, ℵ₀, ℵ₁, ComplexInfinity(), Inf, Inf32, Inf16, big(Inf))
        negative = (-∞, -ComplexInfinity(), -Inf, -Inf32, -Inf16, -big(Inf))
        imaginary = (im*∞, complex(0.0, Inf))
        others = (0, 1.5, -2, -1.5, 0.0, -0.0, NaN, NaN32, prevfloat(Inf), nextfloat(-Inf),
                  nextfloat(0.0), prevfloat(-0.0), "∞", "-∞")

        for xs in (positive, negative, imaginary, others), ys in (positive, negative, imaginary)
            for x in xs, y in ys
                y isa AllInfinities || continue # only our own infinities are admissible as a reference
                @test _isinf(x, y) == (xs === ys)
                # `==` asks the narrower question, and `ℵ₁` is the whole of the difference
                if x !== ℵ₁ && y !== ℵ₁
                    @test _isinf(x, y) == (x == y)
                end
            end
        end
        @test _isinf(ℵ₁, ∞) && _isinf(∞, ℵ₁) && _isinf(Inf, ℵ₁) && _isinf(ℵ₁, ℵ₀)
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

    @testset "NaN arithmetic" begin
        # the result is the package's own undefined value, as `Inf + ∞` is its own infinity
        for nan in (NaN, NaN32, NaN16, big(NaN)), inf in (∞, +∞, -∞, ℵ₀)
            for op in (+, -, *, div, fld, cld)
                @test op(nan, inf) ≡ op(inf, nan) ≡ NotANumber()
            end
            # `mod(inf, x)` discards `x`, so only one order is needed
            @test mod(nan, inf) ≡ NotANumber()
        end
        for nan in (NaN, NaN32, NaN16, big(NaN)), inf in (ComplexInfinity(), -ComplexInfinity())
            # arithmetic is defined for a complex operand, so the undefined result is complex
            for op in (+, -, *)
                @test op(nan, inf) ≡ op(inf, nan) ≡ complex(NotANumber(), NotANumber())
            end
            # `Base` defines no integer operation for a `Complex`, and neither do we
            for op in (div, fld, cld, mod, rem)
                @test_throws MethodError op(nan, inf)
            end
        end
        for nan in (NaN, NaN32, NaN16, big(NaN)), inf in (+∞, -∞)
            @test inf^nan ≡ NotANumber()
        end
    end

    @testset "NotANumber" begin
        nan = NotANumber()
        # every operand it can meet, itself included
        reals = (nan, 0, 1.5, ∞, +∞, -∞, ℵ₀, NaN, NaN32)
        complexes = (ComplexInfinity(), (1+im)*∞, complex(1.0, 2.0), complex(true, false))
        operands = (reals..., complexes...)
        @test isnan(nan) && !isinf(nan) && !isfinite(nan) && !iszero(nan) && !isone(nan) && !signbit(nan)
        @test !isinteger(nan)
        # a `NaN` of any real type is real, and this is the type-independent one
        @test isreal(nan)
        for f in (round, floor, ceil, trunc)
            @test f(nan) ≡ f(nan; digits=2) ≡ nan
        end
        for r in (RoundNearest, RoundUp, RoundDown, RoundToZero)
            @test round(nan, r) ≡ round(nan, r; digits=2) ≡ nan
        end

        # a numeric comparison is false in every direction, against itself included
        for op in (==, <, ≤, >, ≥, isapprox), x in operands
            @test !op(nan, x) && !op(x, nan)
        end

        # `isequal` and `hash` still identify it, as they do for `NaN`
        @test isequal(nan, nan) && isequal(nan, NaN) && isequal(NaN32, nan)
        @test hash(nan) == hash(NaN)

        # the sort order puts it last, alongside `NaN`
        @test sort([1.0, nan, ∞, -∞])[end] ≡ nan
        for x in operands
            @test !isless(nan, x) && isless(x, nan) == !isnan(x)
        end

        # it converts to the `NaN` of whichever float type is asked for
        @test Float64(nan) ≡ float(nan) ≡ NaN
        @test Float32(nan) ≡ NaN32 && Float16(nan) ≡ NaN16
        @test isnan(BigFloat(nan))

        # anything computed from it is undefined again
        for op in (+, -, *, /, ^, div, fld, cld, mod, rem, min, max), x in reals
            @test op(nan, x) ≡ op(x, nan) ≡ nan
        end
        # a complex operand makes the undefined result complex, as it does over the floats
        for op in (+, -, *, /, ^, div, fld, cld, mod, rem, min, max), x in complexes
            @test op(nan, x) ≡ op(x, nan) ≡ complex(nan, nan)
        end
        for x in reals
            @test divrem(nan, x) ≡ divrem(x, nan) ≡ (nan, nan)
        end
        for x in complexes
            @test divrem(nan, x) ≡ divrem(x, nan) ≡ (complex(nan, nan), complex(nan, nan))
        end
        @test nan^(1//2) ≡ (1//2)^nan ≡ ℯ^nan ≡ nan
        @test -nan ≡ +nan ≡ abs(nan) ≡ inv(nan) ≡ sign(nan) ≡ conj(nan) ≡ nan
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

    @testset "against the floats" begin
        values = (0, 1, -2, 1.5, -1.5, 0.0, -0.0, NaN, NaN32, Inf, -Inf,
                  prevfloat(Inf), nextfloat(-Inf), nextfloat(0.0))
        for x in values, (inf, flt) in ((∞, Inf), (+∞, Inf), (-∞, -Inf), (ℵ₀, Inf))
            for op in (<, ≤, >, ≥, ==, isless, isequal)
                @test op(x, inf) == op(x, flt)
                @test op(inf, x) == op(flt, x)
            end
            for op in (max, min)
                @test isequal(op(x, inf), op(x, flt)) && isequal(op(inf, x), op(flt, x))
            end
        end
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
