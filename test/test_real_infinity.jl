using Infinities, Test

struct SignedInfinity <: RealInfinity
    negative::Bool
end
Base.signbit(inf::SignedInfinity) = inf.negative

struct FixedInfinity{Negative} <: RealInfinity end
Base.signbit(::FixedInfinity{Negative}) where {Negative} = Negative

struct MissingSignInfinity <: RealInfinity end

@testset "RealInfinity interface" begin
    for negative in (false, true), custom in (SignedInfinity(negative), FixedInfinity{negative}())
        canonical = RealInfinity(negative)
        for operation in (+, -, signbit, sign, angle, abs, abs2, inv, float,
                          Float16, Float32, Float64, BigFloat, ComplexInfinity,
                          isinf, isfinite, isnan, iszero, isone, isinteger, isreal,
                          real, imag, conj, zero, one, oneunit, round, floor, ceil, trunc, repr)
            @test isequal(operation(custom), operation(canonical))
        end
        for InfinityType in (typeof(custom), typeof(canonical), RealInfinity)
            @test zero(InfinityType) === 0.0
            @test one(InfinityType) === oneunit(InfinityType) === 1.0
            @test float(InfinityType) === Float64
        end
        @test RealInfinity(custom) === custom
        for InfinityType in (typeof(custom), RealInfinity, Real)
            @test convert(InfinityType, custom) === custom
        end
        for seed in (UInt(0), UInt(123))
            @test hash(custom, seed) == hash(canonical, seed)
        end
        @test length(Set((custom, canonical, Float64(canonical)))) == 1
        @test Dict(canonical => :found)[custom] === :found

        for scalar in (-2, -0.0, 0, 2, 2.0, big(2.0), 2//1, Inf, -Inf, NaN,
                       +∞, -∞, SignedInfinity(!negative), FixedInfinity{!negative}(), NotANumber()),
            operation in (+, -, *, /, ==, isequal, isless, <, <=, min, max, copysign, flipsign,
                          div, fld, cld, rem, divrem)
            @test isequal(operation(custom, scalar), operation(canonical, scalar))
            @test isequal(operation(scalar, custom), operation(scalar, canonical))
        end
        for scalar in (-2, -0.0, 0, 2, NaN)
            @test isequal(mod(custom, scalar), mod(canonical, scalar))
            if !isnan(scalar) && signbit(scalar) != negative
                @test_throws ArgumentError mod(scalar, custom)
            else
                @test isequal(mod(scalar, custom), mod(scalar, canonical))
            end
        end
        for exponent in (-3, -2, 0, 2, 3, 2.0, 2//1, big(2.0), Inf, -Inf, NaN, NotANumber())
            if negative && isinf(exponent)
                @test_throws DomainError custom^exponent
            else
                @test isequal(custom^exponent, canonical^exponent)
            end
        end
        for exponent in (0.5, 1//2)
            if negative
                @test_throws DomainError custom^exponent
            else
                @test custom^exponent === canonical^exponent
            end
        end
        for exponent in (-2, -1, 0, 1, 2, 3)
            @test isequal(Base.literal_pow(^, custom, Val(exponent)),
                          Base.literal_pow(^, canonical, Val(exponent)))
        end
        for scalar in (1+im, im*∞), operation in (+, -, *, /, ==, isequal)
            @test isequal(operation(custom, scalar), operation(canonical, scalar))
            @test isequal(operation(scalar, custom), operation(scalar, canonical))
        end
    end
    for operation in (signbit, Float64, hash, repr, inf -> inf < 0)
        @test_throws ArgumentError operation(MissingSignInfinity())
    end
    @test_throws "MissingSignInfinity must implement Base.signbit" signbit(MissingSignInfinity())
end