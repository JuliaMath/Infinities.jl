using Infinities, Base64, Base.Checked, Test
import Infinities: Infinity

@testset "∞" begin
    @testset "∞" begin
        @test ∞ ≠ 1
        @test 1 ≠ ∞
        @test ∞ == ∞
        @test ∞ == Inf
        @test Inf == ∞

        @test +∞ ≡ ∞

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

            @test 5 < ∞ && 5 ≤ ∞
            @test !(∞ < 5) && !(∞ ≤ 5)
            @test ∞ > 5 && ∞ ≥ 5
            @test !(5 > ∞) && !(5 ≥ ∞)
        end

        @test ∞ + ∞ ≡ ∞
        @test ∞ + 1 ≡ 1 + ∞ ≡ ∞ + 1.0 ≡ 1.0 + ∞ ≡ ∞
        @test ∞ - 1 ≡ ∞ - 1.0 ≡ ∞
        @test *(∞) ≡ ∞
        @test ∞*∞ ≡ ∞
        @test ∞ - ∞ isa NotANumber

        @test one(∞) ≡ 1
        @test zero(∞) ≡ 0

        @test !isone(∞)
        @test !iszero(∞)

        @test sign(∞) ≡ 1
        @test angle(∞) ≡ 0

        @test string(∞) == stringmime("text/plain", ∞) == "∞"

        @test isinf(∞)
        @test !isfinite(∞)

        @test Base.to_index(∞) ≡ ∞

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
        end

        @testset "checked" begin
            @test checked_add(5, ∞) ≡ checked_add(∞, 5) ≡ ∞
            @test checked_sub(∞, 5) ≡ ∞
        end
    end

    @testset "RealInfinity" begin
        @test RealInfinity(∞) ≡ convert(RealInfinity, ∞) ≡ RealInfinity() ≡
                RealInfinity(false) ≡ RealInfinity(RealInfinity())

        @test promote_type(Infinity, RealInfinity) == RealInfinity
        @test promote(∞, RealInfinity()) ≡ (RealInfinity(),RealInfinity())

        @test -∞ ≡ RealInfinity(true)
        @test +∞ ≡ ∞

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

        @test -∞ ≤ ∞
        @test RealInfinity() ≤ ∞
        @test ∞ ≤ RealInfinity()
        @test -∞ ≤ -∞
        @test !(∞ ≤ -∞)
        @test -∞ < ∞
        @test !(-∞ < -∞)
        @test !(RealInfinity() < ∞) && !(∞ < RealInfinity())
        @test RealInfinity() ≥ ∞ && ∞ ≥ RealInfinity()
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

        @test ∞ - (-∞) ≡ ∞
        @test (-∞) - ∞ ≡ -∞
        @test (1∞) - (-∞) ≡ 1∞
        @test (-∞) - (1∞) ≡ -∞

        @test_throws ArgumentError ∞ - (1∞)
        @test_throws ArgumentError (1∞) - ∞
        @test_throws ArgumentError (1∞) - (1∞)
        @test_throws ArgumentError (-∞) - (-∞)
        @test_throws ArgumentError 0*∞
        @test_throws ArgumentError 0*(-∞)

        @test (-∞)*2 ≡ 2*(-∞) ≡ -2 * ∞ ≡ ∞ * (-2) ≡ (-2) * RealInfinity() ≡ -∞
        @test (-∞)*2.3 ≡ 2.3*(-∞) ≡ -2.3 * ∞ ≡ ∞ * (-2.3) ≡ (-2.3) * RealInfinity() ≡ -∞

        @test isinf(-∞)
        @test !isfinite(-∞)

        @test [∞, -∞] isa Vector{RealInfinity}

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

        @testset "Checked" begin
            @test checked_sub(5, ∞) ≡ checked_sub(5, RealInfinity()) ≡ -∞
            @test checked_sub(-∞, 5) ≡ -∞
            @test checked_add(5, -∞) ≡ checked_add(-∞, 5) ≡ -∞
            @test checked_mul(-5, ∞) ≡ checked_mul(∞, -5) ≡ -∞
            @test checked_mul(-5, -∞) ≡ checked_mul(-∞, -5) ≡ RealInfinity()
        end
    end

    @testset "ComplexInfinity" begin
        @test ComplexInfinity(∞) ≡ convert(ComplexInfinity, ∞) ≡ ComplexInfinity() ≡
            ComplexInfinity(false) ≡ ComplexInfinity{Bool}(∞) ≡ ComplexInfinity{Bool}(RealInfinity())

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

        @test Inf == ComplexInfinity()
        @test ComplexInfinity() == Inf

        @test isless(-ComplexInfinity(), ComplexInfinity())
        @test isless(5, ComplexInfinity())
        @test !isless(ComplexInfinity(), 5)

        @test 5 < ComplexInfinity() && 5 ≤ ComplexInfinity()
        @test !(ComplexInfinity() < 5) && !(ComplexInfinity() ≤ 5)
        @test 5 > -ComplexInfinity() && 5 ≥ -ComplexInfinity()
        @test ComplexInfinity() > 5 && ComplexInfinity() ≥  5

        @test 1 + ComplexInfinity() ≡ 1.0 + ComplexInfinity() ≡ ComplexInfinity() + 1 ≡ ComplexInfinity() + 1.0 ≡ ComplexInfinity()
        @test 5 * ComplexInfinity() ≡ ComplexInfinity()
        @test (-5) * ComplexInfinity() ≡ -ComplexInfinity()

        @test ComplexInfinity(0.25) * ComplexInfinity(0.5) ≡ ComplexInfinity(0.75)

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
    end

    @testset "Set" begin
        s = Set([∞,1])
        @test 1 in s
        @test ∞ in s
        @test 2 ∉ s
    end
end


include("test_cardinality.jl")