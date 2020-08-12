using Infinities, Test

@testset "∞" begin
    @testset "∞" begin
        @test ∞ ≠ 1
        @test ∞ == ∞
        @test ∞ == Inf

        @test +∞ ≡ ∞

        @test isless(1, ∞)
        @test !isless(Inf, ∞)
        @test !isless(∞, Inf)
        @test !isless(∞, 1)

        @test !isless(∞, ∞)
        @test !(∞ < ∞)
        @test ∞ ≤ ∞
        @test !(∞ > ∞)
        @test ∞ ≥ ∞

        @test ∞ + ∞ ≡ ∞
        @test ∞ + 1 ≡ ∞
        @test *(∞) ≡ ∞
        @test ∞*∞ ≡ ∞
        @test ∞ - ∞ isa NotANumber

        @test one(∞) === 1
        @test zero(∞) === 0

        @test !isone(∞)
        @test !iszero(∞)

        @test max(1,∞) == max(∞,1) == ∞
        @test min(1,∞) == min(∞,1) == 1
        @test maximum([1,∞]) == ∞
        @test minimum([1,∞]) == 1

        @test string(∞) == "∞"

        @test Base.OneTo(∞) == OneToInf()

        @test isinf(∞)
        @test !isfinite(∞)

        @test div(∞, 2) == ∞
        @test fld(∞, 2) == ∞
        @test cld(∞, 2) == ∞
        @test div(2, ∞) == 0
        @test fld(2, ∞) == 0
        @test cld(2, ∞) == 1
        @test div(-2, ∞) == 0
        @test fld(-2, ∞) == -1
        @test cld(-2, ∞) == 0
        @test mod(2,∞) == 2
        @test div(∞,∞) isa NotANumber
        @test fld(∞,∞) isa NotANumber
        @test cld(∞,∞) isa NotANumber
        @test mod(∞,∞) isa NotANumber
        @test mod(∞,2) isa NotANumber
        @test_throws ArgumentError mod(-2,∞)

        @test min(∞, ∞) == ∞
        @test max(∞, ∞) == ∞
        @test min(3,∞) == 3
        @test max(3,∞) == ∞
    end

    @testset "RealInfinity" begin
        @test RealInfinity(∞) ≡ convert(RealInfinity, ∞) ≡ RealInfinity() ≡ 
                RealInfinity(false) ≡ RealInfinity(RealInfinity())

        @test -∞ ≡ RealInfinity(true)
        @test +∞ ≡ ∞

        @test sign(-∞) == -1
        @test angle(-∞) ≈ π

        @test ∞ == +∞ == RealInfinity(∞)
        @test ∞ ≠  -∞
        @test 1-∞ == -∞
        @test 1-(-∞) ≡ RealInfinity()
        @test (-∞) - 5 ≡ -∞

        @test (-∞)*(-∞) ≡ ∞*RealInfinity(∞) ≡ RealInfinity(∞)*∞

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
        @test !(RealInfinity() < ∞)
        @test RealInfinity() ≥ ∞
        @test ∞ ≥ RealInfinity()
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

        @test Base.OneTo(1*∞) == OneToInf()
        @test_throws ArgumentError Base.OneTo(-∞)

        @test isinf(-∞)
        @test !isfinite(-∞)

        @test [∞, -∞] isa Vector{RealInfinity}

        @test mod(-∞, 5) isa NotANumber
        @test mod(-∞, -∞) isa NotANumber
        @test mod(5, RealInfinity()) == 5
        @test_throws ArgumentError mod(5,-∞)

        @test min(-∞, ∞) ≡ min(∞, -∞) ≡ min(-∞, RealInfinity()) ≡ -∞
        @test max(-∞, RealInfinity()) ≡ RealInfinity()
        @test max(∞, -∞) ≡ max(-∞,∞) ≡ ∞
    end

    @testset "ComplexInfinity" begin
        @test ComplexInfinity(∞) ≡ convert(ComplexInfinity, ∞) ≡ ComplexInfinity() ≡
            ComplexInfinity(false)

        @test ComplexInfinity(∞) == ∞
        @test ∞ == ComplexInfinity(∞)
        @test ComplexInfinity(∞) == RealInfinity()
        @test RealInfinity() == ComplexInfinity(∞)
        @test ComplexInfinity(-∞) == -∞
        @test  -∞ == ComplexInfinity(-∞)

        @test ComplexInfinity() + ∞ ≡ ComplexInfinity() + RealInfinity() ≡ 
                ∞ + ComplexInfinity() ≡ RealInfinity() + ComplexInfinity() ≡ ComplexInfinity()
        @test ComplexInfinity(true) + ComplexInfinity(true) == ComplexInfinity(true)
        @test ComplexInfinity(false) + ComplexInfinity(false) == ComplexInfinity(false)
        @test ComplexInfinity(true)+1 == ComplexInfinity(true)
        @test ComplexInfinity(false)+1 == ComplexInfinity(false)

        @test ∞ * ComplexInfinity() ≡ RealInfinity() * ComplexInfinity() ≡ 
             ComplexInfinity() * ∞ ≡ ComplexInfinity() * RealInfinity() ≡ ComplexInfinity()

        @test  2.0im*∞ ≡ ∞*2.0im ≡ 2.0im * RealInfinity() ≡ RealInfinity() * 2.0im ≡ ComplexInfinity(1/2)

        @test exp(im*π/4)*∞ == Inf+im*Inf
        @test exp(im*π/4)+∞ == ∞
    end
end
