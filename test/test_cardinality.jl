using Infinities, Base64, Base.Checked, Test

@testset "InfiniteCardinal" begin
    @testset "basics" begin
        @test !isone(ℵ₀)
        @test !iszero(ℵ₀)
        @test sign(ℵ₀) ≡ 1 && !signbit(ℵ₀)
        @test angle(ℵ₀) ≡ 0
        @test Integer(∞) ≡ convert(Integer,∞) ≡ Integer(ℵ₀) ≡ convert(Integer, ℵ₀) ≡ ℵ₀
        @test abs(ℵ₀) ≡ ℵ₀
        @test zero(ℵ₀) ≡ zero(InfiniteCardinal{0}) ≡ 0
        @test one(ℵ₀) ≡ one(InfiniteCardinal{0}) ≡ oneunit(ℵ₀) ≡ oneunit(InfiniteCardinal{0}) ≡ 1
        @test isinf(ℵ₀) && !isfinite(ℵ₀)
        @test Integer(RealInfinity()) ≡ Integer(ComplexInfinity()) ≡ ℵ₀
        @test_throws InexactError Integer(-∞)
        @test_throws InexactError Integer(exp(0.1im)*∞)
    end

    @testset "equality" begin
        @test ℵ₀ == ℵ₀
        @test ℵ₀ ≠ ℵ₁
        @test ℵ₀ == ∞ && ∞ == ℵ₀
        @test ℵ₁ ≠ ∞ && ∞ ≠ ℵ₁
        @test ℵ₀ ≠ -∞ && -∞ ≠ ℵ₀
        @test ℵ₀ == Inf && Inf == ℵ₀
        @test ℵ₀ ≠ 5 && 5 ≠ ℵ₀
    end

    @testset "inequalities" begin
        @test !isless(ℵ₀, ℵ₀)
        @test isless(ℵ₀, ℵ₁)
        @test isless(Inf, ℵ₁) && !isless(ℵ₁, Inf)
        @test !isless(Inf, ℵ₀) && !isless(ℵ₀, Inf)
        @test isless(5, ℵ₀) && !isless(ℵ₀, 5)
        @test isless(5, ℵ₁) && !isless(ℵ₁, 5)
        @test !isless(RealInfinity(), ℵ₀)
        @test isless(-RealInfinity(), ℵ₀)
        @test !isless(ℵ₀, RealInfinity())
        @test !isless(ℵ₀, -RealInfinity())
        @test isless(RealInfinity(), ℵ₁)
        @test isless(-RealInfinity(), ℵ₁)
        @test !isless(ℵ₁, RealInfinity())
        @test !isless(ℵ₁, -RealInfinity())
        @test !isless(∞, ℵ₀)
        @test !isless(ℵ₀, ∞)
        @test isless(∞, ℵ₁)
        @test !isless(ℵ₁, ∞)


        @test !(ℵ₀ < ℵ₀) && !(ℵ₀ > ℵ₀)
        @test ℵ₀ ≤ ℵ₀ && ℵ₀ ≥ ℵ₀
        @test ℵ₀ < ℵ₁ && ℵ₀ ≤ ℵ₁
        @test !(ℵ₀ > ℵ₁) && !(ℵ₀ ≥ ℵ₁)
        @test ℵ₁ ≤ ℵ₁ && ℵ₁ ≥ ℵ₁ && !(ℵ₁ > ℵ₁) && !(ℵ₁ < ℵ₁)

        @test !(∞ < ℵ₀) && ∞ ≤ ℵ₀
        @test !(∞ > ℵ₀) && ∞ ≥ ℵ₀
        @test ∞ < ℵ₁ && ∞ ≤ ℵ₁
        @test !(∞ > ℵ₁) && !(∞ ≥ ℵ₁)
        @test ℵ₀ ≤ ∞
        @test !(ℵ₁ < ∞) && !(ℵ₁ ≤ ∞)
        @test !(ℵ₀ > ∞)
        @test ℵ₁ > ∞ && ℵ₁ ≥ ∞

        @test -∞ < ℵ₀ && -∞ ≤ ℵ₀
        @test -∞ < ℵ₁ && -∞ ≤ ℵ₁
        @test !(-∞ > ℵ₀) && RealInfinity() ≥ ℵ₀
        @test !(-∞ > ℵ₁) && !(RealInfinity() ≥ ℵ₁)
        @test !(-∞ > ℵ₀) && !(-∞ ≥ ℵ₀)
        @test ℵ₀ > -∞
        @test ℵ₁ > -∞ && ℵ₁ ≥ -∞
        @test ℵ₀ ≤ RealInfinity()
        @test !(ℵ₀ ≤ -∞)
        @test !(ℵ₁ ≤ RealInfinity())

        @test Inf < ℵ₁ && !(ℵ₁ < Inf)
        @test !(Inf < ℵ₀) && !(ℵ₀ < Inf)
        @test (5 < ℵ₀) && !(ℵ₀ < 5)
        @test 5 ≤ ℵ₁ && !(5 ≥ ℵ₁) && !(ℵ₁ ≤ 5) && ℵ₁ > 5
        @test 5 < ℵ₀ && 5 ≤ ℵ₀
        @test !(ℵ₀ < 5) && !(ℵ₀ ≤ 5)
        @test ℵ₀ > 5 && ℵ₀ ≥ 5
        @test !(5 > ℵ₀) && !(5 ≥ ℵ₀)

        @testset "BigInt/BigFloat" begin
            for x in (big(2), big(2.0))
                @test !(x == ℵ₀)
                @test !(ℵ₀ == x)
                @test x < ℵ₀
                @test !(ℵ₀ < x)
                @test x <= ℵ₀
                @test !(ℵ₀ <= x)
                @test x < InfiniteCardinal{1}()
                @test !(InfiniteCardinal{1}() < x)
                @test x <= InfiniteCardinal{1}()
                @test !(InfiniteCardinal{1}() <= x)
            end
            @test (ℵ₀ == big(Inf)) == (ℵ₀ == Inf)
            @test (big(Inf) == ℵ₀) == (Inf == ℵ₀)
            @test (ℵ₀ < big(Inf)) == (ℵ₀ < Inf)
            @test (big(Inf) < ℵ₀) == (Inf < ℵ₀)
            @test (ℵ₀ <= big(Inf)) == (ℵ₀ <= Inf)
            @test (big(Inf) <= ℵ₀) == (Inf <= ℵ₀)
        end

    end

    @testset "min/max" begin
        @test @inferred(min(ℵ₀,ℵ₁)) ≡ ℵ₀
        @test @inferred(min(ℵ₀,ℵ₁)) ≡ min(ℵ₁,ℵ₀) ≡ ℵ₀
        @test @inferred(max(ℵ₀,ℵ₁)) ≡ max(ℵ₁,ℵ₀) ≡ ℵ₁
        @test min(∞,ℵ₀) ≡ max(ℵ₀,∞) ≡ ∞
        @test max(∞,ℵ₀) ≡ min(ℵ₀,∞) ≡ ℵ₀
        @test min(-∞,ℵ₀) ≡ min(ℵ₀,-∞) ≡ -∞
        @test max(-∞,ℵ₀) ≡ max(ℵ₀,-∞) ≡ ℵ₀
        @test min(5,ℵ₀) ≡ min(ℵ₀,5) ≡ 5
        @test max(5,ℵ₀) ≡ max(ℵ₀,5) ≡ ℵ₀
    end

    @testset "algebra" begin
        @test +(ℵ₀) ≡ ℵ₀
        @test ℵ₀ + ℵ₁ ≡ ℵ₁ + ℵ₀ ≡ ℵ₁
        @test 5 + ℵ₀ ≡ ℵ₀ + 5 ≡ ℵ₀
        @test ℵ₀ - 5 ≡ ℵ₀
        @test 5 - ℵ₀ ≡ -∞
        @test_throws ArgumentError ℵ₀ - ℵ₀
        @test -ℵ₀ ≡ -∞

        @test *(ℵ₀) ≡ ℵ₀
        @test ℵ₀ * ℵ₀ ≡ ℵ₀
        @test ℵ₀ * ∞ ≡ ∞ * ℵ₀ ≡ ℵ₀
        @test 5 * ℵ₀ ≡ ℵ₀ * 5 ≡ ℵ₀
        @test 5.0ℵ₀ ≡ ℵ₀*5.0 ≡ RealInfinity()
        @test_throws ArgumentError (-5) * ℵ₀

        @test ℵ₀ - 5.1 ≡ ∞
        @test ℵ₀ + 5.1 ≡ ∞
        @test 5.1 + ℵ₀ ≡ ∞
        @test 5.1 - ℵ₀ ≡ -∞

        @test ℵ₀ + ∞ == ∞
        @test ∞ + ℵ₀ == ∞
        @test ℵ₀ + RealInfinity() == ∞
        @test ℵ₀ - (-∞) == ∞
        @test  RealInfinity() + ℵ₀ == ∞
    end

    @testset "fld/cld/div/mod" begin
        @test ℵ₀ ÷ 5 ≡ ℵ₀
        @test ℵ₀ ÷ ℵ₀ ≡ NotANumber()
        @test 5 ÷ ℵ₀ ≡ 0
        @test fld(-5, ℵ₀) ≡ -1
        @test cld(5, ℵ₀) ≡ 1
        @test mod(ℵ₀,ℵ₀) ≡ NotANumber()
        @test mod(ℵ₀,6) ≡ NotANumber()
        @test mod(5,ℵ₀) ≡ 5
        @test_throws ArgumentError mod(-1,ℵ₀)
    end

    @test string(ℵ₀) == stringmime("text/plain", ℵ₀) == "ℵ₀"
    @test string(ℵ₁) == stringmime("text/plain", ℵ₁) == "ℵ₁"
    @test Base.to_index(ℵ₀) ≡ Base.to_shape(ℵ₀) ≡ ℵ₀
    @test Base.to_shape((∞,)) ≡ (ℵ₀,)

    @testset "Set" begin
        s = Set([ℵ₀,ℵ₁,∞,1])
        @test 1 ∈ s
        @test ∞ ∈ s
        @test ℵ₀ ∈ s
        @test ℵ₁ ∈ s
        @test 2 ∉ s
    end

    @testset "checked" begin
        @test checked_add(5, ℵ₀) ≡ checked_add(ℵ₀, 5) ≡ ℵ₀
        @test checked_sub(ℵ₀, 5) ≡ ℵ₀
        @test checked_sub(5, ℵ₀) ≡ -∞
        @test checked_mul(-5, ℵ₀) ≡ checked_mul(ℵ₀, -5) ≡ -∞
        @test checked_sub(ℵ₀, ℵ₀) ≡ NotANumber()
        @test checked_add(ℵ₀, ℵ₀) ≡ ℵ₀
        @test checked_mul(ℵ₀, ℵ₀) ≡ ℵ₀
    end

    @testset "indexing" begin
        @test_throws BoundsError randn(3)[ℵ₀]
        @test_throws ErrorException Base._unsafe_getindex(IndexCartesian(),permutedims(1:3)',ℵ₀)
        @test_throws BoundsError view(randn(3),1:2)[ℵ₀]
        # check ambiguity is not introduced
        @test view(collect(1:5),2)[] == 2
    end
end
