using Infinities, Base64, Test

@testset "InfiniteCardinal" begin
    @test !isone(ℵ₀)
    @test !iszero(ℵ₀)

    @test ℵ₀ ≠ 5 && 5 ≠ ℵ₀
    @test ℵ₀ * ℵ₀ ≡ ℵ₀
    @test ℵ₀ * ∞ ≡ ∞ * ℵ₀ ≡ ℵ₀
    @test 5 * ℵ₀ ≡ ℵ₀ * 5 ≡ ℵ₀
    @test_throws ArgumentError (-5) * ℵ₀

    @test abs(ℵ₀) ≡ ℵ₀
    @test zero(ℵ₀) ≡ 0
    @test one(ℵ₀) ≡ 1

    @test 5 < ℵ₀ && 5 ≤ ℵ₀
    @test !(ℵ₀ < 5) && !(ℵ₀ ≤ 5)
    @test ℵ₀ > 5 && ℵ₀ ≥ 5
    @test !(5 > ℵ₀) && !(5 ≥ ℵ₀)

    @test string(ℵ₀) == stringmime("text/plain", ℵ₀) == "ℵ₀"
    @test string(ℵ₁) == stringmime("text/plain", ℵ₁) == "ℵ₁"
end