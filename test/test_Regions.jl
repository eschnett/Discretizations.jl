using Traits



function test_Region{V}(::Type{V})
    @test istrait(AbstractVS{V})
    Reg = Region{V}
    @test vspace(Reg) === V

    S = veltype(V)
    s1 = sconst(S, 1)
    s2 = sconst(S, 2)
    D = vdim(V)
    @test D>=0
    p0 = vnull(V)
    p1 = map(x->x+s1, p0)::V
    p2 = map(x->x+s2, p0)::V

    reg0 = Reg()
    @test isempty(reg0) == (D>0)
    @test (p0 ∉ reg0) == (D>0)
    @test (p1 ∉ reg0) == (D>0)
    @test (p2 ∉ reg0) == (D>0)

    reg1 = Reg(p0, p1)
    reg2 = Reg(p0, p2)
    @test !isempty(reg1)
    @test !isempty(reg2)
    @test p0 ∈ reg1
    @test p0 ∈ reg2
    @test p1 ∈ reg2
    @test (reg1 ≠ reg2) == (D>0)
    @test (reg1 ⊊ reg2) == (D>0)
    @test reg1 ⊆ reg2
    @test reg1 ∩ reg2 == reg1
    @test boundingbox(reg1, reg2) == reg2
end
function test_Regions()
    test_Region(EmptyVS{Int8})
    test_Region(ScalarVS{Int})
    test_Region(ProductVS{BigFloat,BigFloat})
    test_Region(MultiProductVS{NTuple{4,Float64}})
end
test_Regions()
