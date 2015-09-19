using Traits



function test_Region{V}(::Type{V})
    @test istrait(AbstractVS{V})
    Reg = Region{V}
    @test vspace(Reg) === V
    @test eltype(Reg) === V

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
    test_Region(ProductVS{Rational{Int}, Rational{Int}})
    test_Region(MultiProductVS{NTuple{4,Float64}})
end
test_Regions()



function test_RegionSet{V}(::Type{V})
    @test istrait(AbstractVS{V})
    Reg = Region{V}
    Regs = RegionSet{V}
    @test vspace(Regs) === V
    @test eltype(Regs) === V

    S = veltype(V)
    s1 = sconst(S, 1)
    s2 = sconst(S, 2)
    D = vdim(V)
    @test D>=0
    p0 = vnull(V)
    p1 = map(x->x+s1, p0)::V
    p2 = map(x->x+s2, p0)::V

    reg0 = Reg(p0, p0)
    reg1 = Reg(p0, p1)
    reg2 = Reg(p1, p2)
    reg3 = Reg(p0, p2)

    regs0 = Regs(reg0)
    regs1 = Regs(reg1)
    regs2 = Regs(reg2)
    regs3 = Regs(reg3)
    regs01 = regs0 ∪ regs1
    regs12 = regs1 ∪ regs2
    regs0123 = regs0 ∪ regs1 ∪ regs2 ∪ regs3

    @test isempty(Regs())
    @test isempty(regs0) == (D>0)
    @test !isempty(regs1)
    @test (p0 ∉ regs0) == (D>0)
    @test p0 ∈ regs1
    @test (p1 ∉ regs1) == (D>0)

    @test regs12 ∩ regs1 == regs1
    @test regs3 ∩ regs1 == regs1
    @test regs3 ∩ regs12 == regs12
    @test isempty(regs3 ∩ regs0) == (D>0)
    @test regs1 ∩ regs2 == regs0

    @test (setdiff(regs1, regs0) == regs1) == (D>0)
    @test (setdiff(regs12, regs1) == regs2) == (D>0)
    @test (setdiff(regs12, regs2) == regs1) == (D>0)
    @test isempty(setdiff(regs12, regs3))
    @test !isempty(setdiff(regs3, regs12)) == (D>1)

    @test symdiff(regs1, regs12) == (D>0 ? regs2 : Regs())
    @test symdiff(regs1, regs0) == (D>0 ? regs1 : Regs())
    @test symdiff(regs3, regs12) == setdiff(regs3, regs12)
    @test symdiff(regs1, regs2) == (D>0 ? regs12 : Regs())

    @test (regs0 == Regs()) == (D>0)
    @test regs01 == regs1
    @test regs12 == regs12
    @test regs0123 == regs3
    @test (regs0 != regs1) == (D>0)
    @test (regs1 != regs12) == (D>0)
    @test (regs2 != regs3) == (D>0)
    @test Regs([reg1]) == regs1
    if D>0
        @test Regs([reg1, reg2]) == regs12
    end

    @test regs0 <= regs1
    @test regs1 <= regs12
    @test regs12 <= regs3
    @test (!(regs1 <= regs2)) == (D>0)
    @test (!(regs3 <= regs2)) == (D>0)

    @test isdisjoint(regs0, regs0) == (D>0)
    @test isdisjoint(regs1, regs2) == (D>0)
    @test !isdisjoint(regs1, regs12)
    @test !isdisjoint(regs2, regs3)

    @test (regs0 < regs1) == (D>0)
    @test (regs1 < regs12) == (D>0)
    @test !(regs1 < regs1)
    @test !(regs0123 < regs3)
end
function test_RegionSets()
    test_RegionSet(EmptyVS{Int8})
    test_RegionSet(ScalarVS{Int})
    test_RegionSet(ProductVS{Rational{Int}, Rational{Int}})
    test_RegionSet(MultiProductVS{NTuple{4,Float64}})
end
test_RegionSets()
