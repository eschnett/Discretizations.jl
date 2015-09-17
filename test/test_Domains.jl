using Traits



function test_EmptyDomain{V}(::Type{V})
    @test istrait(AbstractVS{V})
    Dom = EmptyDomain{V}
    @test istrait(AbstractDomain{Dom})

    D = vdim(V)
    p0 = vnull(V)

    dom = Dom()
    @test vspace(Dom) === V
    @test isempty(dom)
    @test p0 ∉ dom
    @test isempty(boundingbox(dom)) == (D>0)
end
function test_EmptyDomains()
    test_EmptyDomain(EmptyVS{Int8})
    test_EmptyDomain(ScalarVS{Int})
    test_EmptyDomain(ProductVS{BigFloat,BigFloat})
    test_EmptyDomain(MultiProductVS{NTuple{4,Float64}})
end
test_EmptyDomains()

function test_BoxDomain{V}(::Type{V})
    @test istrait(AbstractVS{V})
    Reg = Region{V}
    Dom = BoxDomain{V}
    @test istrait(AbstractDomain{Dom})

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
    reg2 = Reg(p0, p2)

    dom0 = Dom(reg0)
    @test vspace(Dom) === V
    @test isempty(dom0) == isempty(reg0)
    @test (p0 ∉ dom0) == (D>0)
    @test boundingbox(dom0) == reg0

    dom1 = Dom(reg1)
    dom2 = Dom(reg2)
    @test !isempty(dom1)
    @test !isempty(dom2)
    @test p0 ∈ dom1
    @test p0 ∈ dom2
    @test p1 ∈ dom2
    @test boundingbox(dom1) == reg1
    @test boundingbox(dom2) == reg2
end
function test_BoxDomains()
    test_BoxDomain(EmptyVS{Int8})
    test_BoxDomain(ScalarVS{Int})
    test_BoxDomain(ProductVS{BigFloat,BigFloat})
    test_BoxDomain(MultiProductVS{NTuple{4,Float64}})
end
test_BoxDomains()
