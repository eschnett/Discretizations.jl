"""
Regions
"""
module Regions

using Traits
using ..VectorSpaces

import Base: show
import Base: isempty, in, ==, <=, <, issubset, ⊊
import Base: intersect, issubset
import Base: union, setdiff, symdiff

export show
export isempty, in, ==, <=, <, issubset, ⊊
export intersect, issubset
export union, setdiff, symdiff



export Region
export vspace, isdisjoint, boundingbox

# TODO: Use Range?
immutable Region{V}
    min::V
    max::V
    function Region(min, max)
        _check_Region(V)
        @assert vdim(min) == vdim(max) == vdim(V)
        new(min, max)
    end
    Region() = Region{V}(vnull(V), vnull(V))
end

@generated function _check_Region{V}(::Type{V})
    @assert istrait(AbstractVS{V})
    @assert vdim(V)>=0
    S = veltype(V)
    @assert sconst(S,0) < sconst(S,1) # must support comparison
    :nothing
end

function show{V}(io::IO, r::Region{V})
    show(io, "Region{$V}($(r.min):$(r.max))")
end

vspace{V}(::Type{Region{V}}) = V

isempty(r::Region) = any(map(>=, r.min, r.max))
in{V}(p::V, r::Region{V}) = all(map(>=, p, r.min)) && all(map(<, p, r.max))

function =={V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) && isempty(r2) return true end
    if isempty(r1) || isempty(r2) return false end
    all(map(==, r1.min, r2.min)) && all(map(==, r1.max, r2.max))
end
function <={V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) return true end
    if isempty(r2) return false end
    all(map(>=, r1.min, r2.min)) && all(map(<=, r1.max, r2.max))
end
function isdisjoint{V}(r1::Region{V}, r2::Region{V})
    any(map(>=, map(max, r1.min, r2.min), map(min, r1.max, r2.max)))
end
<{V}(r1::Region{V}, r2::Region{V}) = r1 <= r2 && r1 != r2
issubset{V}(r1::Region{V}, r2::Region{V}) = r1 <= r2
⊊{V}(r1::Region{V}, r2::Region{V}) = r1 < r2

# TODO: isless, hash

intersect(r::Region) = r
function intersect{V}(r1::Region{V}, r2::Region{V})
    # if isempty(r1) || isempty(r2) return false end
    Region{V}(map(max, r1.min, r2.min), map(min, r1.max, r2.max))
end

function boundingbox{V}(r1::Region{V}, r2::Region{V})
    # if isempty(r1) && isempty(r2) return Region{V}() end
    if isempty(r1) return r2 end
    if isempty(r2) return r1 end
    Region{V}(map(min, r1.min, r2.min), map(max, r1.max, r2.max))
end



export RegionSet

immutable RegionSet{V}
    regs::Vector{Region{V}}
    function RegionSet(regs)
        _check_RegionSet(V)
        @assert invariant(rs)
        rs
    end
end

@generated function _check_RegionSet{V}(::Type{V})
    @assert istrait(AbstractVS{V})
    @assert vdim(V)>=0
    S = veltype(V)
    @assert sconst(S,0) < sconst(S,1) # must support comparison
    :nothing
end

RegionSet{V}(::Type{V}) = RegionSet{V}(Region{V}[])

"Create a RegionSet, filtering out empty regions"
function _make_regionset{V}(rs::Vector{Region{V}})
    RegionSet{V}(filter(r->!isempty(r), rs))
end
RegionSet{V}(r::Region{V}) = _make_regionset([r])

function invariant{V}(rs::RegionSet{V})
    # regions may not be emtpy
    if any(isempty, rs.regs) return false end
    # regions may not overlap
    for i in eachindex(rs.regs), j in 1:i-1
        if !isempty(rs.regs[i] ∩ rs.regs[j]) return false end
    end
    true
end

vspace{V}(::Type{RegionSet{V}}) = V

isempty(rs::RegionSet) = isempty(rs.regs)
in{V}(p::V, rs::RegionSet{V}) = any(r -> p in r, rs.regs)

# (Region, RegionSet) -> Region

function intersect{V}(r1::Region{V}, rs2::RegionSet{V})
    r = r1
    for r2 in rs2.regs
        r = r ∩ r2
    end
    r
end
intersect{V}(rs1::RegionSet{V}, r2::Region{V}) = intersect(r2, rs1)
function boundingbox{V}(rs::RegionSet{V})
    r = Region{V}()
    for r2 in rs.regs
        r = boundingbox(r, r2)
    end
    r
end

# (Region, Region) -> RegionSet

function _split{V}(p::V, r::Region{V})
    if p ∉ r return RegionSet{V}(r) end
    D = vdim(V)
    rs = Region{V}[]
    for i in 1:1<<D
        rmin, rmax = r.min, r.max
        faces = Bool[(i-1) & (1<<(d-1)) != 0 for d in 1:D]
        rmin = map((rmin,p,f) -> !f ? rmin : p, r.min, p, faces)
        rmax = map((p,rmax,f) -> !f ? p : rmax, p, r.max, faces)
        push!(rs, Region{V}(rmin, rmax))
    end
    _make_regionset(rs)
end

function setdiff{V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) || isempty(r2) return r1 end
    rs = _split(r2.min, r1).regs
    rs = vcat(Region{V}[_split(r2.max, r).regs for r in rs]...)
    rs = filter(r -> r != r2, rs)
    _make_regionset(rs)
end
function union{V}(r1::Region{V}, r2::Region{V})
    rs = setdiff(r1, r2).regs
    push!(rs, r2)
    _make_regionset(rs)
end
function symdiff{V}(r1::Region{V}, r2::Region{V})
    _make_regionset(vcat(setdiff(r1, r2).regs, setdiff(r2, r1).regs))
end

# (RegionSet, RegionSet) -> RegionSet

function setdiff{V}(rs1::RegionSet{V}, r2::Region{V})
    _make_regionset(map(r1 -> setdiff(r1,r2), rs1.regs))
end
function setdiff{V}(rs1::RegionSet{V}, rs2::RegionSet{V})
    rs = rs1
    for r2 in rs2.regs
        rs = setdiff(rs, r2)
    end
    rs
end
setdiff{V}(r1::Region{V}, rs2::RegionSet{V}) = setdiff(RegionSet{V}(r1), r2)

function symdiff{V}(r1::Region{V}, rs2::RegionSet{V})
    _make_regionset(vcat(setdiff(r1, rs2).regs, setdiff(rs2, r1).regs))
end
function symdiff{V}(rs1::RegionSet{V}, r2::Region{V})
    _make_regionset(vcat(setdiff(r1s, r2).regs, setdiff(r2, rs1).regs))
end
function symdiff{V}(rs1::RegionSet{V}, rs2::RegionSet{V})
    _make_regionset(vcat(setdiff(r1s, rs2).regs, setdiff(rs2, rs1).regs))
end

function union{V}(rs1::RegionSet{V}, r2::Region{V})
    rs = map(r1 -> setdiff(r1, r2), rs1.regs)
    push!(rs, r2)
    _make_regionset(rs)
end
union{V}(r1::Region{V}, rs2::RegionSet{V}) = union(rs2, r1)
function union{V}(rs1::RegionSet{V}, rs2::RegionSet{V})
    rs = rs1
    for r2 in rs2
        rs = rs ∪ r2
    end
    rs
end
∩

<={V}(rs1::RegionSet{V}, rs2::RegionSet{V}) = isempty(setdiff(rs2, rs1))
=={V}(rs1::RegionSet{V}, rs2::RegionSet{V}) = rs1 <= rs2 && rs2 <= rs1
<{V}(rs1::RegionSet{V}, rs2::RegionSet{V}) = rs1 <= rs2 && rs1 != rs2
issubset{V}(rs1::RegionSet{V}, rs2::RegionSet{V}) = rs1 <= rs2
⊊{V}(rs1::RegionSet{V}, rs2::RegionSet{V}) = rs1 < rs2

# TODO: isless, hash

end
