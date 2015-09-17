"""
Domains
"""
module Domains

using Traits
using ..VectorSpaces

import Base: show
import Base: eltype, ndims
import Base: isempty, in, ==, <, issubset, ⊊
import Base: intersect, issubset

export show
export eltype, ndims
export isempty, in, ==, <, issubset, ⊊
export intersect, issubset



export Region
export vspace, boundingbox

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

isempty(r::Region) = any(map(<=, r.max, r.min))
in{V}(p::V, r::Region{V}) = all(map(>=, p, r.min)) && all(map(<, p, r.max))

function =={V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) && isempty(r2) return true end
    if isempty(r1) || isempty(r2) return false end
    all(map(==, r1.min, r2.min)) && all(map(==, r1.max, r2.max))
end
function <{V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) return true end
    if isempty(r2) return false end
    if r1==r2 return false end
    all(map(>=, r1.min, r2.min)) && all(map(<=, r1.max, r2.max))
end
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

# TODO: union, setdiff



export AbstractDomain
export vspace, boundingbox

@traitdef AbstractDomain{Dom} begin
    V = vspace(Dom)
    @constraints begin
        istrait(AbstractVS{V})
    end
    isempty(Dom) -> Bool
    in(V, Dom) -> Bool
    boundingbox(Dom) -> Region{V}
end



# Empty Domain

export EmptyDomain

immutable EmptyDomain{V}
    function EmptyDomain()
        _check_EmptyDomain(V)
        new()
    end
end

@generated function _check_EmptyDomain{V}(::Type{V})
    @assert istrait(AbstractVS{V})
    @assert vdim(V)>=0
    :nothing
end

function show{V}(io::IO, d::EmptyDomain{V})
    show(io, "Domain{$V}[]")
end

vspace{V}(::Type{EmptyDomain{V}}) = V

isempty{V}(d::EmptyDomain{V}) = true
in{V}(p::V, d::EmptyDomain{V}) = false
boundingbox{V}(d::EmptyDomain{V}) = Region{V}()::Region{V}



# Cuboid Domain

export BoxDomain

immutable BoxDomain{V}
    shape::Region{V}
    function BoxDomain(shape)
        _check_BoxDomain(V)
        new(shape)
    end
end

@generated function _check_BoxDomain{V}(::Type{V})
    @assert istrait(AbstractVS{V})
    @assert vdim(V)>=0
    :nothing
end

function show{V}(io::IO, d::BoxDomain{V})
    show(io, "Domain{$V}[$(d.shape)]")
end

vspace{V}(::Type{BoxDomain{V}}) = V

isempty{V}(d::BoxDomain{V}) = isempty(d.shape)::Bool
in{V}(p::V, d::BoxDomain{V}) = (p in d.shape)::Bool
boundingbox{V}(d::BoxDomain{V}) = d.shape::Region{V}

end
