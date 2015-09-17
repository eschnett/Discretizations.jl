"""
Domains
"""
module Domains

using Traits
using ..VectorSpaces
using ..Regions

import Base: show
import Base: isempty, in
import ..Regions: vspace, boundingbox

export show
export isempty, in
export vspace, boundingbox



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



# TODO: Product domains (multi-block systems)
# TODO: Power domains (grids)
# TODO: Sum domains (either/or systems)
# TODO: AMR domains

end
