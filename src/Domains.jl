# """
# Domains
# """
# module Domains

using Traits

import Base: eltype, ndims
import Base: isempty, in, intersect, issubset

export eltype, ndims
export isempty, in, intersect, issubset



export Region
export boundingbox

immutable Region{V}
    min::V
    max::V
    function Region(min, max)
        _check_Region(V)
        @assert vdim(min) == vdim(max) == vdim(V)
        new(min, max)
    end
    Region() = Region{T,D}(vnull(V), vnull(V))
end

@generated function _check_Region{V}(::Type{V})
    @assert istrait(AbstractVector{V})
    vdim(V)
    :nothing
end

eltype{V}(::Type{Region{V}}) = eltype(V)
ndims{V}(::Type{Region{V}}) = length(V)

isempty(r::Region) = any(r.max .<= r.min)
in{V}(p::V, r::Region{V}) = all(p .>= r.min) && all(p .< r.max)

intersect(r::Region) = r
function intersect{V}(r1::Region{V}, r2::Region{V})
    # if isempty(r1) || isempty(r2) return false end
    Region{V}(map(max, r1.min, r2.min), map(min, r1.max, r2.max))
end
function issubset{V}(r1::Region{V}, r2::Region{V})
    if isempty(r1) return true end
    # if isempty(r2) return false end
    all(r1.min .>= r2.min) && all(r1.max .<= r2.max)
end
function boundingbox{V}(r1::Region{V}, r2::Region{V})
    # if isempty(r1) && isempty(r2) return Region{V}() end
    if isempty(r2) return r1 end
    if isempty(r1) return r2 end
    Region{V}(map(max, r1.min, r2.min), map(min, r1.max, r2.max))
end

# union
# setdiff



# export Domain
# 
# @traitdef Domain{Dom} begin
#     T = eltype(Dom)
#     D = ndims(Dom)
#     eltype(Type{Dom}) -> Type
#     ndims(Type{Dom}) -> Int
#     isempty(Dom) -> Bool
#     in(Point{T,D}, Dom) -> Bool
#     boundingbox(Dom) -> Region{T,D}
# end
# 
# 
# 
# # Empty Domain
# 
# export EmptyDomain
# 
# immutable EmptyDomain{T,D} end
# 
# eltype{T,D}(::Type{EmptyDomain{T,D}}) = T
# ndims{T,D}(::Type{EmptyDomain{T,D}}) = D
# 
# isempty{T,D}(d::EmptyDomain{T,D}) = true
# in{T,D}(p::Point{T,D}, d::EmptyDomain{T,D}) = false
# boundingbox{T,D}(d::EmptyDomain{T,D}) = Region{T,D}()
# 
# 
# 
# # Cuboid Domain
# 
# export BoxDomain
# 
# immutable BoxDomain{T,D}
#     shape::Region{T,D}
# end
# 
# eltype{T,D}(::Type{BoxDomain{T,D}}) = T
# ndims{T,D}(::Type{BoxDomain{T,D}}) = D
# 
# isempty{T,D}(d::BoxDomain{T,D}) = isempty(d.shape) #TODO ::Bool
# in{T,D}(p::Point{T,D}, d::BoxDomain{T,D}) = p in d.shape
# boundingbox{T,D}(d::BoxDomain{T,D}) = d.shape



# end
