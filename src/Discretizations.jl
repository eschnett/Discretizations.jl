__precompile__()

"""
Tools for discretizations
"""
module Discretizations

using Reexport

include("VectorSpaces.jl")
include("Regions.jl")
include("Domains.jl")

@reexport using .VectorSpaces
@reexport using .Regions
@reexport using .Domains

end
