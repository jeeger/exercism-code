# submit
function count_nucleotides(strand::AbstractString)
    count = Dict('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0)
    try
        for c in strand
            count[c] = count[c] + 1
        end
        return count
    catch e
        if e isa KeyError
            throw(DomainError(e.key, "Invalid base."))
        else
            throw(e)
        end
    end
end
