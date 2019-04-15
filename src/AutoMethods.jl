module AutoMethods
export @auto


macro auto(e)
    definitions = auto(e)
    esc(Expr(:block, definitions...))
end

function auto(e::Expr)
    e.head in [:(=), :function] && e.args[1] isa Expr ||
        throw(ArgumentError("expected function definition"))

    wherevars = []
    while e.args[1].head == :where
        append!(wherevars, e.args[1].args[2:end])
        e.args[1] = e.args[1].args[1] # delete where
    end
    e.args[1] isa Expr && e.args[1].head == :call ||
        throw(ArgumentError("expected function definition"))

    fname = e.args[1].args[1]
    fargs = @view e.args[1].args[2:end] # this will be mutated
    defaults = [] # mapping: arg indice => default value
    argnames = []
    for i = eachindex(fargs)
        if fargs[i] isa Expr && fargs[i].head == :(:=)
            push!(defaults, i => fargs[i].args[2])
            fargs[i] = fargs[i].args[1] # remove `:= default` part
        end
        push!(argnames, getargname(fargs[i]))
    end
    linefile = e.args[2].args[1]
    @assert linefile isa LineNumberNode
    pushfirst!(gendefinitions(fname, fargs, defaults, argnames, linefile, wherevars),
               wherewrap!(e, wherevars))
end

function gendefinitions(fname, fargs, defaults, argnames, linefile, wherevars)
    length(defaults) > 31 &&
        throw(ArgumentError(
            "number of defaults ($(length(defs))) exceeded maximum (31)"))

    definitions = []
    for n = 1:(2^length(defaults)-1)
        sig = copy(fargs)
        an = copy(argnames)
        for b = 31:-1:1
            if n & (1 << (b-1)) != 0
                idx, defval = defaults[b]
                deleteat!(sig, idx)
                # handle cases with references to previous args,
                # like f([x=1], y=typeof(x))
                replacedefault!(view(sig, idx:length(sig)), # not idx+1, as idx was just deleted
                                an[idx], defval)
                an[idx] = defval
            end
        end
        newdef = :($fname($(sig...)) = $fname($(an...)))
        newvars = wherevars[findtypevars(newdef, wherevars)]
        @assert newdef.args[2].args[1] isa LineNumberNode
        newdef.args[2].args[1] = linefile
        push!(definitions, wherewrap!(newdef, newvars))
    end
    definitions
end

function replacedefault!(args, sym, defval)
    for i in eachindex(args)
        if args[i] isa Expr
            replacedefault!(args[i].args, sym, defval)
        elseif args[i] === sym
            args[i] = defval
        end
    end
end

function wherewrap!(def, wherevars)
    if !isempty(wherevars)
        def.args[1] = Expr(:where, def.args[1], wherevars...)
    end
    def
end

findtypevars(e::Expr, vars) = findtypevars!(falses(length(vars)), e, vars)

function findtypevars!(found, e::Expr, vars)
    for a in e.args
        all(found) && break # handles well empty case
        findtypevars!(found, a, vars)
    end
    found
end

function findtypevars!(found, e, vars)
    i = findfirst(==(e), vars)
    i !== nothing && (found[i] = true)
    found
end

function getargname(arg::Expr)
    if arg.head == :(::)
        if length(arg.args) == 2
            arg.args[1]
        else # length == 1
            arg1 = arg.args[1]
            arg1 isa Expr && arg1.head == :curly && arg1.args[1] == :Type ||
                throw(ArgumentError("unsupported argument"))
            arg1.args[2]
        end
    elseif arg.head == :kw
        getargname(arg.args[1])
    elseif arg.head == :...
        Expr(:..., getargname(arg.args[1]))
    else
        throw(ArgumentError("unsupported definition"))
    end
end

getargname(arg::Symbol) = arg
getargname(arg) = throw(ArgumentError("unsupported definition"))


end # module
