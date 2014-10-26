local function foo(a)
    local function fna (b)

        local function fnb (c) 
            return function (e) return a + b + c; end;
        end


        local function fnc (d) 
            return function (f) return a + b + d; end;
        end
        
        return fnb;
    end
end

print( foo(1)(2)  )


        let func = args.d as uint;
        vm.slots[args.a] = SCC(Closure{func:func,
                                       freevar:vec![]});
        vm.fetch_next()

--[[


]]--
