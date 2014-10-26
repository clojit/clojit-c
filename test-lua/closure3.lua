local function foo(a)
    return function (b)
        return function (c)
            return c;
        end
    end
end

foo(1) --(2)(3)
