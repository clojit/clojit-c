local function myplus (a)
  return a + a;
end

myplus(100);



-- BYTECODE -- function.lua:1-3
0001    ADDVV    1   0   0
0002    RET1     1   2

-- BYTECODE -- function.lua:0-6
0001    FNEW     0   0      ; function.lua:1
0002    MOV      1   0
0003    KSHORT   2 100
0004    CALL     1   1   2
0005    RET0     0   1
