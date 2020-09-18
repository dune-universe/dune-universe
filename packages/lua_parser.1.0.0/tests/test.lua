local function ls(path)
   local str, handle, out, result
   if path == nil then
      str = "ls"
   else
      str = "ls "..path
   end
   handle = io.popen(str)
   result = handle:read("*a")
   handle:close()
   out = {}
   for tok in string.gmatch(result, "[^%s]+") do
      table.insert(out,tok)
   end
   return out
end

local fnames = ls()

local names = {}

for i=1,#fnames do
   if (fnames[i] ~= "dis_x86.lua" and fnames[i] ~= "test.lua" 
       and fnames[i] ~= "trans.lua" and fnames[i] ~= "xd.lua"
       and fnames[i] ~= "string.lua") then
      table.insert(names, "luajit "..fnames[i].."\necho\n")
   end
end

local out = table.concat(names)

os.execute(out)