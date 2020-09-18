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

os.execute("mkdir fout")

for i=1,#fnames do
   run = "./l2l -lua "..fnames[i].." > ./fout/"..fnames[i]
   if fnames[i] ~= "Makefile" and fnames[i] ~= "Makefile.test" and fnames[i] ~= "l2l" then
      print(fnames[i])
      os.execute(run)
   end
end
