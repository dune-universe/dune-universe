--[ local variable definition --]
a = 100

--[ check the boolean condition --]

if( a == 10 ) then
   --[ if condition is true then print the following --]
   print("Value of a is 10" )
   print("Value of a is 10" )   
elseif( a == 20 ) then   
   --[ if else if condition is true --]
   print("Value of a is 20" )
   print("Value of a is 20" )   
elseif( a == 30 ) then
   --[ if else if condition is true  --]
   print("Value of a is 30" )
   print("Value of a is 30" )   
else
   --[ if none of the conditions is true --]
   print("None of the values is matching" )
   print("None of the values is matching" )   
end

function run()
   if( a == 10 ) then
      --[ if condition is true then print the following --]
      print("Value of a is 10" )
      print("Value of a is 10" )   
   elseif( a == 20 ) then   
      --[ if else if condition is true --]
      print("Value of a is 20" )
      print("Value of a is 20" )   
   elseif( a == 30 ) then
      --[ if else if condition is true  --]
      print("Value of a is 30" )
      print("Value of a is 30" )   
   else
      --[ if none of the conditions is true --]
      print("None of the values is matching" )
      print("None of the values is matching" )   
   end
end

function bb()
   a=10
   print(a)
end

repeat 
   print("Exact value of a is: ", a )
until a>10
