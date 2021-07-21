module Result = struct
  let (let*) = Result.bind
  let (let+) x f = Result.map f x
end
