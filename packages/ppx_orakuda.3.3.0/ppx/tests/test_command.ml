let x = {qx|hello|qx}
let x = {qx|hello%d|qx} 2
let x = {qx|hello%${x}d|qx}

let x = {qx|hello|qx} [@top]
let x = ({qx|hello%d|qx} [@top]) 2 
let x = {qx|hello%${x}d|qx} [@top]


