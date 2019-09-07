open Js_of_ocaml
open Chartjs

module Scheme = struct
  type t = Js.js_string

  let brewer_YlGn3 = Js.string "brewer.YlGn3"

  let brewer_YlGn4 = Js.string "brewer.YlGn4"

  let brewer_YlGn5 = Js.string "brewer.YlGn5"

  let brewer_YlGn6 = Js.string "brewer.YlGn6"

  let brewer_YlGn7 = Js.string "brewer.YlGn7"

  let brewer_YlGn8 = Js.string "brewer.YlGn8"

  let brewer_YlGn9 = Js.string "brewer.YlGn9"

  let brewer_YlGnBu3 = Js.string "brewer.YlGnBu3"

  let brewer_YlGnBu4 = Js.string "brewer.YlGnBu4"

  let brewer_YlGnBu5 = Js.string "brewer.YlGnBu5"

  let brewer_YlGnBu6 = Js.string "brewer.YlGnBu6"

  let brewer_YlGnBu7 = Js.string "brewer.YlGnBu7"

  let brewer_YlGnBu8 = Js.string "brewer.YlGnBu8"

  let brewer_YlGnBu9 = Js.string "brewer.YlGnBu9"

  let brewer_GnBu3 = Js.string "brewer.GnBu3"

  let brewer_GnBu4 = Js.string "brewer.GnBu4"

  let brewer_GnBu5 = Js.string "brewer.GnBu5"

  let brewer_GnBu6 = Js.string "brewer.GnBu6"

  let brewer_GnBu7 = Js.string "brewer.GnBu7"

  let brewer_GnBu8 = Js.string "brewer.GnBu8"

  let brewer_GnBu9 = Js.string "brewer.GnBu9"

  let brewer_BuGn3 = Js.string "brewer.BuGn3"

  let brewer_BuGn4 = Js.string "brewer.BuGn4"

  let brewer_BuGn5 = Js.string "brewer.BuGn5"

  let brewer_BuGn6 = Js.string "brewer.BuGn6"

  let brewer_BuGn7 = Js.string "brewer.BuGn7"

  let brewer_BuGn8 = Js.string "brewer.BuGn8"

  let brewer_BuGn9 = Js.string "brewer.BuGn9"

  let brewer_PuBuGn3 = Js.string "brewer.PuBuGn3"

  let brewer_PuBuGn4 = Js.string "brewer.PuBuGn4"

  let brewer_PuBuGn5 = Js.string "brewer.PuBuGn5"

  let brewer_PuBuGn6 = Js.string "brewer.PuBuGn6"

  let brewer_PuBuGn7 = Js.string "brewer.PuBuGn7"

  let brewer_PuBuGn8 = Js.string "brewer.PuBuGn8"

  let brewer_PuBuGn9 = Js.string "brewer.PuBuGn9"

  let brewer_PuBu3 = Js.string "brewer.PuBu3"

  let brewer_PuBu4 = Js.string "brewer.PuBu4"

  let brewer_PuBu5 = Js.string "brewer.PuBu5"

  let brewer_PuBu6 = Js.string "brewer.PuBu6"

  let brewer_PuBu7 = Js.string "brewer.PuBu7"

  let brewer_PuBu8 = Js.string "brewer.PuBu8"

  let brewer_PuBu9 = Js.string "brewer.PuBu9"

  let brewer_BuPu3 = Js.string "brewer.BuPu3"

  let brewer_BuPu4 = Js.string "brewer.BuPu4"

  let brewer_BuPu5 = Js.string "brewer.BuPu5"

  let brewer_BuPu6 = Js.string "brewer.BuPu6"

  let brewer_BuPu7 = Js.string "brewer.BuPu7"

  let brewer_BuPu8 = Js.string "brewer.BuPu8"

  let brewer_BuPu9 = Js.string "brewer.BuPu9"

  let brewer_RdPu3 = Js.string "brewer.RdPu3"

  let brewer_RdPu4 = Js.string "brewer.RdPu4"

  let brewer_RdPu5 = Js.string "brewer.RdPu5"

  let brewer_RdPu6 = Js.string "brewer.RdPu6"

  let brewer_RdPu7 = Js.string "brewer.RdPu7"

  let brewer_RdPu8 = Js.string "brewer.RdPu8"

  let brewer_RdPu9 = Js.string "brewer.RdPu9"

  let brewer_PuRd3 = Js.string "brewer.PuRd3"

  let brewer_PuRd4 = Js.string "brewer.PuRd4"

  let brewer_PuRd5 = Js.string "brewer.PuRd5"

  let brewer_PuRd6 = Js.string "brewer.PuRd6"

  let brewer_PuRd7 = Js.string "brewer.PuRd7"

  let brewer_PuRd8 = Js.string "brewer.PuRd8"

  let brewer_PuRd9 = Js.string "brewer.PuRd9"

  let brewer_OrRd3 = Js.string "brewer.OrRd3"

  let brewer_OrRd4 = Js.string "brewer.OrRd4"

  let brewer_OrRd5 = Js.string "brewer.OrRd5"

  let brewer_OrRd6 = Js.string "brewer.OrRd6"

  let brewer_OrRd7 = Js.string "brewer.OrRd7"

  let brewer_OrRd8 = Js.string "brewer.OrRd8"

  let brewer_OrRd9 = Js.string "brewer.OrRd9"

  let brewer_YlOrRd3 = Js.string "brewer.YlOrRd3"

  let brewer_YlOrRd4 = Js.string "brewer.YlOrRd4"

  let brewer_YlOrRd5 = Js.string "brewer.YlOrRd5"

  let brewer_YlOrRd6 = Js.string "brewer.YlOrRd6"

  let brewer_YlOrRd7 = Js.string "brewer.YlOrRd7"

  let brewer_YlOrRd8 = Js.string "brewer.YlOrRd8"

  let brewer_YlOrRd9 = Js.string "brewer.YlOrRd9"

  let brewer_YlOrBr3 = Js.string "brewer.YlOrBr3"

  let brewer_YlOrBr4 = Js.string "brewer.YlOrBr4"

  let brewer_YlOrBr5 = Js.string "brewer.YlOrBr5"

  let brewer_YlOrBr6 = Js.string "brewer.YlOrBr6"

  let brewer_YlOrBr7 = Js.string "brewer.YlOrBr7"

  let brewer_YlOrBr8 = Js.string "brewer.YlOrBr8"

  let brewer_YlOrBr9 = Js.string "brewer.YlOrBr9"

  let brewer_Purples3 = Js.string "brewer.Purples3"

  let brewer_Purples4 = Js.string "brewer.Purples4"

  let brewer_Purples5 = Js.string "brewer.Purples5"

  let brewer_Purples6 = Js.string "brewer.Purples6"

  let brewer_Purples7 = Js.string "brewer.Purples7"

  let brewer_Purples8 = Js.string "brewer.Purples8"

  let brewer_Purples9 = Js.string "brewer.Purples9"

  let brewer_Blues3 = Js.string "brewer.Blues3"

  let brewer_Blues4 = Js.string "brewer.Blues4"

  let brewer_Blues5 = Js.string "brewer.Blues5"

  let brewer_Blues6 = Js.string "brewer.Blues6"

  let brewer_Blues7 = Js.string "brewer.Blues7"

  let brewer_Blues8 = Js.string "brewer.Blues8"

  let brewer_Blues9 = Js.string "brewer.Blues9"

  let brewer_Greens3 = Js.string "brewer.Greens3"

  let brewer_Greens4 = Js.string "brewer.Greens4"

  let brewer_Greens5 = Js.string "brewer.Greens5"

  let brewer_Greens6 = Js.string "brewer.Greens6"

  let brewer_Greens7 = Js.string "brewer.Greens7"

  let brewer_Greens8 = Js.string "brewer.Greens8"

  let brewer_Greens9 = Js.string "brewer.Greens9"

  let brewer_Oranges3 = Js.string "brewer.Oranges3"

  let brewer_Oranges4 = Js.string "brewer.Oranges4"

  let brewer_Oranges5 = Js.string "brewer.Oranges5"

  let brewer_Oranges6 = Js.string "brewer.Oranges6"

  let brewer_Oranges7 = Js.string "brewer.Oranges7"

  let brewer_Oranges8 = Js.string "brewer.Oranges8"

  let brewer_Oranges9 = Js.string "brewer.Oranges9"

  let brewer_Reds3 = Js.string "brewer.Reds3"

  let brewer_Reds4 = Js.string "brewer.Reds4"

  let brewer_Reds5 = Js.string "brewer.Reds5"

  let brewer_Reds6 = Js.string "brewer.Reds6"

  let brewer_Reds7 = Js.string "brewer.Reds7"

  let brewer_Reds8 = Js.string "brewer.Reds8"

  let brewer_Reds9 = Js.string "brewer.Reds9"

  let brewer_Greys3 = Js.string "brewer.Greys3"

  let brewer_Greys4 = Js.string "brewer.Greys4"

  let brewer_Greys5 = Js.string "brewer.Greys5"

  let brewer_Greys6 = Js.string "brewer.Greys6"

  let brewer_Greys7 = Js.string "brewer.Greys7"

  let brewer_Greys8 = Js.string "brewer.Greys8"

  let brewer_Greys9 = Js.string "brewer.Greys9"

  let brewer_PuOr3 = Js.string "brewer.PuOr3"

  let brewer_PuOr4 = Js.string "brewer.PuOr4"

  let brewer_PuOr5 = Js.string "brewer.PuOr5"

  let brewer_PuOr6 = Js.string "brewer.PuOr6"

  let brewer_PuOr7 = Js.string "brewer.PuOr7"

  let brewer_PuOr8 = Js.string "brewer.PuOr8"

  let brewer_PuOr9 = Js.string "brewer.PuOr9"

  let brewer_PuOr10 = Js.string "brewer.PuOr10"

  let brewer_PuOr11 = Js.string "brewer.PuOr11"

  let brewer_BrBG3 = Js.string "brewer.BrBG3"

  let brewer_BrBG4 = Js.string "brewer.BrBG4"

  let brewer_BrBG5 = Js.string "brewer.BrBG5"

  let brewer_BrBG6 = Js.string "brewer.BrBG6"

  let brewer_BrBG7 = Js.string "brewer.BrBG7"

  let brewer_BrBG8 = Js.string "brewer.BrBG8"

  let brewer_BrBG9 = Js.string "brewer.BrBG9"

  let brewer_BrBG10 = Js.string "brewer.BrBG10"

  let brewer_BrBG11 = Js.string "brewer.BrBG11"

  let brewer_PRGn3 = Js.string "brewer.PRGn3"

  let brewer_PRGn4 = Js.string "brewer.PRGn4"

  let brewer_PRGn5 = Js.string "brewer.PRGn5"

  let brewer_PRGn6 = Js.string "brewer.PRGn6"

  let brewer_PRGn7 = Js.string "brewer.PRGn7"

  let brewer_PRGn8 = Js.string "brewer.PRGn8"

  let brewer_PRGn9 = Js.string "brewer.PRGn9"

  let brewer_PRGn10 = Js.string "brewer.PRGn10"

  let brewer_PRGn11 = Js.string "brewer.PRGn11"

  let brewer_PiYG3 = Js.string "brewer.PiYG3"

  let brewer_PiYG4 = Js.string "brewer.PiYG4"

  let brewer_PiYG5 = Js.string "brewer.PiYG5"

  let brewer_PiYG6 = Js.string "brewer.PiYG6"

  let brewer_PiYG7 = Js.string "brewer.PiYG7"

  let brewer_PiYG8 = Js.string "brewer.PiYG8"

  let brewer_PiYG9 = Js.string "brewer.PiYG9"

  let brewer_PiYG10 = Js.string "brewer.PiYG10"

  let brewer_PiYG11 = Js.string "brewer.PiYG11"

  let brewer_RdBu3 = Js.string "brewer.RdBu3"

  let brewer_RdBu4 = Js.string "brewer.RdBu4"

  let brewer_RdBu5 = Js.string "brewer.RdBu5"

  let brewer_RdBu6 = Js.string "brewer.RdBu6"

  let brewer_RdBu7 = Js.string "brewer.RdBu7"

  let brewer_RdBu8 = Js.string "brewer.RdBu8"

  let brewer_RdBu9 = Js.string "brewer.RdBu9"

  let brewer_RdBu10 = Js.string "brewer.RdBu10"

  let brewer_RdBu11 = Js.string "brewer.RdBu11"

  let brewer_RdGy3 = Js.string "brewer.RdGy3"

  let brewer_RdGy4 = Js.string "brewer.RdGy4"

  let brewer_RdGy5 = Js.string "brewer.RdGy5"

  let brewer_RdGy6 = Js.string "brewer.RdGy6"

  let brewer_RdGy7 = Js.string "brewer.RdGy7"

  let brewer_RdGy8 = Js.string "brewer.RdGy8"

  let brewer_RdGy9 = Js.string "brewer.RdGy9"

  let brewer_RdGy10 = Js.string "brewer.RdGy10"

  let brewer_RdGy11 = Js.string "brewer.RdGy11"

  let brewer_RdYlBu3 = Js.string "brewer.RdYlBu3"

  let brewer_RdYlBu4 = Js.string "brewer.RdYlBu4"

  let brewer_RdYlBu5 = Js.string "brewer.RdYlBu5"

  let brewer_RdYlBu6 = Js.string "brewer.RdYlBu6"

  let brewer_RdYlBu7 = Js.string "brewer.RdYlBu7"

  let brewer_RdYlBu8 = Js.string "brewer.RdYlBu8"

  let brewer_RdYlBu9 = Js.string "brewer.RdYlBu9"

  let brewer_RdYlBu10 = Js.string "brewer.RdYlBu10"

  let brewer_RdYlBu11 = Js.string "brewer.RdYlBu11"

  let brewer_Spectral3 = Js.string "brewer.Spectral3"

  let brewer_Spectral4 = Js.string "brewer.Spectral4"

  let brewer_Spectral5 = Js.string "brewer.Spectral5"

  let brewer_Spectral6 = Js.string "brewer.Spectral6"

  let brewer_Spectral7 = Js.string "brewer.Spectral7"

  let brewer_Spectral8 = Js.string "brewer.Spectral8"

  let brewer_Spectral9 = Js.string "brewer.Spectral9"

  let brewer_Spectral10 = Js.string "brewer.Spectral10"

  let brewer_Spectral11 = Js.string "brewer.Spectral11"

  let brewer_RdYlGn3 = Js.string "brewer.RdYlGn3"

  let brewer_RdYlGn4 = Js.string "brewer.RdYlGn4"

  let brewer_RdYlGn5 = Js.string "brewer.RdYlGn5"

  let brewer_RdYlGn6 = Js.string "brewer.RdYlGn6"

  let brewer_RdYlGn7 = Js.string "brewer.RdYlGn7"

  let brewer_RdYlGn8 = Js.string "brewer.RdYlGn8"

  let brewer_RdYlGn9 = Js.string "brewer.RdYlGn9"

  let brewer_RdYlGn10 = Js.string "brewer.RdYlGn10"

  let brewer_RdYlGn11 = Js.string "brewer.RdYlGn11"

  let brewer_Accent3 = Js.string "brewer.Accent3"

  let brewer_Accent4 = Js.string "brewer.Accent4"

  let brewer_Accent5 = Js.string "brewer.Accent5"

  let brewer_Accent6 = Js.string "brewer.Accent6"

  let brewer_Accent7 = Js.string "brewer.Accent7"

  let brewer_Accent8 = Js.string "brewer.Accent8"

  let brewer_DarkTwo3 = Js.string "brewer.DarkTwo3"

  let brewer_DarkTwo4 = Js.string "brewer.DarkTwo4"

  let brewer_DarkTwo5 = Js.string "brewer.DarkTwo5"

  let brewer_DarkTwo6 = Js.string "brewer.DarkTwo6"

  let brewer_DarkTwo7 = Js.string "brewer.DarkTwo7"

  let brewer_DarkTwo8 = Js.string "brewer.DarkTwo8"

  let brewer_Paired3 = Js.string "brewer.Paired3"

  let brewer_Paired4 = Js.string "brewer.Paired4"

  let brewer_Paired5 = Js.string "brewer.Paired5"

  let brewer_Paired6 = Js.string "brewer.Paired6"

  let brewer_Paired7 = Js.string "brewer.Paired7"

  let brewer_Paired8 = Js.string "brewer.Paired8"

  let brewer_Paired9 = Js.string "brewer.Paired9"

  let brewer_Paired10 = Js.string "brewer.Paired10"

  let brewer_Paired11 = Js.string "brewer.Paired11"

  let brewer_Paired12 = Js.string "brewer.Paired12"

  let brewer_PastelOne3 = Js.string "brewer.PastelOne3"

  let brewer_PastelOne4 = Js.string "brewer.PastelOne4"

  let brewer_PastelOne5 = Js.string "brewer.PastelOne5"

  let brewer_PastelOne6 = Js.string "brewer.PastelOne6"

  let brewer_PastelOne7 = Js.string "brewer.PastelOne7"

  let brewer_PastelOne8 = Js.string "brewer.PastelOne8"

  let brewer_PastelOne9 = Js.string "brewer.PastelOne9"

  let brewer_PastelTwo3 = Js.string "brewer.PastelTwo3"

  let brewer_PastelTwo4 = Js.string "brewer.PastelTwo4"

  let brewer_PastelTwo5 = Js.string "brewer.PastelTwo5"

  let brewer_PastelTwo6 = Js.string "brewer.PastelTwo6"

  let brewer_PastelTwo7 = Js.string "brewer.PastelTwo7"

  let brewer_PastelTwo8 = Js.string "brewer.PastelTwo8"

  let brewer_SetOne3 = Js.string "brewer.SetOne3"

  let brewer_SetOne4 = Js.string "brewer.SetOne4"

  let brewer_SetOne5 = Js.string "brewer.SetOne5"

  let brewer_SetOne6 = Js.string "brewer.SetOne6"

  let brewer_SetOne7 = Js.string "brewer.SetOne7"

  let brewer_SetOne8 = Js.string "brewer.SetOne8"

  let brewer_SetOne9 = Js.string "brewer.SetOne9"

  let brewer_SetTwo3 = Js.string "brewer.SetTwo3"

  let brewer_SetTwo4 = Js.string "brewer.SetTwo4"

  let brewer_SetTwo5 = Js.string "brewer.SetTwo5"

  let brewer_SetTwo6 = Js.string "brewer.SetTwo6"

  let brewer_SetTwo7 = Js.string "brewer.SetTwo7"

  let brewer_SetTwo8 = Js.string "brewer.SetTwo8"

  let brewer_SetThree3 = Js.string "brewer.SetThree3"

  let brewer_SetThree4 = Js.string "brewer.SetThree4"

  let brewer_SetThree5 = Js.string "brewer.SetThree5"

  let brewer_SetThree6 = Js.string "brewer.SetThree6"

  let brewer_SetThree7 = Js.string "brewer.SetThree7"

  let brewer_SetThree8 = Js.string "brewer.SetThree8"

  let brewer_SetThree9 = Js.string "brewer.SetThree9"

  let brewer_SetThree10 = Js.string "brewer.SetThree10"

  let brewer_SetThree11 = Js.string "brewer.SetThree11"

  let brewer_SetThree12 = Js.string "brewer.SetThree12"

  let office_Adjacency6 = Js.string "office.Adjacency6"

  let office_Advantage6 = Js.string "office.Advantage6"

  let office_Angles6 = Js.string "office.Angles6"

  let office_Apex6 = Js.string "office.Apex6"

  let office_Apothecary6 = Js.string "office.Apothecary6"

  let office_Aspect6 = Js.string "office.Aspect6"

  let office_Atlas6 = Js.string "office.Atlas6"

  let office_Austin6 = Js.string "office.Austin6"

  let office_Badge6 = Js.string "office.Badge6"

  let office_Banded6 = Js.string "office.Banded6"

  let office_Basis6 = Js.string "office.Basis6"

  let office_Berlin6 = Js.string "office.Berlin6"

  let office_BlackTie6 = Js.string "office.BlackTie6"

  let office_Blue6 = Js.string "office.Blue6"

  let office_BlueGreen6 = Js.string "office.BlueGreen6"

  let office_BlueII6 = Js.string "office.BlueII6"

  let office_BlueRed6 = Js.string "office.BlueRed6"

  let office_BlueWarm6 = Js.string "office.BlueWarm6"

  let office_Breeze6 = Js.string "office.Breeze6"

  let office_Capital6 = Js.string "office.Capital6"

  let office_Celestial6 = Js.string "office.Celestial6"

  let office_Circuit6 = Js.string "office.Circuit6"

  let office_Civic6 = Js.string "office.Civic6"

  let office_Clarity6 = Js.string "office.Clarity6"

  let office_Codex6 = Js.string "office.Codex6"

  let office_Composite6 = Js.string "office.Composite6"

  let office_Concourse6 = Js.string "office.Concourse6"

  let office_Couture6 = Js.string "office.Couture6"

  let office_Crop6 = Js.string "office.Crop6"

  let office_Damask6 = Js.string "office.Damask6"

  let office_Depth6 = Js.string "office.Depth6"

  let office_Dividend6 = Js.string "office.Dividend6"

  let office_Droplet6 = Js.string "office.Droplet6"

  let office_Elemental6 = Js.string "office.Elemental6"

  let office_Equity6 = Js.string "office.Equity6"

  let office_Essential6 = Js.string "office.Essential6"

  let office_Excel16 = Js.string "office.Excel16"

  let office_Executive6 = Js.string "office.Executive6"

  let office_Exhibit6 = Js.string "office.Exhibit6"

  let office_Expo6 = Js.string "office.Expo6"

  let office_Facet6 = Js.string "office.Facet6"

  let office_Feathered6 = Js.string "office.Feathered6"

  let office_Flow6 = Js.string "office.Flow6"

  let office_Focus6 = Js.string "office.Focus6"

  let office_Folio6 = Js.string "office.Folio6"

  let office_Formal6 = Js.string "office.Formal6"

  let office_Forte6 = Js.string "office.Forte6"

  let office_Foundry6 = Js.string "office.Foundry6"

  let office_Frame6 = Js.string "office.Frame6"

  let office_Gallery6 = Js.string "office.Gallery6"

  let office_Genesis6 = Js.string "office.Genesis6"

  let office_Grayscale6 = Js.string "office.Grayscale6"

  let office_Green6 = Js.string "office.Green6"

  let office_GreenYellow6 = Js.string "office.GreenYellow6"

  let office_Grid6 = Js.string "office.Grid6"

  let office_Habitat6 = Js.string "office.Habitat6"

  let office_Hardcover6 = Js.string "office.Hardcover6"

  let office_Headlines6 = Js.string "office.Headlines6"

  let office_Horizon6 = Js.string "office.Horizon6"

  let office_Infusion6 = Js.string "office.Infusion6"

  let office_Inkwell6 = Js.string "office.Inkwell6"

  let office_Inspiration6 = Js.string "office.Inspiration6"

  let office_Integral6 = Js.string "office.Integral6"

  let office_Ion6 = Js.string "office.Ion6"

  let office_IonBoardroom6 = Js.string "office.IonBoardroom6"

  let office_Kilter6 = Js.string "office.Kilter6"

  let office_Madison6 = Js.string "office.Madison6"

  let office_MainEvent6 = Js.string "office.MainEvent6"

  let office_Marquee6 = Js.string "office.Marquee6"

  let office_Median6 = Js.string "office.Median6"

  let office_Mesh6 = Js.string "office.Mesh6"

  let office_Metail6 = Js.string "office.Metail6"

  let office_Metro6 = Js.string "office.Metro6"

  let office_Metropolitan6 = Js.string "office.Metropolitan6"

  let office_Module6 = Js.string "office.Module6"

  let office_NewsPrint6 = Js.string "office.NewsPrint6"

  let office_Office6 = Js.string "office.Office6"

  let office_OfficeClassic6 = Js.string "office.OfficeClassic6"

  let office_Opulent6 = Js.string "office.Opulent6"

  let office_Orange6 = Js.string "office.Orange6"

  let office_OrangeRed6 = Js.string "office.OrangeRed6"

  let office_Orbit6 = Js.string "office.Orbit6"

  let office_Organic6 = Js.string "office.Organic6"

  let office_Oriel6 = Js.string "office.Oriel6"

  let office_Origin6 = Js.string "office.Origin6"

  let office_Paper6 = Js.string "office.Paper6"

  let office_Parallax6 = Js.string "office.Parallax6"

  let office_Parcel6 = Js.string "office.Parcel6"

  let office_Perception6 = Js.string "office.Perception6"

  let office_Perspective6 = Js.string "office.Perspective6"

  let office_Pixel6 = Js.string "office.Pixel6"

  let office_Plaza6 = Js.string "office.Plaza6"

  let office_Precedent6 = Js.string "office.Precedent6"

  let office_Pushpin6 = Js.string "office.Pushpin6"

  let office_Quotable6 = Js.string "office.Quotable6"

  let office_Red6 = Js.string "office.Red6"

  let office_RedOrange6 = Js.string "office.RedOrange6"

  let office_RedViolet6 = Js.string "office.RedViolet6"

  let office_Retrospect6 = Js.string "office.Retrospect6"

  let office_Revolution6 = Js.string "office.Revolution6"

  let office_Saddle6 = Js.string "office.Saddle6"

  let office_Savon6 = Js.string "office.Savon6"

  let office_Sketchbook6 = Js.string "office.Sketchbook6"

  let office_Sky6 = Js.string "office.Sky6"

  let office_Slate6 = Js.string "office.Slate6"

  let office_Slice6 = Js.string "office.Slice6"

  let office_Slipstream6 = Js.string "office.Slipstream6"

  let office_SOHO6 = Js.string "office.SOHO6"

  let office_Solstice6 = Js.string "office.Solstice6"

  let office_Spectrum6 = Js.string "office.Spectrum6"

  let office_Story6 = Js.string "office.Story6"

  let office_Studio6 = Js.string "office.Studio6"

  let office_Summer6 = Js.string "office.Summer6"

  let office_Technic6 = Js.string "office.Technic6"

  let office_Thatch6 = Js.string "office.Thatch6"

  let office_Tradition6 = Js.string "office.Tradition6"

  let office_Travelogue6 = Js.string "office.Travelogue6"

  let office_Trek6 = Js.string "office.Trek6"

  let office_Twilight6 = Js.string "office.Twilight6"

  let office_Urban6 = Js.string "office.Urban6"

  let office_UrbanPop6 = Js.string "office.UrbanPop6"

  let office_VaporTrail6 = Js.string "office.VaporTrail6"

  let office_Venture6 = Js.string "office.Venture6"

  let office_Verve6 = Js.string "office.Verve6"

  let office_View6 = Js.string "office.View6"

  let office_Violet6 = Js.string "office.Violet6"

  let office_VioletII6 = Js.string "office.VioletII6"

  let office_Waveform6 = Js.string "office.Waveform6"

  let office_Wisp6 = Js.string "office.Wisp6"

  let office_WoodType6 = Js.string "office.WoodType6"

  let office_Yellow6 = Js.string "office.Yellow6"

  let office_YellowOrange6 = Js.string "office.YellowOrange6"

  let tableau_Tableau10 = Js.string "tableau.Tableau10"

  let tableau_Tableau20 = Js.string "tableau.Tableau20"

  let tableau_ColorBlind10 = Js.string "tableau.ColorBlind10"

  let tableau_SeattleGrays5 = Js.string "tableau.SeattleGrays5"

  let tableau_Traffic9 = Js.string "tableau.Traffic9"

  let tableau_MillerStone11 = Js.string "tableau.MillerStone11"

  let tableau_SuperfishelStone10 = Js.string "tableau.SuperfishelStone10"

  let tableau_NurielStone9 = Js.string "tableau.NurielStone9"

  let tableau_JewelBright9 = Js.string "tableau.JewelBright9"

  let tableau_Summer8 = Js.string "tableau.Summer8"

  let tableau_Winter10 = Js.string "tableau.Winter10"

  let tableau_GreenOrangeTeal12 = Js.string "tableau.GreenOrangeTeal12"

  let tableau_RedBlueBrown12 = Js.string "tableau.RedBlueBrown12"

  let tableau_PurplePinkGray12 = Js.string "tableau.PurplePinkGray12"

  let tableau_HueCircle19 = Js.string "tableau.HueCircle19"

  let tableau_OrangeBlue7 = Js.string "tableau.OrangeBlue7"

  let tableau_RedGreen7 = Js.string "tableau.RedGreen7"

  let tableau_GreenBlue7 = Js.string "tableau.GreenBlue7"

  let tableau_RedBlue7 = Js.string "tableau.RedBlue7"

  let tableau_RedBlack7 = Js.string "tableau.RedBlack7"

  let tableau_GoldPurple7 = Js.string "tableau.GoldPurple7"

  let tableau_RedGreenGold7 = Js.string "tableau.RedGreenGold7"

  let tableau_SunsetSunrise7 = Js.string "tableau.SunsetSunrise7"

  let tableau_OrangeBlueWhite7 = Js.string "tableau.OrangeBlueWhite7"

  let tableau_RedGreenWhite7 = Js.string "tableau.RedGreenWhite7"

  let tableau_GreenBlueWhite7 = Js.string "tableau.GreenBlueWhite7"

  let tableau_RedBlueWhite7 = Js.string "tableau.RedBlueWhite7"

  let tableau_RedBlackWhite7 = Js.string "tableau.RedBlackWhite7"

  let tableau_OrangeBlueLight7 = Js.string "tableau.OrangeBlueLight7"

  let tableau_Temperature7 = Js.string "tableau.Temperature7"

  let tableau_BlueGreen7 = Js.string "tableau.BlueGreen7"

  let tableau_BlueLight7 = Js.string "tableau.BlueLight7"

  let tableau_OrangeLight7 = Js.string "tableau.OrangeLight7"

  let tableau_Blue20 = Js.string "tableau.Blue20"

  let tableau_Orange20 = Js.string "tableau.Orange20"

  let tableau_Green20 = Js.string "tableau.Green20"

  let tableau_Red20 = Js.string "tableau.Red20"

  let tableau_Purple20 = Js.string "tableau.Purple20"

  let tableau_Brown20 = Js.string "tableau.Brown20"

  let tableau_Gray20 = Js.string "tableau.Gray20"

  let tableau_GrayWarm20 = Js.string "tableau.GrayWarm20"

  let tableau_BlueTeal20 = Js.string "tableau.BlueTeal20"

  let tableau_OrangeGold20 = Js.string "tableau.OrangeGold20"

  let tableau_GreenGold20 = Js.string "tableau.GreenGold20"

  let tableau_RedGold21 = Js.string "tableau.RedGold21"

  let tableau_Classic10 = Js.string "tableau.Classic10"

  let tableau_ClassicMedium10 = Js.string "tableau.ClassicMedium10"

  let tableau_ClassicLight10 = Js.string "tableau.ClassicLight10"

  let tableau_Classic20 = Js.string "tableau.Classic20"

  let tableau_ClassicGray5 = Js.string "tableau.ClassicGray5"

  let tableau_ClassicColorBlind10 = Js.string "tableau.ClassicColorBlind10"

  let tableau_ClassicTrafficLight9 = Js.string "tableau.ClassicTrafficLight9"

  let tableau_ClassicPurpleGray6 = Js.string "tableau.ClassicPurpleGray6"

  let tableau_ClassicPurpleGray12 = Js.string "tableau.ClassicPurpleGray12"

  let tableau_ClassicGreenOrange6 = Js.string "tableau.ClassicGreenOrange6"

  let tableau_ClassicGreenOrange12 = Js.string "tableau.ClassicGreenOrange12"

  let tableau_ClassicBlueRed6 = Js.string "tableau.ClassicBlueRed6"

  let tableau_ClassicBlueRed12 = Js.string "tableau.ClassicBlueRed12"

  let tableau_ClassicCyclic13 = Js.string "tableau.ClassicCyclic13"

  let tableau_ClassicGreen7 = Js.string "tableau.ClassicGreen7"

  let tableau_ClassicGray13 = Js.string "tableau.ClassicGray13"

  let tableau_ClassicBlue7 = Js.string "tableau.ClassicBlue7"

  let tableau_ClassicRed9 = Js.string "tableau.ClassicRed9"

  let tableau_ClassicOrange7 = Js.string "tableau.ClassicOrange7"

  let tableau_ClassicAreaRed11 = Js.string "tableau.ClassicAreaRed11"

  let tableau_ClassicAreaGreen11 = Js.string "tableau.ClassicAreaGreen11"

  let tableau_ClassicAreaBrown11 = Js.string "tableau.ClassicAreaBrown11"

  let tableau_ClassicRedGreen11 = Js.string "tableau.ClassicRedGreen11"

  let tableau_ClassicRedBlue11 = Js.string "tableau.ClassicRedBlue11"

  let tableau_ClassicRedBlack11 = Js.string "tableau.ClassicRedBlack11"

  let tableau_ClassicAreaRedGreen21 = Js.string "tableau.ClassicAreaRedGreen21"

  let tableau_ClassicOrangeBlue13 = Js.string "tableau.ClassicOrangeBlue13"

  let tableau_ClassicGreenBlue11 = Js.string "tableau.ClassicGreenBlue11"

  let tableau_ClassicRedWhiteGreen11 = Js.string "tableau.ClassicRedWhiteGreen11"

  let tableau_ClassicRedWhiteBlack11 = Js.string "tableau.ClassicRedWhiteBlack11"

  let tableau_ClassicOrangeWhiteBlue11 = Js.string "tableau.ClassicOrangeWhiteBlue11"

  let tableau_ClassicRedWhiteBlackLight10 =
    Js.string "tableau.ClassicRedWhiteBlackLight10"

  let tableau_ClassicOrangeWhiteBlueLight11 =
    Js.string "tableau.ClassicOrangeWhiteBlueLight11"

  let tableau_ClassicRedWhiteGreenLight11 =
    Js.string "tableau.ClassicRedWhiteGreenLight11"

  let tableau_ClassicRedGreenLight11 = Js.string "tableau.ClassicRedGreenLight11"

  let make = Js.string
end

class type colorschemes =
  object
    method fillAlpha : float Js.t Js.prop

    method scheme : Scheme.t Js.t Indexable.t Js.t Js.prop

    method reverse : bool Js.t Js.prop

    method override : bool Js.t Js.prop

    method custom :
      (Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.js_array Js.t)
      Js.callback
      Js.optdef
      Js.prop
  end

let empty_colorschemes_config () = Js.Unsafe.obj [||]

let of_chart_options options = (Js.Unsafe.coerce options)##.plugins##.colorschemes

let of_global () = Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.colorschemes

let set_to_chart_options options plugin =
  (Js.Unsafe.coerce options)##.plugins##.colorschemes := plugin

let set_globally plugin =
  Js.Unsafe.global##._Chart##.defaults##.global##.plugins##.colorschemes := plugin
