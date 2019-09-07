open Js_of_ocaml
open Chartjs

module Scheme : sig
  type t

  val brewer_YlGn3 : t Js.t

  val brewer_YlGn4 : t Js.t

  val brewer_YlGn5 : t Js.t

  val brewer_YlGn6 : t Js.t

  val brewer_YlGn7 : t Js.t

  val brewer_YlGn8 : t Js.t

  val brewer_YlGn9 : t Js.t

  val brewer_YlGnBu3 : t Js.t

  val brewer_YlGnBu4 : t Js.t

  val brewer_YlGnBu5 : t Js.t

  val brewer_YlGnBu6 : t Js.t

  val brewer_YlGnBu7 : t Js.t

  val brewer_YlGnBu8 : t Js.t

  val brewer_YlGnBu9 : t Js.t

  val brewer_GnBu3 : t Js.t

  val brewer_GnBu4 : t Js.t

  val brewer_GnBu5 : t Js.t

  val brewer_GnBu6 : t Js.t

  val brewer_GnBu7 : t Js.t

  val brewer_GnBu8 : t Js.t

  val brewer_GnBu9 : t Js.t

  val brewer_BuGn3 : t Js.t

  val brewer_BuGn4 : t Js.t

  val brewer_BuGn5 : t Js.t

  val brewer_BuGn6 : t Js.t

  val brewer_BuGn7 : t Js.t

  val brewer_BuGn8 : t Js.t

  val brewer_BuGn9 : t Js.t

  val brewer_PuBuGn3 : t Js.t

  val brewer_PuBuGn4 : t Js.t

  val brewer_PuBuGn5 : t Js.t

  val brewer_PuBuGn6 : t Js.t

  val brewer_PuBuGn7 : t Js.t

  val brewer_PuBuGn8 : t Js.t

  val brewer_PuBuGn9 : t Js.t

  val brewer_PuBu3 : t Js.t

  val brewer_PuBu4 : t Js.t

  val brewer_PuBu5 : t Js.t

  val brewer_PuBu6 : t Js.t

  val brewer_PuBu7 : t Js.t

  val brewer_PuBu8 : t Js.t

  val brewer_PuBu9 : t Js.t

  val brewer_BuPu3 : t Js.t

  val brewer_BuPu4 : t Js.t

  val brewer_BuPu5 : t Js.t

  val brewer_BuPu6 : t Js.t

  val brewer_BuPu7 : t Js.t

  val brewer_BuPu8 : t Js.t

  val brewer_BuPu9 : t Js.t

  val brewer_RdPu3 : t Js.t

  val brewer_RdPu4 : t Js.t

  val brewer_RdPu5 : t Js.t

  val brewer_RdPu6 : t Js.t

  val brewer_RdPu7 : t Js.t

  val brewer_RdPu8 : t Js.t

  val brewer_RdPu9 : t Js.t

  val brewer_PuRd3 : t Js.t

  val brewer_PuRd4 : t Js.t

  val brewer_PuRd5 : t Js.t

  val brewer_PuRd6 : t Js.t

  val brewer_PuRd7 : t Js.t

  val brewer_PuRd8 : t Js.t

  val brewer_PuRd9 : t Js.t

  val brewer_OrRd3 : t Js.t

  val brewer_OrRd4 : t Js.t

  val brewer_OrRd5 : t Js.t

  val brewer_OrRd6 : t Js.t

  val brewer_OrRd7 : t Js.t

  val brewer_OrRd8 : t Js.t

  val brewer_OrRd9 : t Js.t

  val brewer_YlOrRd3 : t Js.t

  val brewer_YlOrRd4 : t Js.t

  val brewer_YlOrRd5 : t Js.t

  val brewer_YlOrRd6 : t Js.t

  val brewer_YlOrRd7 : t Js.t

  val brewer_YlOrRd8 : t Js.t

  val brewer_YlOrRd9 : t Js.t

  val brewer_YlOrBr3 : t Js.t

  val brewer_YlOrBr4 : t Js.t

  val brewer_YlOrBr5 : t Js.t

  val brewer_YlOrBr6 : t Js.t

  val brewer_YlOrBr7 : t Js.t

  val brewer_YlOrBr8 : t Js.t

  val brewer_YlOrBr9 : t Js.t

  val brewer_Purples3 : t Js.t

  val brewer_Purples4 : t Js.t

  val brewer_Purples5 : t Js.t

  val brewer_Purples6 : t Js.t

  val brewer_Purples7 : t Js.t

  val brewer_Purples8 : t Js.t

  val brewer_Purples9 : t Js.t

  val brewer_Blues3 : t Js.t

  val brewer_Blues4 : t Js.t

  val brewer_Blues5 : t Js.t

  val brewer_Blues6 : t Js.t

  val brewer_Blues7 : t Js.t

  val brewer_Blues8 : t Js.t

  val brewer_Blues9 : t Js.t

  val brewer_Greens3 : t Js.t

  val brewer_Greens4 : t Js.t

  val brewer_Greens5 : t Js.t

  val brewer_Greens6 : t Js.t

  val brewer_Greens7 : t Js.t

  val brewer_Greens8 : t Js.t

  val brewer_Greens9 : t Js.t

  val brewer_Oranges3 : t Js.t

  val brewer_Oranges4 : t Js.t

  val brewer_Oranges5 : t Js.t

  val brewer_Oranges6 : t Js.t

  val brewer_Oranges7 : t Js.t

  val brewer_Oranges8 : t Js.t

  val brewer_Oranges9 : t Js.t

  val brewer_Reds3 : t Js.t

  val brewer_Reds4 : t Js.t

  val brewer_Reds5 : t Js.t

  val brewer_Reds6 : t Js.t

  val brewer_Reds7 : t Js.t

  val brewer_Reds8 : t Js.t

  val brewer_Reds9 : t Js.t

  val brewer_Greys3 : t Js.t

  val brewer_Greys4 : t Js.t

  val brewer_Greys5 : t Js.t

  val brewer_Greys6 : t Js.t

  val brewer_Greys7 : t Js.t

  val brewer_Greys8 : t Js.t

  val brewer_Greys9 : t Js.t

  val brewer_PuOr3 : t Js.t

  val brewer_PuOr4 : t Js.t

  val brewer_PuOr5 : t Js.t

  val brewer_PuOr6 : t Js.t

  val brewer_PuOr7 : t Js.t

  val brewer_PuOr8 : t Js.t

  val brewer_PuOr9 : t Js.t

  val brewer_PuOr10 : t Js.t

  val brewer_PuOr11 : t Js.t

  val brewer_BrBG3 : t Js.t

  val brewer_BrBG4 : t Js.t

  val brewer_BrBG5 : t Js.t

  val brewer_BrBG6 : t Js.t

  val brewer_BrBG7 : t Js.t

  val brewer_BrBG8 : t Js.t

  val brewer_BrBG9 : t Js.t

  val brewer_BrBG10 : t Js.t

  val brewer_BrBG11 : t Js.t

  val brewer_PRGn3 : t Js.t

  val brewer_PRGn4 : t Js.t

  val brewer_PRGn5 : t Js.t

  val brewer_PRGn6 : t Js.t

  val brewer_PRGn7 : t Js.t

  val brewer_PRGn8 : t Js.t

  val brewer_PRGn9 : t Js.t

  val brewer_PRGn10 : t Js.t

  val brewer_PRGn11 : t Js.t

  val brewer_PiYG3 : t Js.t

  val brewer_PiYG4 : t Js.t

  val brewer_PiYG5 : t Js.t

  val brewer_PiYG6 : t Js.t

  val brewer_PiYG7 : t Js.t

  val brewer_PiYG8 : t Js.t

  val brewer_PiYG9 : t Js.t

  val brewer_PiYG10 : t Js.t

  val brewer_PiYG11 : t Js.t

  val brewer_RdBu3 : t Js.t

  val brewer_RdBu4 : t Js.t

  val brewer_RdBu5 : t Js.t

  val brewer_RdBu6 : t Js.t

  val brewer_RdBu7 : t Js.t

  val brewer_RdBu8 : t Js.t

  val brewer_RdBu9 : t Js.t

  val brewer_RdBu10 : t Js.t

  val brewer_RdBu11 : t Js.t

  val brewer_RdGy3 : t Js.t

  val brewer_RdGy4 : t Js.t

  val brewer_RdGy5 : t Js.t

  val brewer_RdGy6 : t Js.t

  val brewer_RdGy7 : t Js.t

  val brewer_RdGy8 : t Js.t

  val brewer_RdGy9 : t Js.t

  val brewer_RdGy10 : t Js.t

  val brewer_RdGy11 : t Js.t

  val brewer_RdYlBu3 : t Js.t

  val brewer_RdYlBu4 : t Js.t

  val brewer_RdYlBu5 : t Js.t

  val brewer_RdYlBu6 : t Js.t

  val brewer_RdYlBu7 : t Js.t

  val brewer_RdYlBu8 : t Js.t

  val brewer_RdYlBu9 : t Js.t

  val brewer_RdYlBu10 : t Js.t

  val brewer_RdYlBu11 : t Js.t

  val brewer_Spectral3 : t Js.t

  val brewer_Spectral4 : t Js.t

  val brewer_Spectral5 : t Js.t

  val brewer_Spectral6 : t Js.t

  val brewer_Spectral7 : t Js.t

  val brewer_Spectral8 : t Js.t

  val brewer_Spectral9 : t Js.t

  val brewer_Spectral10 : t Js.t

  val brewer_Spectral11 : t Js.t

  val brewer_RdYlGn3 : t Js.t

  val brewer_RdYlGn4 : t Js.t

  val brewer_RdYlGn5 : t Js.t

  val brewer_RdYlGn6 : t Js.t

  val brewer_RdYlGn7 : t Js.t

  val brewer_RdYlGn8 : t Js.t

  val brewer_RdYlGn9 : t Js.t

  val brewer_RdYlGn10 : t Js.t

  val brewer_RdYlGn11 : t Js.t

  val brewer_Accent3 : t Js.t

  val brewer_Accent4 : t Js.t

  val brewer_Accent5 : t Js.t

  val brewer_Accent6 : t Js.t

  val brewer_Accent7 : t Js.t

  val brewer_Accent8 : t Js.t

  val brewer_DarkTwo3 : t Js.t

  val brewer_DarkTwo4 : t Js.t

  val brewer_DarkTwo5 : t Js.t

  val brewer_DarkTwo6 : t Js.t

  val brewer_DarkTwo7 : t Js.t

  val brewer_DarkTwo8 : t Js.t

  val brewer_Paired3 : t Js.t

  val brewer_Paired4 : t Js.t

  val brewer_Paired5 : t Js.t

  val brewer_Paired6 : t Js.t

  val brewer_Paired7 : t Js.t

  val brewer_Paired8 : t Js.t

  val brewer_Paired9 : t Js.t

  val brewer_Paired10 : t Js.t

  val brewer_Paired11 : t Js.t

  val brewer_Paired12 : t Js.t

  val brewer_PastelOne3 : t Js.t

  val brewer_PastelOne4 : t Js.t

  val brewer_PastelOne5 : t Js.t

  val brewer_PastelOne6 : t Js.t

  val brewer_PastelOne7 : t Js.t

  val brewer_PastelOne8 : t Js.t

  val brewer_PastelOne9 : t Js.t

  val brewer_PastelTwo3 : t Js.t

  val brewer_PastelTwo4 : t Js.t

  val brewer_PastelTwo5 : t Js.t

  val brewer_PastelTwo6 : t Js.t

  val brewer_PastelTwo7 : t Js.t

  val brewer_PastelTwo8 : t Js.t

  val brewer_SetOne3 : t Js.t

  val brewer_SetOne4 : t Js.t

  val brewer_SetOne5 : t Js.t

  val brewer_SetOne6 : t Js.t

  val brewer_SetOne7 : t Js.t

  val brewer_SetOne8 : t Js.t

  val brewer_SetOne9 : t Js.t

  val brewer_SetTwo3 : t Js.t

  val brewer_SetTwo4 : t Js.t

  val brewer_SetTwo5 : t Js.t

  val brewer_SetTwo6 : t Js.t

  val brewer_SetTwo7 : t Js.t

  val brewer_SetTwo8 : t Js.t

  val brewer_SetThree3 : t Js.t

  val brewer_SetThree4 : t Js.t

  val brewer_SetThree5 : t Js.t

  val brewer_SetThree6 : t Js.t

  val brewer_SetThree7 : t Js.t

  val brewer_SetThree8 : t Js.t

  val brewer_SetThree9 : t Js.t

  val brewer_SetThree10 : t Js.t

  val brewer_SetThree11 : t Js.t

  val brewer_SetThree12 : t Js.t

  val office_Adjacency6 : t Js.t

  val office_Advantage6 : t Js.t

  val office_Angles6 : t Js.t

  val office_Apex6 : t Js.t

  val office_Apothecary6 : t Js.t

  val office_Aspect6 : t Js.t

  val office_Atlas6 : t Js.t

  val office_Austin6 : t Js.t

  val office_Badge6 : t Js.t

  val office_Banded6 : t Js.t

  val office_Basis6 : t Js.t

  val office_Berlin6 : t Js.t

  val office_BlackTie6 : t Js.t

  val office_Blue6 : t Js.t

  val office_BlueGreen6 : t Js.t

  val office_BlueII6 : t Js.t

  val office_BlueRed6 : t Js.t

  val office_BlueWarm6 : t Js.t

  val office_Breeze6 : t Js.t

  val office_Capital6 : t Js.t

  val office_Celestial6 : t Js.t

  val office_Circuit6 : t Js.t

  val office_Civic6 : t Js.t

  val office_Clarity6 : t Js.t

  val office_Codex6 : t Js.t

  val office_Composite6 : t Js.t

  val office_Concourse6 : t Js.t

  val office_Couture6 : t Js.t

  val office_Crop6 : t Js.t

  val office_Damask6 : t Js.t

  val office_Depth6 : t Js.t

  val office_Dividend6 : t Js.t

  val office_Droplet6 : t Js.t

  val office_Elemental6 : t Js.t

  val office_Equity6 : t Js.t

  val office_Essential6 : t Js.t

  val office_Excel16 : t Js.t

  val office_Executive6 : t Js.t

  val office_Exhibit6 : t Js.t

  val office_Expo6 : t Js.t

  val office_Facet6 : t Js.t

  val office_Feathered6 : t Js.t

  val office_Flow6 : t Js.t

  val office_Focus6 : t Js.t

  val office_Folio6 : t Js.t

  val office_Formal6 : t Js.t

  val office_Forte6 : t Js.t

  val office_Foundry6 : t Js.t

  val office_Frame6 : t Js.t

  val office_Gallery6 : t Js.t

  val office_Genesis6 : t Js.t

  val office_Grayscale6 : t Js.t

  val office_Green6 : t Js.t

  val office_GreenYellow6 : t Js.t

  val office_Grid6 : t Js.t

  val office_Habitat6 : t Js.t

  val office_Hardcover6 : t Js.t

  val office_Headlines6 : t Js.t

  val office_Horizon6 : t Js.t

  val office_Infusion6 : t Js.t

  val office_Inkwell6 : t Js.t

  val office_Inspiration6 : t Js.t

  val office_Integral6 : t Js.t

  val office_Ion6 : t Js.t

  val office_IonBoardroom6 : t Js.t

  val office_Kilter6 : t Js.t

  val office_Madison6 : t Js.t

  val office_MainEvent6 : t Js.t

  val office_Marquee6 : t Js.t

  val office_Median6 : t Js.t

  val office_Mesh6 : t Js.t

  val office_Metail6 : t Js.t

  val office_Metro6 : t Js.t

  val office_Metropolitan6 : t Js.t

  val office_Module6 : t Js.t

  val office_NewsPrint6 : t Js.t

  val office_Office6 : t Js.t

  val office_OfficeClassic6 : t Js.t

  val office_Opulent6 : t Js.t

  val office_Orange6 : t Js.t

  val office_OrangeRed6 : t Js.t

  val office_Orbit6 : t Js.t

  val office_Organic6 : t Js.t

  val office_Oriel6 : t Js.t

  val office_Origin6 : t Js.t

  val office_Paper6 : t Js.t

  val office_Parallax6 : t Js.t

  val office_Parcel6 : t Js.t

  val office_Perception6 : t Js.t

  val office_Perspective6 : t Js.t

  val office_Pixel6 : t Js.t

  val office_Plaza6 : t Js.t

  val office_Precedent6 : t Js.t

  val office_Pushpin6 : t Js.t

  val office_Quotable6 : t Js.t

  val office_Red6 : t Js.t

  val office_RedOrange6 : t Js.t

  val office_RedViolet6 : t Js.t

  val office_Retrospect6 : t Js.t

  val office_Revolution6 : t Js.t

  val office_Saddle6 : t Js.t

  val office_Savon6 : t Js.t

  val office_Sketchbook6 : t Js.t

  val office_Sky6 : t Js.t

  val office_Slate6 : t Js.t

  val office_Slice6 : t Js.t

  val office_Slipstream6 : t Js.t

  val office_SOHO6 : t Js.t

  val office_Solstice6 : t Js.t

  val office_Spectrum6 : t Js.t

  val office_Story6 : t Js.t

  val office_Studio6 : t Js.t

  val office_Summer6 : t Js.t

  val office_Technic6 : t Js.t

  val office_Thatch6 : t Js.t

  val office_Tradition6 : t Js.t

  val office_Travelogue6 : t Js.t

  val office_Trek6 : t Js.t

  val office_Twilight6 : t Js.t

  val office_Urban6 : t Js.t

  val office_UrbanPop6 : t Js.t

  val office_VaporTrail6 : t Js.t

  val office_Venture6 : t Js.t

  val office_Verve6 : t Js.t

  val office_View6 : t Js.t

  val office_Violet6 : t Js.t

  val office_VioletII6 : t Js.t

  val office_Waveform6 : t Js.t

  val office_Wisp6 : t Js.t

  val office_WoodType6 : t Js.t

  val office_Yellow6 : t Js.t

  val office_YellowOrange6 : t Js.t

  val tableau_Tableau10 : t Js.t

  val tableau_Tableau20 : t Js.t

  val tableau_ColorBlind10 : t Js.t

  val tableau_SeattleGrays5 : t Js.t

  val tableau_Traffic9 : t Js.t

  val tableau_MillerStone11 : t Js.t

  val tableau_SuperfishelStone10 : t Js.t

  val tableau_NurielStone9 : t Js.t

  val tableau_JewelBright9 : t Js.t

  val tableau_Summer8 : t Js.t

  val tableau_Winter10 : t Js.t

  val tableau_GreenOrangeTeal12 : t Js.t

  val tableau_RedBlueBrown12 : t Js.t

  val tableau_PurplePinkGray12 : t Js.t

  val tableau_HueCircle19 : t Js.t

  val tableau_OrangeBlue7 : t Js.t

  val tableau_RedGreen7 : t Js.t

  val tableau_GreenBlue7 : t Js.t

  val tableau_RedBlue7 : t Js.t

  val tableau_RedBlack7 : t Js.t

  val tableau_GoldPurple7 : t Js.t

  val tableau_RedGreenGold7 : t Js.t

  val tableau_SunsetSunrise7 : t Js.t

  val tableau_OrangeBlueWhite7 : t Js.t

  val tableau_RedGreenWhite7 : t Js.t

  val tableau_GreenBlueWhite7 : t Js.t

  val tableau_RedBlueWhite7 : t Js.t

  val tableau_RedBlackWhite7 : t Js.t

  val tableau_OrangeBlueLight7 : t Js.t

  val tableau_Temperature7 : t Js.t

  val tableau_BlueGreen7 : t Js.t

  val tableau_BlueLight7 : t Js.t

  val tableau_OrangeLight7 : t Js.t

  val tableau_Blue20 : t Js.t

  val tableau_Orange20 : t Js.t

  val tableau_Green20 : t Js.t

  val tableau_Red20 : t Js.t

  val tableau_Purple20 : t Js.t

  val tableau_Brown20 : t Js.t

  val tableau_Gray20 : t Js.t

  val tableau_GrayWarm20 : t Js.t

  val tableau_BlueTeal20 : t Js.t

  val tableau_OrangeGold20 : t Js.t

  val tableau_GreenGold20 : t Js.t

  val tableau_RedGold21 : t Js.t

  val tableau_Classic10 : t Js.t

  val tableau_ClassicMedium10 : t Js.t

  val tableau_ClassicLight10 : t Js.t

  val tableau_Classic20 : t Js.t

  val tableau_ClassicGray5 : t Js.t

  val tableau_ClassicColorBlind10 : t Js.t

  val tableau_ClassicTrafficLight9 : t Js.t

  val tableau_ClassicPurpleGray6 : t Js.t

  val tableau_ClassicPurpleGray12 : t Js.t

  val tableau_ClassicGreenOrange6 : t Js.t

  val tableau_ClassicGreenOrange12 : t Js.t

  val tableau_ClassicBlueRed6 : t Js.t

  val tableau_ClassicBlueRed12 : t Js.t

  val tableau_ClassicCyclic13 : t Js.t

  val tableau_ClassicGreen7 : t Js.t

  val tableau_ClassicGray13 : t Js.t

  val tableau_ClassicBlue7 : t Js.t

  val tableau_ClassicRed9 : t Js.t

  val tableau_ClassicOrange7 : t Js.t

  val tableau_ClassicAreaRed11 : t Js.t

  val tableau_ClassicAreaGreen11 : t Js.t

  val tableau_ClassicAreaBrown11 : t Js.t

  val tableau_ClassicRedGreen11 : t Js.t

  val tableau_ClassicRedBlue11 : t Js.t

  val tableau_ClassicRedBlack11 : t Js.t

  val tableau_ClassicAreaRedGreen21 : t Js.t

  val tableau_ClassicOrangeBlue13 : t Js.t

  val tableau_ClassicGreenBlue11 : t Js.t

  val tableau_ClassicRedWhiteGreen11 : t Js.t

  val tableau_ClassicRedWhiteBlack11 : t Js.t

  val tableau_ClassicOrangeWhiteBlue11 : t Js.t

  val tableau_ClassicRedWhiteBlackLight10 : t Js.t

  val tableau_ClassicOrangeWhiteBlueLight11 : t Js.t

  val tableau_ClassicRedWhiteGreenLight11 : t Js.t

  val tableau_ClassicRedGreenLight11 : t Js.t

  val make : string -> t Js.t
end

class type colorschemes =
  object
    (** The transparency value for the line fill color.
      Must be a number between [0.0] (fully transparent)
      and [1.0] (no transparency). *)
    method fillAlpha : float Js.t Js.prop

    (** Color scheme name.
      It also accepts an array of color strings,
      which is primarily for ES modules. *)
    method scheme : Scheme.t Js.t Indexable.t Js.t Js.prop

    (** If set to [true], the order of the colors in the selected scheme is reversed. *)
    method reverse : bool Js.t Js.prop

    (** If set to [true], the specified color scheme will override
      the existing color options.
      If [false], it is only applied when no color setting exists. *)
    method override : bool Js.t Js.prop

    (** A function that takes a copy of the color string array for scheme
      in order to extend the predefined scheme colors. *)
    method custom :
      (Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.js_array Js.t)
      Js.callback
      Js.optdef
      Js.prop
  end

val empty_colorschemes_config : unit -> colorschemes Js.t

val of_chart_options : #chartOptions Js.t -> colorschemes Js.t Js.optdef

val of_global : unit -> colorschemes Js.t Js.optdef

val set_to_chart_options : #chartOptions Js.t -> colorschemes Js.t -> unit

val set_globally : colorschemes Js.t -> unit
