
let french_input =
  "Cette princesse était belle, quoiqu’elle eût passé la première \
   jeunesse ; elle aimait la grandeur, la magnificence et les \
   plaisirs. Le roi l’avait épousée lorsqu’il était encore duc \
   d’Orléans, et qu’il avait pour aîné le dauphin, qui mourut à \
   Tournon, prince que sa naissance et ses grandes qualités \
   destinaient à remplir dignement la place du roi François premier, \
   son père."

let french_expected =
  "Cette princesse etait belle, quoiqu'elle eut passe la premiere \
   jeunesse ; elle aimait la grandeur, la magnificence et les \
   plaisirs. Le roi l'avait epousee lorsqu'il etait encore duc \
   d'Orleans, et qu'il avait pour aine le dauphin, qui mourut a \
   Tournon, prince que sa naissance et ses grandes qualites \
   destinaient a remplir dignement la place du roi Francois premier, \
   son pere."

let vietnamese_input =
  "Vũ Ngọc Phan (1902-1987) là nhà văn, nhà nghiên cứu văn học hiện \
   đại và văn học dân gian Việt Nam. Trong những năm đầu cầm bút, ông \
   còn có bút danh là Chỉ Qua Thị."

let vietnamese_expected =
  "Vu Ngoc Phan (1902-1987) la nha van, nha nghien cuu van hoc hien \
   dai va van hoc dan gian Viet Nam. Trong nhung nam dau cam but, ong \
   con co but danh la Chi Qua Thi."

let russian_input =
  "Оросой холбоото улас (ородоор Росси́йская Федера́ция), тобшолбол \
   Росси гү, али Орос Улас (ородоор Росси́я) — болбол Евразиин хойто \
   хэһэгээр үргэлжэлхэ улас юм. Орос хахад-юрэнхылэгшын засаглалтай \
   бүгэдэ найрамдаха улас холбооной 83 нютаг можоһоо бүрилдэнэ. Орос \
   Улас зүүн тиишэ Норвеги, Финланд, Эстони, Латви, Литва, Польшо, \
   Беларусь, Украина, Гүржи, Азербайджан, Казахстан, Хитад, Монгол, \
   Хойто Солонгос гэһэн арбан дүрбэн гүрэнүүдтэ хилэ зурыдаг, мүн уһаар \
   АНУ-ай Аляска можотой болон Япон уластай хилэ нэгэтэй."

let russian_expected =
  "Orosoi kholbooto oulas (orodoor Rossiiskaya Federatsiya), \
   tobcholbol Rossi gu, ali Oros OUlas (orodoor Rossiya) -- bolbol \
   Evraziin khoito kheһegeer urgeljelkhe oulas yum. Oros \
   khakhad-yurenkhylegchyn zasaglaltai bugede nairamdakha oulas \
   kholboonoi 83 nyutag mojoһoo burildene. Oros OUlas zuun tiiche \
   Norvegi, Finland, Estoni, Latvi, Litva, Pol'cho, Belarous', OUkraina, \
   Gurji, Azerbaidjan, Kazakhstan, KHitad, Mongol, KHoito Solongos geһen \
   arban durben gurenuudte khile zourydag, mun ouһaar ANOU-ai Alyaska \
   mojotoi bolon YApon oulastai khile negetei. "
