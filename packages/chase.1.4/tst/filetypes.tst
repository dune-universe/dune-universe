% chase version 1.4
% bound = 50, limit = 500, input_order = false
% ********
% file(F) => link(F) | regular(F). % (0)
% link(L) => target_of(L) = F & file(F). % (1)
% file(F). % (2)
% ********

(3,1){0}![file(f), regular(f)]

(6,4){0}![file(f), file(f_0), link(f), regular(f_0), target_of(f) = f_0]

(9,7){0}![file(f), file(f_0), file(f_1), link(f), link(f_0), regular(f_1),
  target_of(f) = f_0, target_of(f_0) = f_1]

(12,10){0}![file(f), file(f_0), file(f_1), file(f_2), link(f), link(f_0),
  link(f_1), regular(f_2), target_of(f) = f_0, target_of(f_0) = f_1,
  target_of(f_1) = f_2]

(15,13){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), link(f),
  link(f_0), link(f_1), link(f_2), regular(f_3), target_of(f) = f_0,
  target_of(f_0) = f_1, target_of(f_1) = f_2, target_of(f_2) = f_3]

(18,16){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  link(f), link(f_0), link(f_1), link(f_2), link(f_3), regular(f_4),
  target_of(f) = f_0, target_of(f_0) = f_1, target_of(f_1) = f_2,
  target_of(f_2) = f_3, target_of(f_3) = f_4]

(21,19){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), link(f), link(f_0), link(f_1), link(f_2), link(f_3), link(f_4),
  regular(f_5), target_of(f) = f_0, target_of(f_0) = f_1,
  target_of(f_1) = f_2, target_of(f_2) = f_3, target_of(f_3) = f_4,
  target_of(f_4) = f_5]

(24,22){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), link(f), link(f_0), link(f_1), link(f_2), link(f_3),
  link(f_4), link(f_5), regular(f_6), target_of(f) = f_0,
  target_of(f_0) = f_1, target_of(f_1) = f_2, target_of(f_2) = f_3,
  target_of(f_3) = f_4, target_of(f_4) = f_5, target_of(f_5) = f_6]

(27,25){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), link(f), link(f_0), link(f_1), link(f_2),
  link(f_3), link(f_4), link(f_5), link(f_6), regular(f_7),
  target_of(f) = f_0, target_of(f_0) = f_1, target_of(f_1) = f_2,
  target_of(f_2) = f_3, target_of(f_3) = f_4, target_of(f_4) = f_5,
  target_of(f_5) = f_6, target_of(f_6) = f_7]

(30,28){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), link(f), link(f_0), link(f_1),
  link(f_2), link(f_3), link(f_4), link(f_5), link(f_6), link(f_7),
  regular(f_8), target_of(f) = f_0, target_of(f_0) = f_1,
  target_of(f_1) = f_2, target_of(f_2) = f_3, target_of(f_3) = f_4,
  target_of(f_4) = f_5, target_of(f_5) = f_6, target_of(f_6) = f_7,
  target_of(f_7) = f_8]

(33,31){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), link(f), link(f_0),
  link(f_1), link(f_2), link(f_3), link(f_4), link(f_5), link(f_6),
  link(f_7), link(f_8), regular(f_9), target_of(f) = f_0,
  target_of(f_0) = f_1, target_of(f_1) = f_2, target_of(f_2) = f_3,
  target_of(f_3) = f_4, target_of(f_4) = f_5, target_of(f_5) = f_6,
  target_of(f_6) = f_7, target_of(f_7) = f_8, target_of(f_8) = f_9]

(36,34){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10), link(f),
  link(f_0), link(f_1), link(f_2), link(f_3), link(f_4), link(f_5),
  link(f_6), link(f_7), link(f_8), link(f_9), regular(f_10),
  target_of(f) = f_0, target_of(f_0) = f_1, target_of(f_1) = f_2,
  target_of(f_2) = f_3, target_of(f_3) = f_4, target_of(f_4) = f_5,
  target_of(f_5) = f_6, target_of(f_6) = f_7, target_of(f_7) = f_8,
  target_of(f_8) = f_9, target_of(f_9) = f_10]

(39,37){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10),
  file(f_11), link(f), link(f_0), link(f_1), link(f_2), link(f_3), link(f_4),
  link(f_5), link(f_6), link(f_7), link(f_8), link(f_9), link(f_10),
  regular(f_11), target_of(f) = f_0, target_of(f_0) = f_1,
  target_of(f_1) = f_2, target_of(f_2) = f_3, target_of(f_3) = f_4,
  target_of(f_4) = f_5, target_of(f_5) = f_6, target_of(f_6) = f_7,
  target_of(f_7) = f_8, target_of(f_8) = f_9, target_of(f_9) = f_10,
  target_of(f_10) = f_11]

(42,40){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10),
  file(f_11), file(f_12), link(f), link(f_0), link(f_1), link(f_2),
  link(f_3), link(f_4), link(f_5), link(f_6), link(f_7), link(f_8),
  link(f_9), link(f_10), link(f_11), regular(f_12), target_of(f) = f_0,
  target_of(f_0) = f_1, target_of(f_1) = f_2, target_of(f_2) = f_3,
  target_of(f_3) = f_4, target_of(f_4) = f_5, target_of(f_5) = f_6,
  target_of(f_6) = f_7, target_of(f_7) = f_8, target_of(f_8) = f_9,
  target_of(f_9) = f_10, target_of(f_10) = f_11, target_of(f_11) = f_12]

(45,43){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10),
  file(f_11), file(f_12), file(f_13), link(f), link(f_0), link(f_1),
  link(f_2), link(f_3), link(f_4), link(f_5), link(f_6), link(f_7),
  link(f_8), link(f_9), link(f_10), link(f_11), link(f_12), regular(f_13),
  target_of(f) = f_0, target_of(f_0) = f_1, target_of(f_1) = f_2,
  target_of(f_2) = f_3, target_of(f_3) = f_4, target_of(f_4) = f_5,
  target_of(f_5) = f_6, target_of(f_6) = f_7, target_of(f_7) = f_8,
  target_of(f_8) = f_9, target_of(f_9) = f_10, target_of(f_10) = f_11,
  target_of(f_11) = f_12, target_of(f_12) = f_13]

(48,46){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10),
  file(f_11), file(f_12), file(f_13), file(f_14), link(f), link(f_0),
  link(f_1), link(f_2), link(f_3), link(f_4), link(f_5), link(f_6),
  link(f_7), link(f_8), link(f_9), link(f_10), link(f_11), link(f_12),
  link(f_13), regular(f_14), target_of(f) = f_0, target_of(f_0) = f_1,
  target_of(f_1) = f_2, target_of(f_2) = f_3, target_of(f_3) = f_4,
  target_of(f_4) = f_5, target_of(f_5) = f_6, target_of(f_6) = f_7,
  target_of(f_7) = f_8, target_of(f_8) = f_9, target_of(f_9) = f_10,
  target_of(f_10) = f_11, target_of(f_11) = f_12, target_of(f_12) = f_13,
  target_of(f_13) = f_14]

(51,49){0}![file(f), file(f_0), file(f_1), file(f_2), file(f_3), file(f_4),
  file(f_5), file(f_6), file(f_7), file(f_8), file(f_9), file(f_10),
  file(f_11), file(f_12), file(f_13), file(f_14), file(f_15), link(f),
  link(f_0), link(f_1), link(f_2), link(f_3), link(f_4), link(f_5),
  link(f_6), link(f_7), link(f_8), link(f_9), link(f_10), link(f_11),
  link(f_12), link(f_13), link(f_14), regular(f_15), target_of(f) = f_0,
  target_of(f_0) = f_1, target_of(f_1) = f_2, target_of(f_2) = f_3,
  target_of(f_3) = f_4, target_of(f_4) = f_5, target_of(f_5) = f_6,
  target_of(f_6) = f_7, target_of(f_7) = f_8, target_of(f_8) = f_9,
  target_of(f_9) = f_10, target_of(f_10) = f_11, target_of(f_11) = f_12,
  target_of(f_12) = f_13, target_of(f_13) = f_14, target_of(f_14) = f_15]
